# QuantWealthR code review findings

Scope: all files in `R/` (`app.R` excluded, per your instruction). No fixes have been made yet — this is the findings report before any action. Each item names the file, the mechanism, and why it matters. I've ordered them by severity: high first (silently wrong results or crashes), then medium (real but narrower or situational), then low (style/efficiency, no behavioural impact).

## High severity

### 1. `calculate_prestige()` silently overwrites the original grave-good count columns (`prestige.R`, line 389)

The final assembly step runs `for (col in count_cols) final[[col]] <- out[[col]]`. `out` is the result of `apply_prestige_mean()`/`apply_prestige_bayesian()`, where each count column has already been replaced by that type's prestige weight (e.g. a "sword" column holding `3` becomes `4.52`, the Bayesian estimate for swords, with the original count of 3 discarded entirely). Because this is written back into `final` under the *same column name*, the data frame `calculate_prestige()` returns has its original artefact-count columns replaced by weight values, not preserved alongside them. The function's own `@return` doc says "the original data frame plus columns `raw_TOT`, `prestige_total`, `combined_total`..." — wording that implies the original columns survive unchanged. They don't. Anyone inspecting `result$sword` after calling `calculate_prestige()` expecting a raw count will silently get a prestige weight instead, with no error or warning. I checked whether anything downstream in the package relies on this overwritten value — it doesn't (plotting/inequality functions only touch `raw_TOT`, `prestige_total`, `combined_total`) — which suggests this is an unintended side effect rather than deliberate design. Recommended fix: don't overwrite `final[[col]]`; either drop that loop entirely (since the weighted columns aren't used anywhere) or write the weights to differently-named columns if you want to expose them.

### 2. `calculate_prestige()` can crash outright when there are zero "adult" graves (`prestige.R`, lines 301–320)

`tot_adults <- get_TOT_range(df_adults)` is called with no NULL/empty check. If `is_adult_grave` is `FALSE` for every row — which happens whenever every grave in the dataset has `adult_count == 0` and `subadult_count >= 1`, i.e. an all-subadult/all-infant sample — `get_TOT_range()` returns `NULL`. That `NULL` is then passed straight into `bayesian_prestige(tot_adults)` (or `get_TOT_means(tot_adults)` for the mean method), both of which pipe it into `dplyr::group_by()`, which errors on `NULL` input. This is not a contrived edge case for your field: a cemetery sample restricted to child/infant burials is a normal thing to analyze. By contrast, the parallel code path for subadults (`tot_subs`, lines 309–314) *does* have explicit `if (nrow(...) > 0)` guards — so the two halves of this function are inconsistently defensive, and the asymmetry is exactly where the gap is. Note: if you call `calculate_prestige()` indirectly through `qw_pipeline()`, this crash is caught by that function's `tryCatch()` and just produces a skipped-group warning rather than stopping your session — but calling `calculate_prestige()` directly (as the package's own examples show) will stop with an unhelpful dplyr error. Recommended fix: add the same NULL/empty guard used for `tot_subs` to the `tot_adults` path, and decide what should happen in that case (skip age separation? error with a clear message?).

### 3. `simulate_category_bias(df, graves = "adults")` silently includes subadult-only graves if `adult_count` is missing (`bias_simulation.R`, lines 84–96)

When `has_adult` is `FALSE` (no `adult_count` column at all, but `subadult_count` is present), the code sets `ac <- rep(1L, nrow(df))` — i.e. it assumes every single grave has exactly one adult. Combined with `is_adult_grave <- ac >= 1 | (...)`, this makes `is_adult_grave` `TRUE` for every row, regardless of `subadult_count`. So requesting `graves = "adults"` on a dataset that only records `subadult_count` doesn't filter out subadult-only graves at all — it silently returns everything, unfiltered, with no warning. This is a correctness bug, not just a missing-data inconvenience: the function name and the `graves` parameter imply a real filter is being applied, and in this configuration it quietly does nothing. Recommended fix: if `adult_count` is absent, either warn explicitly that "adults" filtering can't be applied and fall back to `"all"`, or stop with a clear message — but don't fabricate `ac = 1` for every row.

## Medium severity

### 4. `identify_count_columns()`'s exclusion list doesn't actually catch the real column name (`utils.R`, line 46)

The hardcoded `excluded` vector includes the literal string `"tot"`, presumably meant to exclude a derived total column from being treated as a count column. But the actual column produced by `calculate_prestige()` is named `raw_TOT`, which lowercases to `"raw_tot"` — not `"tot"`. The entries `"prestige_total"` and `"combined_total"` in the same list do match correctly (exact match), so only this one entry is off. The mismatch is currently masked because the two call sites inside `plot_prestige()` (`plots_ggplot.R`, ~lines 40–41 and 66–67) defensively re-filter with `setdiff(count_cols, c("raw_TOT", ...))` right after calling `identify_count_columns()` — i.e. the calling code has already learned to route around this bug. Other callers (`bias_simulation.R`, `pipeline.R`) only ever call it on data that hasn't been through `calculate_prestige()` yet, so they're not currently affected. Still worth fixing at the source (`"tot"` → `"raw_tot"`) so the defensive workarounds elsewhere become unnecessary and any future caller doesn't inherit the same trap.

### 5. `apply_prestige_mean()` / `apply_prestige_bayesian()`: blanket `out[is.na(out)] <- 0` touches the ID column too (`prestige.R`, lines 174, 196)

This line zeroes out *any* `NA` anywhere in `out`, including column 1 (the grave/unit ID), not just the count columns. If a dataset has a missing ID for some row (blank cell, malformed CSV row, etc.), that row's ID would silently become the numeral `0` rather than staying missing or raising an error — which could cause two rows to collide under the same ID. Low likelihood in clean data, but silent and hard to trace if it does happen. Safer to restrict the substitution to `out[, -1]`.

### 6. `read_qw_data()`'s zero→NA conversion applies to *every* numeric column, including ones where zero is meaningful (`utils.R`, lines 90–98)

The function's stated purpose — converting `0` to `NA` so that "no recorded swords" doesn't get treated differently from "missing data" — makes sense for grave-good count columns. But the loop runs over every column from 2 onward indiscriminately, which also includes `adult_count` / `subadult_count` (where `0` is a real, informative value — "this grave has zero adults," i.e. it's a child/subadult-only grave, not missing data) and `lat`/`lng` (where `0` could be a genuine coordinate near the equator/prime meridian). I traced the `adult_count`/`subadult_count` case further: `calculate_prestige()` re-converts any `NA` back to `0` immediately (`adult_counts[is.na(adult_counts)] <- 0`), so the round-trip through `read_qw_data()` is probably a harmless no-op for those two columns specifically — a real `0` becomes `NA` then becomes `0` again. I did not verify this is lossless for every call path, and the `lat`/`lng` case has no such round-trip safety net, so a genuine zero coordinate would stay corrupted to `NA`. This is more of a design question than an unambiguous bug — worth deciding deliberately (e.g. exclude `adult_count`, `subadult_count`, `lat`, `lng` from the zero→NA pass) rather than leaving it as an accidental side effect of looping over "every column but the first."

### 7. `bayesian_prestige()`'s zero-variance floor can produce overconfident credible intervals for small samples (`prestige.R`, lines 108–109)

When a type has `n >= 2` occurrences and those values happen to be identical (`var(y) == 0`), the code substitutes `V <- 0.1` to avoid a divide-by-zero in the method-of-moments prior. The posterior *mean* turns out to be exactly right regardless of this substitution (it always converges to the shared value when variance is truly zero), but the substituted `V` controls how wide the resulting credible interval is — and an artificial `0.1` can make the interval much narrower than is statistically justified from just 2–3 coincidentally-matching observations. This isn't necessarily wrong (you have to pick *something* when variance is zero), but it is a bespoke heuristic with no documented rationale, and it's the kind of thing that can make sparse types look more precisely estimated than they really are. Worth a second look from a statistical-design standpoint rather than treating it as settled.

### 8. The "unknown age" fallback in age separation isn't documented as a default assumption (`prestige.R`, line 297; duplicated in `bias_simulation.R`, line 98)

`is_adult_grave <- adult_counts >= 1 | (adult_counts == 0 & subadult_counts == 0)` means a grave with *no* age information recorded at all (`adult_count == 0` and `subadult_count == 0`) is classified as an adult grave by default, and is divided by `pmax(adult_counts, 1) = 1` for per-capita purposes (i.e. treated as exactly one adult). That's a real, consequential modelling choice — graves with unknown age silently default to "single adult" rather than being excluded or flagged — and it isn't mentioned in `calculate_prestige()`'s documentation, which only describes what happens when age columns are present or absent entirely, not this partial/unknown case. Separately, this exact line of logic is copy-pasted verbatim into `bias_simulation.R` rather than factored into one shared helper — meaning if the convention is ever revisited, it has to be changed in two places, and it would be easy to update one and miss the other.

### 9. `plot_gini_comparison()` reuses the same destructive-clipping pattern already fixed elsewhere (`comparison.R`, line 135)

`scale_x_continuous(limits = c(0, NA))` is the same construct that caused the "Removed N rows" warning we fixed earlier in `plot_prestige()` — it deletes out-of-range data rather than just changing the visible window. Here it's applied to Gini values and their bootstrap CI bounds. `calculate_gini()` uses `type = "basic"` bootstrap CIs, which are not constrained to stay within Gini's theoretical [0, 1] range — a `lower` bound can come out negative for small or low-inequality samples. If that happens, the CI whisker (or even the point) would be silently truncated/dropped here, the same bug class as before, just not yet fixed in this second location.

### 10. `plot_bias()`'s `geom_jitter()` only sets `height`, leaving `width` at ggplot2's default (`plots_ggplot.R`, line 527, combined with line 540)

The x-axis here is the simulated Gini coefficient, hard-limited via `scale_x_continuous(limits = c(0, 1))` — destructive limits again. `geom_jitter(height = 0.4, alpha = 0.5, size = 1.5)` doesn't set `width`, so ggplot2 falls back to its automatic default jitter width on the x-axis. Any Gini value near 0 or 1 (or, depending on the resolution ggplot2 infers, even mid-range values) can get jittered outside `[0, 1]` and be silently dropped, the same way the earlier `plot_prestige()` bug worked. This is the third occurrence of the same underlying bug class (destructive `limits=` + jitter) in the package.

### 11. `lorenz_data()` and `decile_data()` have no graves filter, unlike `plot_density()` (`inequality.R`, lines 150–151 and 188–189)

`plot_density()` exposes a `graves = c("all", "adults", "subadults")` parameter so you can look at any subset. `lorenz_data()` and `decile_data()` hard-code `df <- prestige_df[prestige_df$is_adult_grave, ]` with no equivalent option — there's no way to get a Lorenz curve or decile breakdown for subadult-only graves, even though `inequality_summary()` happily computes a subadult Gini. This is an inconsistency in what the package lets you do with the same underlying data, not a correctness bug, but it's a real gap if you ever want to visualize the subadult distribution rather than just see its Gini number.

## Low severity (style / efficiency, no behavioural impact)

### 12. `format_dates()` is more complex than it needs to be (`utils.R`, lines 18–27)

The inner `fmt()` helper already handles both BCE and CE single-year formatting correctly, so the three-branch `if/else if/else` could collapse to one line (`paste0(fmt(start_year), " - ", fmt(end_year))` with a same-suffix shortcut). Not wrong, just more code than the logic requires. Also worth noting: there's no check for `start_year > end_year` (a reversed range would format without complaint).

### 13. `.auto_detect_sep()`'s tie-breaking picks the first separator on a tie (`utils.R`, line 113)

`which.max()` returns only the first index on a tie. If a file uses commas and semicolons in genuinely equal numbers across the first 5 lines, the "wrong" one could be picked. Low real-world likelihood.

### 14. `calculate_prestige()` recomputes `plot_means` instead of reusing `means_df` (`prestige.R`, lines 359–364)

In the age-separated, `method == "mean"` branch, `means_df` is already computed at lines 323–326. The `plot_means` attribute then redoes the identical computation from scratch a few lines later rather than just assigning `means_df`. Harmless (deterministic, same result both times) but wasteful, and a latent risk if the two copies are ever edited independently. The parallel `plot_bayes` attribute, just above it, correctly reuses the already-computed `bayes_df` instead of recomputing — so the inconsistency is local and easy to fix by matching that pattern.

### 15. `simulate_category_bias(df, graves = "subadults")` gives a misleading error when `subadult_count` is absent (`bias_simulation.R`, lines 93–104, 120)

If `adult_count` exists but `subadult_count` doesn't, requesting `graves = "subadults"` produces `sc <- rep(0L, ...)`, making `is_subadult_only` `FALSE` for every row, so `df` becomes a zero-row data frame after filtering. That then surfaces as `stop("No usable count columns found.")` — a confusing message that has nothing to do with the actual problem (there's no `subadult_count` column to filter on). Not a silent-wrong-answer bug like finding #3, since it does stop with an error rather than proceeding — but the error message points the user in the wrong direction.

### 16. `plot_gini_comparison()` parses the same CI string format two different ways (`comparison.R`, lines 52 and 81)

Both the physical-measure branch and the count-data branch parse a CI string of the identical format (`"[X–Y]"`, built by the same `paste0(...)` pattern in `pipeline.R`), but with two different regexes (`[0-9.]+.+[0-9.]+` vs. `[0-9.]+[^0-9.]+[0-9.]+`). Both seem to work correctly for the actual format produced, but having two separate, subtly different patterns for one parsing task is a maintenance smell — if the format ever changes, it's easy to update one and forget the other.

## What I didn't deep-dive

I read every file in `R/` and grepped the whole of `plots_ggplot.R` for the two bug patterns we already know about (destructive `scale_*_continuous(limits=)`, and `stat_density()`/`geom_jitter()` interactions), but I did not give `plot_lorenz()`, `plot_decile()`, and `plot_bias_wealth_lorenz()`/`plot_physical_lorenz()` the same line-by-line scrutiny as `prestige.R` and `bias_simulation.R`. If you want, I can do a second, narrower pass focused just on those once we've settled what to do with the findings above.

## Suggested next step

Let me know which of these you want fixed now versus deferred or left as-is (some, like #6 and #7, are design questions rather than bugs with an obvious correct answer). I'd suggest prioritizing #1–#3 first since those affect correctness of returned values, not just plot appearance.
