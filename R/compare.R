# ============================================================================
# Comparing cases across data sources: combined uncertainty and multiverse
# ============================================================================

# ---- Shared helpers --------------------------------------------------------

#' Inequality statistics for one vector of per-grave wealth
#'
#' Returns a named numeric vector. Names: "gini", "top_share",
#' "bottom_share", "atkinson_<eps>", "mean_median_ratio" (only those asked
#' for in `stats`).
#' @noRd
.case_stats <- function(w, stats, top_pct = 10, bottom_pct = 50, atkinson_eps = 2) {
  w <- w[!is.na(w)]
  out <- c()
  if (length(w) < 2 || !isTRUE(sum(w) > 0)) {
    nm <- .stat_names(stats, atkinson_eps)
    return(stats::setNames(rep(NA_real_, length(nm)), nm))
  }
  if ("gini" %in% stats)
    out["gini"] <- DescTools::Gini(w, unbiased = TRUE)
  if ("top_share" %in% stats)
    out["top_share"] <- .top_share(w, top_pct)
  if ("bottom_share" %in% stats)
    out["bottom_share"] <- .bottom_share(w, bottom_pct)
  if ("atkinson" %in% stats)
    for (e in atkinson_eps) out[paste0("atkinson_", e)] <- .atkinson(w, e)
  if ("mean_median_ratio" %in% stats) {
    m <- stats::median(w)
    out["mean_median_ratio"] <- if (m > 0) mean(w) / m else NA_real_
  }
  out
}

#' @noRd
.stat_names <- function(stats, atkinson_eps) {
  nm <- c()
  for (s in stats) nm <- c(nm, if (s == "atkinson") paste0("atkinson_", atkinson_eps) else s)
  nm
}

#' Readable labels for the statistic names
#' @noRd
.stat_labels <- function(nm, top_pct = 10, bottom_pct = 50) {
  vapply(nm, function(s) {
    if (s == "gini") "Gini"
    else if (s == "top_share") sprintf("Top %g%% share", top_pct)
    else if (s == "bottom_share") sprintf("Bottom %g%% share", bottom_pct)
    else if (s == "mean_median_ratio") "Mean / median"
    else if (startsWith(s, "atkinson_"))
      sprintf("Atkinson (%s=%s)", .sym("epsilon"), sub("atkinson_", "", s))
    else s
  }, character(1), USE.NAMES = FALSE)
}

#' For every statistic: does a larger value mean MORE inequality?
#' (All do except the bottom share, where a larger share = more equal.)
#' @noRd
.higher_is_more_unequal <- function(nm) nm != "bottom_share"

#' Per-grave wealth of one group (observed), for the chosen grave subset
#' @noRd
.group_wealth <- function(g, graves = "adults", measure = "combined") {
  pd <- g$prestige_df
  if (is.null(pd)) return(NULL)
  col <- switch(measure, combined = "combined_total", prestige = "prestige_total",
                tot = "raw_TOT")
  keep <- switch(graves,
    adults    = if ("is_adult_grave" %in% names(pd)) pd$is_adult_grave else rep(TRUE, nrow(pd)),
    subadults = if ("is_subadult_only" %in% names(pd)) pd$is_subadult_only else rep(FALSE, nrow(pd)),
    all       = rep(TRUE, nrow(pd)))
  v <- pd[[col]][keep %in% TRUE]
  if (measure == "tot") v <- v + 1
  v[!is.na(v)]
}


# ---- compare_cases() -------------------------------------------------------

#' Compare two cases with all sources of uncertainty combined
#'
#' Answers "is case A more unequal than case B?" by building a distribution
#' of the difference A - B for several inequality statistics. Each draw
#' combines up to three sources of uncertainty:
#'
#' 1. **Recording / categorisation** (if `use_bias = TRUE` and the pipeline
#'    was run with `run_bias_sim = TRUE`): a randomly chosen split or lump
#'    simulation's per-grave wealth is used instead of the observed wealth.
#' 2. **Sampling**: the graves are resampled with replacement (bootstrap).
#' 3. **Phase imputation** (if `pipeline_result` is a list of pipeline
#'    results, e.g. one per imputed dataset): each draw uses a randomly
#'    chosen result.
#'
#' A and B are drawn independently, so cases from different datasets (with
#' their own bias simulations) can be compared. The result says, for each
#' statistic, how often A came out more unequal than B, e.g. "in 87% of
#' draws". Note the direction for the bottom share: a *larger* bottom share
#' means *more equal*, so A counts as more unequal when its bottom share is
#' smaller.
#'
#' What this cannot include: differences between sources that are not
#' categorisation (e.g. graves without goods missing from one dataset, or
#' construction features recorded in only one). Use [multiverse_compare()]
#' for those.
#'
#' @param pipeline_result A [count_pipeline()] result containing both cases,
#'   or a list of such results (e.g. one per phase imputation).
#' @param case_a,case_b Case names, i.e. keys of `$by_group`, e.g.
#'   `"SK: 2480-2300"` and `"GAC: 2900-2680"`.
#' @param stats Any of `"gini"`, `"top_share"`, `"bottom_share"`,
#'   `"atkinson"`, `"mean_median_ratio"`.
#' @param graves `"adults"` (default), `"all"` or `"subadults"`. The bias
#'   simulations only exist for the subset they were run on (adults when age
#'   separation was used); for another subset only sampling (and imputation)
#'   uncertainty is included, with a message.
#' @param measure `"combined"` (default), `"prestige"` or `"tot"`.
#' @param n_draws Number of draws (default 1000).
#' @param use_bias Logical. Include the bias simulations (default TRUE).
#' @param top_pct,bottom_pct,atkinson_eps As in [plot_gini_comparison()].
#'   Default `atkinson_eps = 2` (bottom-sensitive).
#' @param seed Optional random seed, for reproducible results.
#' @return An object of class `"qw_compare"`: a list with `summary` (one row
#'   per statistic: observed values, observed difference, median and 95%
#'   interval of the difference, `p_a_more_unequal`), `draws` (all draws)
#'   and the settings. Plot it with [plot_compare_cases()].
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' cc <- compare_cases(res, "A: A2", "B: B2", n_draws = 200, seed = 1)
#' cc               # how often A2 is more unequal than B2, per statistic
#' plot_compare_cases(cc)
#'
#' \donttest{
#' # Including phase-imputation uncertainty: one pipeline per imputed copy
#' imps <- impute_phases(ph[ph$culture == "A", ], phase_col = "phase",
#'                       unit_col = "Grave_ID", event_pattern = NULL,
#'                       n_imp = 5, seed = 1, verbose = FALSE)
#' res_list <- lapply(imps, function(a)
#'   count_pipeline(list(A = a, B = ph[ph$culture == "B", ]), group_col = "phase",
#'                  id_col = "Grave_ID", date_col = "date_BCE", run_bias_sim = TRUE, n_bias_sims = 20,
#'                  progress = FALSE))
#' compare_cases(res_list, "A: A2", "B: B2", n_draws = 200, seed = 1)
#' }
#' @export
compare_cases <- function(pipeline_result, case_a, case_b,
                          stats   = c("gini", "top_share", "bottom_share", "atkinson"),
                          graves  = c("adults", "all", "subadults"),
                          measure = c("combined", "prestige", "tot"),
                          n_draws = 1000,
                          use_bias = TRUE,
                          top_pct = 10, bottom_pct = 50, atkinson_eps = 2,
                          seed = NULL) {
  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  stats   <- match.arg(stats, c("gini", "top_share", "bottom_share", "atkinson",
                                "mean_median_ratio"), several.ok = TRUE)
  results <- if (inherits(pipeline_result, "qw_pipeline")) list(pipeline_result) else pipeline_result
  if (!all(vapply(results, inherits, logical(1), "qw_pipeline")))
    stop("pipeline_result must be a count_pipeline() result or a list of them.")
  for (cs in c(case_a, case_b)) {
    ok <- vapply(results, function(r) cs %in% names(r$by_group), logical(1))
    if (!all(ok))
      stop(sprintf("Case '%s' not found in %s. Available: %s", cs,
                   if (length(results) > 1) "every pipeline result" else "the pipeline result",
                   paste(names(results[[1]]$by_group), collapse = ", ")))
  }
  if (!is.null(seed)) set.seed(seed)
  sim_col <- switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")

  # Per result and case: observed wealth + simulated wealth vectors (if usable)
  prep <- function(g, cs) {
    obs <- .group_wealth(g, graves, measure)
    sims <- NULL
    if (use_bias && !is.null(g$bias$wealth_scores)) {
      b <- if (identical(graves, "subadults") && !is.null(g$bias_subadults))
        g$bias_subadults else g$bias
      if (identical(b$graves_used %||% "adults", graves)) {
        ws <- b$wealth_scores
        sims <- lapply(Filter(Negate(is.null), c(ws$split, ws$lump)),
                       function(s) s[[sim_col]][!is.na(s[[sim_col]])])
      }
    }
    list(obs = obs, sims = sims)
  }
  pa <- lapply(results, function(r) prep(r$by_group[[case_a]], case_a))
  pb <- lapply(results, function(r) prep(r$by_group[[case_b]], case_b))
  bias_used <- all(vapply(c(pa, pb), function(x) length(x$sims) > 0, logical(1)))
  if (use_bias && !bias_used)
    message("Bias simulations not available for both cases (or for this grave subset); ",
            "only sampling", if (length(results) > 1) " and imputation", " uncertainty is included.")

  draw_one <- function(p) {
    w <- if (bias_used) p$sims[[sample.int(length(p$sims), 1)]] else p$obs
    .case_stats(sample(w, length(w), replace = TRUE), stats, top_pct, bottom_pct, atkinson_eps)
  }
  nm <- .stat_names(stats, atkinson_eps)
  ra <- matrix(NA_real_, n_draws, length(nm), dimnames = list(NULL, nm))
  rb <- ra
  for (d in seq_len(n_draws)) {
    k <- sample.int(length(results), 1)
    ra[d, ] <- draw_one(pa[[k]])[nm]
    rb[d, ] <- draw_one(pb[[k]])[nm]
  }
  obs_a <- .case_stats(pa[[1]]$obs, stats, top_pct, bottom_pct, atkinson_eps)[nm]
  obs_b <- .case_stats(pb[[1]]$obs, stats, top_pct, bottom_pct, atkinson_eps)[nm]
  dif   <- ra - rb
  hi    <- .higher_is_more_unequal(nm)

  summary <- data.frame(
    stat        = nm,
    label       = .stat_labels(nm, top_pct, bottom_pct),
    case_a      = case_a, case_b = case_b,
    observed_a  = unname(obs_a), observed_b = unname(obs_b),
    observed_diff = unname(obs_a - obs_b),
    median_diff = apply(dif, 2, stats::median, na.rm = TRUE),
    lower_95    = apply(dif, 2, stats::quantile, 0.025, na.rm = TRUE),
    upper_95    = apply(dif, 2, stats::quantile, 0.975, na.rm = TRUE),
    p_a_more_unequal = vapply(seq_along(nm), function(j)
      mean(if (hi[j]) dif[, j] > 0 else dif[, j] < 0, na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL)

  draws <- data.frame(
    stat  = rep(nm, each = n_draws),
    label = rep(.stat_labels(nm, top_pct, bottom_pct), each = n_draws),
    a = as.vector(ra), b = as.vector(rb), diff = as.vector(dif),
    stringsAsFactors = FALSE)

  structure(list(summary = summary, draws = draws, case_a = case_a, case_b = case_b,
                 sources = c("sampling", if (bias_used) "categorisation (bias simulations)",
                             if (length(results) > 1) "phase imputation"),
                 n_draws = n_draws, graves = graves, measure = measure),
            class = c("qw_compare", "list"))
}

#' Print a case comparison
#'
#' Prints, for each statistic, the observed values of both cases, the median
#' difference with its 95% interval, and the share of draws in which case A
#' is more unequal than case B, followed by the uncertainty sources included.
#'
#' @param x An object returned by [compare_cases()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' cc <- compare_cases(res, "A: A2", "B: B2", n_draws = 200, seed = 1)
#' print(cc)
#' @export
print.qw_compare <- function(x, ...) {
  cat(sprintf("%s vs %s  (%d draws; uncertainty: %s)\n\n", x$case_a, x$case_b,
              x$n_draws, paste(x$sources, collapse = ", ")))
  s <- x$summary
  out <- data.frame(Statistic = s$label,
                    A = round(s$observed_a, 3), B = round(s$observed_b, 3),
                    `Diff (A-B)` = round(s$observed_diff, 3),
                    `95% interval` = sprintf("[%.3f, %.3f]", s$lower_95, s$upper_95),
                    `P(A more unequal)` = sprintf("%.0f%%", 100 * s$p_a_more_unequal),
                    check.names = FALSE)
  print(out, row.names = FALSE)
  cat("\nBottom share: a larger share means MORE equal, so A counts as more unequal",
      "when its bottom share is smaller.\n")
  invisible(x)
}

#' Plot the difference distributions of [compare_cases()]
#'
#' One panel per statistic: the distribution of the difference A - B across
#' draws (violin with the 95% interval and median), a dashed line at 0, and
#' the share of draws in which A is more unequal.
#'
#' @param x Output of [compare_cases()].
#' @param title Optional title.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' cc <- compare_cases(res, "A: A3", "B: B3", n_draws = 200, seed = 1)
#' plot_compare_cases(cc)
#' @export
plot_compare_cases <- function(x, title = NULL) {
  if (!inherits(x, "qw_compare")) stop("x must be the output of compare_cases().")
  d <- x$draws[!is.na(x$draws$diff), ]
  s <- x$summary
  lv <- s$label
  d$label <- factor(d$label, levels = lv)
  s$label <- factor(s$label, levels = lv)
  s$txt <- sprintf("A more unequal\nin %.0f%% of draws", 100 * s$p_a_more_unequal)
  ggplot2::ggplot(d, ggplot2::aes(x = 0, y = diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_violin(fill = "grey85", colour = "grey50", width = 0.8) +
    ggplot2::geom_pointrange(data = s, ggplot2::aes(x = 0, y = median_diff,
                                                   ymin = lower_95, ymax = upper_95),
                             inherit.aes = FALSE, colour = "black") +
    ggplot2::geom_text(data = s, ggplot2::aes(x = 0, y = Inf, label = txt),
                       inherit.aes = FALSE, vjust = 1.2, size = 3) +
    ggplot2::facet_wrap(~ label, scales = "free_y", nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::labs(title = title %||% paste0(x$case_a, " minus ", x$case_b, .age_label(x$graves)),
                  x = NULL, y = "Difference A - B",
                  caption = paste0("Uncertainty: ", paste(x$sources, collapse = ", "),
                                   ". Point and bar: median and 95% interval. ",
                                   "Bottom share: negative = A more unequal."))
}


# ---- multiverse_compare() --------------------------------------------------

#' Merge columns matching each pattern into one column (sum of counts)
#' @noRd
.lump_columns <- function(df, patterns, protect) {
  for (i in seq_along(patterns)) {
    cols <- setdiff(grep(patterns[i], names(df), value = TRUE), protect)
    if (length(cols) < 2) next
    m <- sapply(df[cols], function(x) suppressWarnings(as.numeric(x)))
    m[is.na(m)] <- 0
    v <- rowSums(as.matrix(m))
    v[v == 0] <- NA
    new <- names(patterns)[i] %||% ""
    if (!nzchar(new)) new <- paste0("lumped_", i)
    df <- df[, setdiff(names(df), cols), drop = FALSE]
    df[[new]] <- v
  }
  df
}

#' Multiverse comparison of inequality across data sources
#'
#' Data from different sources differ in more than category granularity:
#' some record construction features, some lack graves without goods, some
#' split pottery into many forms. Instead of choosing one "correct" set-up,
#' this runs [count_pipeline()] for **every combination** of defensible
#' choices (a "multiverse") and reports, for each pair of cases from
#' different datasets, how often case A comes out more unequal than case B.
#' If the answer is the same in nearly all combinations, it is robust to
#' these choices; if it flips, the data cannot decide the question.
#'
#' The choices (each a dimension of the grid):
#' - `exclude_patterns`: alternative column exclusions (regular expressions,
#'   `NA` = exclude nothing extra), e.g. with and without construction
#'   columns.
#' - `drop_empty`: keep graves without goods, or drop them (in every
#'   dataset) -- e.g. because one source hardly records them.
#' - `age_known_only`: use all graves, or only graves with a recorded
#'   `adult_count` or `subadult_count` above 0. Graves without age
#'   information are otherwise counted as adult graves; this tests whether
#'   that matters. Graves with age information are a selection (often those
#'   studied anthropologically), so the subset is a check, not a cleaner
#'   version of the data. Datasets without age columns are used unfiltered,
#'   with a warning. Default `FALSE` (all graves only).
#' - `row_filters`: alternative selections of graves, for checks that
#'   differ between datasets, e.g. "secure graves only" in one source and
#'   "graves with age information" in another. Each alternative is `NULL`
#'   (all graves), a function that takes a dataset and returns `TRUE` for
#'   the rows to keep (applied to every dataset), or a named list of such
#'   functions, one per dataset (datasets not named are not filtered).
#'   [has_age_info()] is a ready-made filter.
#' - `lump`: alternative lumpings. Each alternative is `NULL` (no lumping)
#'   or a named list giving, per dataset, regular expressions whose matching
#'   columns are merged into one column (e.g. all pottery forms into one).
#'   Names of the regex vector become the new column names.
#' - imputations (optional): if a dataset is given as a list of data frames
#'   (e.g. the output of [impute_phases()]), each one is used in turn;
#'   datasets with fewer versions are recycled. Or use `impute` to let this
#'   function make the imputed versions itself. Every other choice is then
#'   combined with every imputation, so the shares also include the
#'   uncertainty of the phase assignment.
#'
#' Only the inequality statistics are compared (they are unit-free); wealth
#' levels are not comparable between sources.
#'
#' @param data Named list of datasets (data frames, or lists of data frames
#'   for imputations).
#' @param group_col,id_col,date_col,exclude_cols,method,use_age_sep,min_group_size
#'   As in [count_pipeline()] (used for every run).
#' @param exclude_patterns Named character vector of alternatives, `NA` for
#'   none. Default: `c(all_columns = NA, no_constructions = "^(CON|ritual)_")`.
#' @param drop_empty Logical vector of alternatives (default `c(FALSE, TRUE)`).
#' @param age_known_only Logical vector of alternatives: `FALSE` uses all
#'   graves, `TRUE` only graves with age information. Default `FALSE`; use
#'   `c(FALSE, TRUE)` to compare both (this doubles the number of runs).
#' @param row_filters Named list of grave-selection alternatives (default
#'   `list(all_graves = NULL)`: no filtering); see the list above. Example:
#'   `list(all_graves = NULL, checked = list(SK = has_age_info,
#'   GAC = function(d) d$context_type == "grave"))`. Rows where a filter
#'   returns `NA` are dropped.
#' @param lump Named list of lumping alternatives (default: no lumping).
#' @param impute Optional named list: for each dataset to impute, a list of
#'   arguments for [impute_phases()] (at least `phase_col`), e.g.
#'   `list(SK = list(phase_col = "Zeitstufen_final",
#'   date_col = "date_median_BCE"))`. `list(SK = list())` uses `group_col`
#'   as the phase column and the defaults otherwise.
#' @param n_imp Number of imputations when `impute` is used (default 20).
#' @param impute_seed Optional seed for the imputations.
#' @param comparisons Optional list of `c(case_a, case_b)` pairs. Default:
#'   every pair of cases from different datasets.
#' @param stats,graves,measure,top_pct,bottom_pct,atkinson_eps As in
#'   [compare_cases()].
#' @param progress Show a progress bar over the runs (default `interactive()`).
#' @return An object of class `"qw_multiverse"`: `values` (every statistic
#'   for every case and specification), `differences` (A - B per comparison,
#'   statistic and specification), `summary` (per comparison and statistic:
#'   number of specifications, share in which A is more unequal, median /
#'   min / max difference) and `specs` (the grid). Plot with
#'   [plot_multiverse()].
#' @examples
#' \donttest{
#' ph <- read_count_data(example_data_path("phases"))
#' mv <- multiverse_compare(
#'   split(ph, ph$culture), group_col = "phase", id_col = "Grave_ID",
#'   date_col = "date_BCE",
#'   exclude_patterns = c(all_columns = NA, no_constructions = "^CON_"),
#'   drop_empty = c(FALSE, TRUE),
#'   lump = list(none = NULL,
#'               pottery_lumped = list(A = c(c_all = "^c_"), B = c(c_all = "^c_"))),
#'   comparisons = list(c("A: A2", "B: B2"), c("A: A3", "B: B3")),
#'   progress = FALSE)
#' mv                 # share of specifications in which A is more unequal
#' mv$summary
#' plot_multiverse(mv, type = "summary")
#'
#' # Add phase imputation for culture A as one more dimension
#' mv_i <- multiverse_compare(
#'   split(ph, ph$culture), group_col = "phase", id_col = "Grave_ID",
#'   date_col = "date_BCE", drop_empty = FALSE,
#'   impute = list(A = list(phase_col = "phase", unit_col = "Grave_ID",
#'                          event_pattern = NULL)),
#'   n_imp = 3, impute_seed = 1,
#'   comparisons = list(c("A: A2", "B: B2")), progress = FALSE)
#' }
#' @export
multiverse_compare <- function(data, group_col, id_col = NULL, date_col = NULL,
                               exclude_cols = NULL,
                               exclude_patterns = c(all_columns = NA,
                                                    no_constructions = "^(CON|ritual)_"),
                               drop_empty = c(FALSE, TRUE),
                               age_known_only = FALSE,
                               row_filters = list(all_graves = NULL),
                               lump = list(none = NULL),
                               impute = NULL, n_imp = 20, impute_seed = NULL,
                               comparisons = NULL,
                               stats   = c("gini", "top_share", "bottom_share", "atkinson"),
                               graves  = c("adults", "all", "subadults"),
                               measure = c("combined", "prestige", "tot"),
                               top_pct = 10, bottom_pct = 50, atkinson_eps = 2,
                               method = c("bayesian", "mean"), use_age_sep = TRUE,
                               min_group_size = 3, progress = interactive()) {
  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  method  <- match.arg(method)
  stats   <- match.arg(stats, c("gini", "top_share", "bottom_share", "atkinson",
                                "mean_median_ratio"), several.ok = TRUE)
  if (!is.list(data) || is.data.frame(data) || is.null(names(data)) || any(!nzchar(names(data))))
    stop("'data' must be a named list of datasets, e.g. list(SK = sk, GAC = gac).")
  if (is.null(names(exclude_patterns)))
    names(exclude_patterns) <- ifelse(is.na(exclude_patterns), "none", exclude_patterns)
  if (is.null(names(lump))) names(lump) <- paste0("lump", seq_along(lump))

  # Imputations: every dataset as a list of versions
  if (!is.null(impute)) {
    unknown <- setdiff(names(impute), names(data))
    if (length(unknown) > 0)
      stop("impute names not in data: ", paste(unknown, collapse = ", "))
    for (ds in names(impute)) {
      if (!is.data.frame(data[[ds]]))
        stop("Dataset '", ds, "' is already a list of versions; don't also impute it.")
      args <- impute[[ds]]
      if (is.null(args$phase_col)) args$phase_col <- group_col[1]
      if (is.null(args$date_col) && !is.null(date_col)) args$date_col <- date_col
      if (is.null(args$n_imp)) args$n_imp <- n_imp
      if (is.null(args$seed) && !is.null(impute_seed)) args$seed <- impute_seed
      message("Imputing ", ds, ": ", appendLF = FALSE)
      data[[ds]] <- do.call(impute_phases, c(list(df = data[[ds]]), args))
    }
  }
  versions <- lapply(data, function(d) if (is.data.frame(d)) list(d) else d)
  n_imp <- max(lengths(versions))

  if (!is.logical(age_known_only) || anyNA(age_known_only))
    stop("age_known_only must be TRUE, FALSE or c(FALSE, TRUE).")
  if (any(age_known_only)) {
    no_age <- names(versions)[vapply(versions, function(v)
      !any(c("adult_count", "subadult_count") %in% names(v[[1]])), logical(1))]
    if (length(no_age))
      warning("No adult_count/subadult_count in ", paste(no_age, collapse = ", "),
              ": age_known_only = TRUE leaves these datasets unfiltered.", call. = FALSE)
  }
  if (!is.list(row_filters) || is.null(names(row_filters)) || any(names(row_filters) == ""))
    stop("row_filters must be a named list, e.g. list(all_graves = NULL, age_known = has_age_info).")
  for (nm in names(row_filters)) {
    rf <- row_filters[[nm]]
    ok <- is.null(rf) || is.function(rf) ||
      (is.list(rf) && !is.null(names(rf)) && all(vapply(rf, is.function, logical(1))))
    if (!ok) stop("row_filters$", nm, " must be NULL, a function, or a named list of functions ",
                  "(one per dataset).")
    if (is.list(rf)) {
      unk <- setdiff(names(rf), names(versions))
      if (length(unk)) stop("row_filters$", nm, " names unknown dataset(s): ",
                            paste(unk, collapse = ", "))
    }
  }
  specs <- expand.grid(exclude = names(exclude_patterns), drop_empty = drop_empty,
                       age_known_only = unique(age_known_only),
                       row_filter = names(row_filters),
                       lump = names(lump), imputation = seq_len(n_imp),
                       stringsAsFactors = FALSE)
  specs$spec_id <- seq_len(nrow(specs))
  protect <- c(group_col, id_col, date_col, "adult_count", "subadult_count", "lat", "lng")

  pb <- if (isTRUE(progress)) utils::txtProgressBar(0, nrow(specs), style = 3) else NULL
  if (!is.null(pb)) on.exit(close(pb), add = TRUE)

  vals <- list()
  for (i in seq_len(nrow(specs))) {
    sp <- specs[i, ]
    pat <- exclude_patterns[[sp$exclude]]
    pat <- if (is.na(pat)) NULL else pat
    dl <- lapply(names(versions), function(ds) {
      v  <- versions[[ds]]
      df <- v[[(sp$imputation - 1) %% length(v) + 1]]
      lp <- lump[[sp$lump]][[ds]]
      if (!is.null(lp)) df <- .lump_columns(df, lp, protect)
      if (isTRUE(sp$drop_empty)) {
        cc <- setdiff(identify_count_columns(df), c(protect, exclude_cols))
        if (!is.null(pat)) cc <- cc[!grepl(pat, cc, ignore.case = TRUE)]
        has <- rowSums(sapply(df[cc], function(x) {
          x <- suppressWarnings(as.numeric(x)); !is.na(x) & x > 0 })) > 0
        df <- df[has, , drop = FALSE]
      }
      if (isTRUE(sp$age_known_only) &&
          any(c("adult_count", "subadult_count") %in% names(df))) {
        df <- df[has_age_info(df), , drop = FALSE]     # same rule as the exported filter
      }
      rf <- row_filters[[sp$row_filter]]
      f  <- if (is.function(rf)) rf else if (is.list(rf)) rf[[ds]] else NULL
      if (!is.null(f)) {
        keep <- f(df)
        if (!is.logical(keep) || length(keep) != nrow(df))
          stop("row filter '", sp$row_filter, "' for dataset ", ds,
               " must return one TRUE/FALSE per row.")
        df <- df[!is.na(keep) & keep, , drop = FALSE]
      }
      df
    })
    names(dl) <- names(versions)
    res <- tryCatch(suppressWarnings(suppressMessages(count_pipeline(
      dl, group_col = group_col, id_col = id_col, date_col = date_col,
      exclude_cols = exclude_cols, exclude_pattern = pat, method = method,
      use_age_sep = use_age_sep, run_bias_sim = FALSE, conf_level = NA,
      min_group_size = min_group_size, progress = FALSE))),
      error = function(e) { warning("Specification ", i, " failed: ", conditionMessage(e),
                                    call. = FALSE); NULL })
    if (!is.null(res)) {
      for (cs in names(res$by_group)) {
        w  <- .group_wealth(res$by_group[[cs]], graves, measure)
        st <- .case_stats(w, stats, top_pct, bottom_pct, atkinson_eps)
        vals[[length(vals) + 1]] <- data.frame(spec_id = i, case = cs,
          dataset = res$by_group[[cs]]$dataset %||% NA_character_,
          n = length(w), stat = names(st), value = unname(st), stringsAsFactors = FALSE)
      }
    }
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }
  values <- merge(do.call(rbind, vals), specs, by = "spec_id")

  # Comparisons: default every cross-dataset pair
  if (is.null(comparisons)) {
    cs <- unique(values[, c("case", "dataset")])
    comparisons <- list()
    for (a in seq_len(nrow(cs))) for (b in seq_len(nrow(cs)))
      if (a < b && cs$dataset[a] != cs$dataset[b])
        comparisons[[length(comparisons) + 1]] <- c(cs$case[a], cs$case[b])
  }
  diffs <- do.call(rbind, lapply(comparisons, function(cp) {
    va <- values[values$case == cp[1], c("spec_id", "stat", "value")]
    vb <- values[values$case == cp[2], c("spec_id", "stat", "value")]
    m  <- merge(va, vb, by = c("spec_id", "stat"), suffixes = c("_a", "_b"))
    if (nrow(m) == 0) return(NULL)
    m$case_a <- cp[1]; m$case_b <- cp[2]
    m$diff <- m$value_a - m$value_b
    m
  }))
  diffs <- merge(diffs, specs, by = "spec_id")
  diffs$comparison <- paste(diffs$case_a, "vs", diffs$case_b)
  diffs$a_more_unequal <- ifelse(.higher_is_more_unequal(diffs$stat),
                                 diffs$diff > 0, diffs$diff < 0)
  diffs$label <- .stat_labels(diffs$stat, top_pct, bottom_pct)

  summ <- do.call(rbind, lapply(split(diffs, list(diffs$comparison, diffs$stat), drop = TRUE),
    function(d) data.frame(comparison = d$comparison[1], case_a = d$case_a[1],
                           case_b = d$case_b[1], stat = d$stat[1], label = d$label[1],
                           n_specs = sum(!is.na(d$diff)),
                           share_a_more_unequal = mean(d$a_more_unequal, na.rm = TRUE),
                           median_diff = stats::median(d$diff, na.rm = TRUE),
                           min_diff = min(d$diff, na.rm = TRUE),
                           max_diff = max(d$diff, na.rm = TRUE),
                           stringsAsFactors = FALSE)))
  rownames(summ) <- NULL
  summ <- summ[order(summ$comparison, match(summ$stat, unique(values$stat))), ]

  structure(list(values = values, differences = diffs, summary = summ, specs = specs,
                 stats = unique(values$stat), top_pct = top_pct, bottom_pct = bottom_pct,
                 graves = graves),
            class = c("qw_multiverse", "list"))
}

#' Print a multiverse comparison
#'
#' Prints the number of specifications and, for each comparison and
#' statistic, the share of specifications in which case A is more unequal
#' than case B, with the median and range of the difference.
#'
#' @param x An object returned by [multiverse_compare()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' ph <- read_count_data(example_data_path("phases"))
#' mv <- multiverse_compare(
#'   split(ph, ph$culture), group_col = "phase", id_col = "Grave_ID",
#'   date_col = "date_BCE",
#'   exclude_patterns = c(all_columns = NA, no_constructions = "^CON_"),
#'   drop_empty = c(FALSE, TRUE),
#'   lump = list(none = NULL,
#'               pottery_lumped = list(A = c(c_all = "^c_"), B = c(c_all = "^c_"))),
#'   comparisons = list(c("A: A2", "B: B2"), c("A: A3", "B: B3")),
#'   progress = FALSE)
#' print(mv)
#' }
#' @export
print.qw_multiverse <- function(x, ...) {
  cat(sprintf("Multiverse comparison: %d specifications, %d comparisons\n\n",
              nrow(x$specs), length(unique(x$summary$comparison))))
  s <- x$summary
  out <- data.frame(Comparison = s$comparison, Statistic = s$label,
                    `A more unequal` = sprintf("%.0f%%", 100 * s$share_a_more_unequal),
                    `Median diff` = round(s$median_diff, 3),
                    Range = sprintf("[%.3f, %.3f]", s$min_diff, s$max_diff),
                    check.names = FALSE)
  print(out, row.names = FALSE)
  invisible(x)
}

#' Plot a multiverse comparison
#'
#' One panel per statistic. Each row is a comparison (case A vs case B),
#' each point one specification (one combination of choices), coloured by
#' the choice given in `colour_by`. The dashed line marks no difference;
#' the number at the right is the share of specifications in which A is
#' more unequal. Points all on one side = a robust result; points on both
#' sides = the answer depends on how the data are prepared.
#'
#' With `type = "summary"` the plot is a compact grid instead: rows are the
#' A cases, columns the B cases, one panel per statistic, and each cell
#' shows the share of specifications in which A is more unequal (red = A
#' more unequal, blue = B more unequal, white = undecided). With `split_by`
#' the shares are shown separately for each level of one choice (e.g.
#' `split_by = "lump"`: pottery split vs lumped side by side), which shows
#' directly whether that choice decides the answer.
#'
#' @param x Output of [multiverse_compare()].
#' @param stats Optional subset of statistics (names as in `x$stats`).
#' @param colour_by Which choice colours the points (`type = "points"`):
#'   `"exclude"`, `"drop_empty"`, `"age_known_only"`, `"row_filter"`,
#'   `"lump"` or `"imputation"`.
#' @param type `"points"` (default; every specification as a point) or
#'   `"summary"` (grid of shares).
#' @param split_by For `type = "summary"`: optional choice to split the
#'   shares by (`"exclude"`, `"drop_empty"`, `"age_known_only"`,
#'   `"row_filter"`, `"lump"`, `"imputation"`).
#' @param case_order For `type = "summary"`: optional order of the cases,
#'   as full case names (`"SK: 2480-2300"`) or phase labels (`"2480-2300"`);
#'   earliest first gives a chronological grid.
#' @param title Optional title.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' \donttest{
#' ph <- read_count_data(example_data_path("phases"))
#' mv <- multiverse_compare(
#'   split(ph, ph$culture), group_col = "phase", id_col = "Grave_ID",
#'   date_col = "date_BCE",
#'   exclude_patterns = c(all_columns = NA, no_constructions = "^CON_"),
#'   drop_empty = c(FALSE, TRUE),
#'   lump = list(none = NULL,
#'               pottery_lumped = list(A = c(c_all = "^c_"), B = c(c_all = "^c_"))),
#'   comparisons = list(c("A: A2", "B: B2"), c("A: A3", "B: B3")),
#'   progress = FALSE)
#' plot_multiverse(mv)                                   # one point per specification
#' plot_multiverse(mv, colour_by = "lump")
#' plot_multiverse(mv, type = "summary")                 # grid of shares
#' plot_multiverse(mv, type = "summary", split_by = "exclude")
#' }
#' @export
plot_multiverse <- function(x, stats = NULL,
                            colour_by = c("exclude", "drop_empty", "age_known_only", "row_filter", "lump",
                                          "imputation"),
                            type = c("points", "summary"),
                            split_by = NULL,
                            case_order = NULL,
                            title = NULL) {
  if (!inherits(x, "qw_multiverse")) stop("x must be the output of multiverse_compare().")
  colour_by <- match.arg(colour_by)
  type      <- match.arg(type)
  if (type == "summary")
    return(.plot_multiverse_summary(x, stats = stats, split_by = split_by,
                                    case_order = case_order, title = title))
  d <- x$differences
  s <- x$summary
  if (!is.null(stats)) { d <- d[d$stat %in% stats, ]; s <- s[s$stat %in% stats, ] }
  lv <- unique(.stat_labels(x$stats, x$top_pct, x$bottom_pct))
  d$label <- factor(d$label, levels = lv); s$label <- factor(s$label, levels = lv)
  d$colour <- factor(d[[colour_by]])
  s$txt <- sprintf("%.0f%%", 100 * s$share_a_more_unequal)
  ggplot2::ggplot(d, ggplot2::aes(x = diff, y = comparison, colour = colour)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_point(position = ggplot2::position_jitter(height = 0.15, width = 0),
                        alpha = 0.7, size = 2) +
    ggplot2::geom_text(data = s, ggplot2::aes(x = Inf, y = comparison, label = txt),
                       inherit.aes = FALSE, hjust = 1.1, size = 3) +
    ggplot2::facet_wrap(~ label, scales = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = title %||% paste0("Multiverse comparison: case A minus case B",
                                            .age_label(x$graves)),
                  x = "Difference A - B", y = NULL, colour = colour_by,
                  caption = paste0("Each point: one specification. Number: share of ",
                                   "specifications in which A is more unequal ",
                                   "(bottom share: negative difference = A more unequal)."))
}


#' Grid view of a multiverse comparison (type = "summary")
#' @noRd
.plot_multiverse_summary <- function(x, stats = NULL, split_by = NULL, case_order = NULL,
                                     title = NULL) {
  ord <- function(cases) {
    cases <- unique(cases)
    if (is.null(case_order)) return(cases)
    pos <- match(cases, case_order)
    pos[is.na(pos)] <- match(sub("^.*?: ", "", cases[is.na(pos)]), case_order)
    cases[order(is.na(pos), pos)]
  }
  d <- x$differences
  if (!is.null(stats)) d <- d[d$stat %in% stats, ]
  if (!is.null(split_by)) {
    split_by <- match.arg(split_by, c("exclude", "drop_empty", "age_known_only", "row_filter",
                                      "lump", "imputation"))
    d$split <- paste0(split_by, ": ", d[[split_by]])
  } else d$split <- "all specifications"
  keys <- list(d$case_a, d$case_b, d$label, d$split)
  s <- do.call(rbind, lapply(split(d, keys, drop = TRUE), function(g)
    data.frame(case_a = g$case_a[1], case_b = g$case_b[1], label = g$label[1],
               split = g$split[1], n = sum(!is.na(g$a_more_unequal)),
               share = mean(g$a_more_unequal, na.rm = TRUE), stringsAsFactors = FALSE)))
  lv <- unique(.stat_labels(x$stats, x$top_pct, x$bottom_pct))
  s$label  <- factor(s$label, levels = intersect(lv, unique(s$label)))
  s$case_a <- factor(s$case_a, levels = rev(ord(x$differences$case_a)))
  s$case_b <- factor(s$case_b, levels = ord(x$differences$case_b))
  s$txt <- sprintf("%.0f%%", 100 * s$share)
  p <- ggplot2::ggplot(s, ggplot2::aes(x = case_b, y = case_a, fill = share)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = txt), size = 3.2) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                                  midpoint = 0.5, limits = c(0, 1),
                                  labels = function(v) paste0(round(100 * v), "%")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::labs(title = title %||% paste0("Multiverse comparison", .age_label(x$graves)),
                  subtitle = "How often is the row case more unequal than the column case?",
                  x = NULL, y = NULL, fill = "Row case\nmore unequal\nin",
                  caption = paste0("Share of ", if (is.null(split_by)) "all" else "these",
                                   " specifications in which the row case is more unequal ",
                                   "than the column case.\nRed: row more unequal; blue: ",
                                   "column more unequal; white: depends on the choices."))
  if (is.null(split_by)) p + ggplot2::facet_wrap(~ label)
  else p + ggplot2::facet_grid(label ~ split)
}




#' Which graves have age information?
#'
#' A ready-made row filter for [multiverse_compare()] (`row_filters`) or for
#' subsetting data yourself: `TRUE` for graves with a recorded
#' `adult_count` or `subadult_count` above 0, `FALSE` for graves whose age
#' is unknown (which the pipeline otherwise counts as adult graves).
#'
#' @param df A data frame with `adult_count` and/or `subadult_count`.
#' @return A logical vector, one value per row. If `df` has neither column,
#'   all `TRUE` (nothing can be filtered), with a warning.
#' @examples
#' df <- data.frame(id = 1:4, adult_count = c(1, 0, NA, 0),
#'                  subadult_count = c(0, 1, NA, 0))
#' has_age_info(df)           # TRUE TRUE FALSE FALSE
#' df[has_age_info(df), ]
#' @export
has_age_info <- function(df) {
  cols <- intersect(c("adult_count", "subadult_count"), names(df))
  if (length(cols) == 0) {
    warning("No adult_count or subadult_count column: all rows kept.", call. = FALSE)
    return(rep(TRUE, nrow(df)))
  }
  ages <- vapply(cols, function(cl) suppressWarnings(as.numeric(df[[cl]])), numeric(nrow(df)))
  rowSums(matrix(ages, nrow = nrow(df)), na.rm = TRUE) > 0
}
