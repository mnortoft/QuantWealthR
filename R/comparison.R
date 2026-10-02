# ============================================================================
# Multi-case comparison functions
# Work with count_pipeline()/phys_pipeline() output to compare cases side by side
# ============================================================================

#' Resolve the left-to-right order of cases for the comparison plots
#'
#' @param cases Character (or factor) vector of case names present in the data.
#' @param case_order Optional character vector giving the desired order. Cases
#'   not mentioned are appended at the end (in their existing order); names not
#'   present in the data are ignored. If NULL, the existing order is kept.
#' @return Character vector of unique case names in the resolved order.
#' @noRd
.order_cases <- function(cases, case_order = NULL) {
  cases <- unique(as.character(cases))
  if (is.null(case_order)) return(cases)
  case_order <- as.character(case_order)
  unknown <- setdiff(case_order, cases)
  if (length(unknown) > 0)
    warning("case_order names not found in the data (ignored): ",
            paste(unknown, collapse = ", "))
  leftover <- setdiff(cases, case_order)
  if (length(leftover) > 0)
    warning("Cases not listed in case_order (appended at the end): ",
            paste(leftover, collapse = ", "))
  c(intersect(case_order, cases), leftover)
}


#' Collect per-case records from one or more pipeline results
#'
#' Internal helper for [plot_gini_comparison()]. `pipeline_result` can be:
#' a single [count_pipeline()]/[phys_pipeline()] result, or a named list of
#' several such results -- e.g. several [count_pipeline()] results for sites
#' with incompatible columns that were therefore analysed in separate calls
#' -- since each result already carries its own `dataset`/`case` tagging
#' internally when *it* was given multiple datasets at the pipeline stage.
#' Mixing a [count_pipeline()] result with a [phys_pipeline()] result in the
#' same call is gathered here without complaint, but is then rejected by
#' `.assert_single_data_type()` in the calling plot function -- see that
#' function for why.
#'
#' The two layers of tagging (a pipeline result's own internal `dataset`
#' tag from a multi-dataset pipeline call, and an optional outer name from
#' a list passed directly to this function) are combined into one final
#' `dataset` label per case, e.g. `"Counts: SiteA"`.
#'
#' @param pipeline_result See above.
#' @return A list of case records, each with `dataset` (possibly `NA`),
#'   `case`, `group`, `n`, `is_physical`, and either `inequality` (the full
#'   [inequality_summary()] list, for count data) or `gini`/`sizes` (for
#'   physical data).
#' @noRd
.gather_comparison_records <- function(pipeline_result) {
  is_single <- is.list(pipeline_result) &&
    !is.null(pipeline_result$summary) && !is.null(pipeline_result$by_group)
  results_list <- if (is_single) list(pipeline_result) else pipeline_result

  if (!is.list(results_list) || length(results_list) == 0)
    stop("pipeline_result must be a count_pipeline()/phys_pipeline() result, ",
         "or a list of such results.")

  outer_names <- names(results_list)
  if (is.null(outer_names)) outer_names <- rep(NA_character_, length(results_list))
  outer_names[!nzchar(outer_names %||% "")] <- NA_character_
  if (length(results_list) > 1) {
    missing <- is.na(outer_names)
    if (any(missing))
      outer_names[missing] <- paste0("Result", seq_along(outer_names))[missing]
  }

  records <- list()
  for (i in seq_along(results_list)) {
    res <- results_list[[i]]
    if (is.null(res$by_group) || length(res$by_group) == 0) next

    is_physical <- "gini" %in% names(res$summary) && !"gini_combined" %in% names(res$summary)
    outer_nm    <- outer_names[i]

    for (case_key in names(res$by_group)) {
      g <- res$by_group[[case_key]]
      inner_dataset <- g$dataset %||% NA_character_

      dataset <- if (!is.na(outer_nm) && !is.na(inner_dataset)) {
        paste0(outer_nm, ": ", inner_dataset)
      } else if (!is.na(outer_nm)) {
        outer_nm
      } else {
        inner_dataset
      }

      rec_id <- paste(i, case_key, sep = "::")
      records[[rec_id]] <- list(
        dataset     = dataset,
        case        = case_key,
        group       = as.character(g$group %||% case_key),
        n           = g$n,
        is_physical = is_physical,
        inequality  = g$inequality,
        bias        = g$bias,
        bias_subadults = g$bias_subadults,
        gini        = g$gini,
        sizes       = g$sizes,
        prestige_df = g$prestige_df,
        date        = g$date     %||% NA_real_,
        date_min    = g$date_min %||% NA_real_,
        date_max    = g$date_max %||% NA_real_
      )
    }
  }
  records
}


#' Share of total wealth held by the richest `pct` percent of graves
#'
#' Uses k = n * pct / 100 graves; when k is not a whole number the next
#' grave counts with its fractional part (e.g. k = 0.8 -> 80% of the
#' richest grave). With k < 1 the value is just a fraction of the single
#' richest grave's share, so it says little about a "top group".
#' @noRd
.top_share <- function(v, pct) {
  v <- sort(v[!is.na(v)], decreasing = TRUE)
  n <- length(v); tot <- sum(v)
  if (n == 0 || !isTRUE(tot > 0)) return(NA_real_)
  k <- n * pct / 100
  full <- min(floor(k), n); frac <- k - floor(k)
  num <- sum(v[seq_len(full)]) + if (frac > 0 && full < n) frac * v[full + 1] else 0
  num / tot
}

#' Share of total wealth held by the poorest `pct` percent of graves
#'
#' Mirror image of `.top_share()`: k = n * pct / 100 poorest graves, a
#' fractional grave counting with its fraction.
#' @noRd
.bottom_share <- function(v, pct) {
  v <- v[!is.na(v)]
  if (length(v) == 0 || !isTRUE(sum(v) > 0)) return(NA_real_)
  1 - .top_share(v, 100 - pct)
}

#' Atkinson inequality index
#'
#' A(eps) = 1 - (equally distributed equivalent) / mean. eps is the
#' "inequality aversion": the larger it is, the more weight the poorest
#' graves get (eps = 0.5 mild, 1 = geometric mean, 2 = harmonic mean, strongly
#' bottom-sensitive). Needs strictly positive values; with the base value of
#' the wealth score that is always the case for count data.
#' @noRd
.atkinson <- function(v, eps) {
  v <- v[!is.na(v)]
  if (length(v) == 0 || any(v <= 0)) return(NA_real_)
  m <- mean(v)
  ede <- if (abs(eps - 1) < 1e-12) exp(mean(log(v))) else mean(v^(1 - eps))^(1 / (1 - eps))
  1 - ede / m
}

#' Equally distributed equivalent wealth: mean x (1 - Atkinson(eps)), the
#' wealth per grave that, shared equally, would count as much as the actual
#' unequal distribution. Combines level and inequality.
#' @noRd
.ede <- function(v, eps) {
  a <- .atkinson(v, eps)
  if (is.na(a)) NA_real_ else mean(v[!is.na(v)]) * (1 - a)
}

#' Display labels of the share/Atkinson series
#' @noRd
.index_labels <- function(top_pct = 10, bottom_pct = 50, atkinson_eps = c(0.5, 1, 2)) {
  list(top    = sprintf("Top %g%% share", top_pct),
       bottom = sprintf("Bottom %g%% share", bottom_pct),
       atk    = stats::setNames(sprintf("Atkinson (%s=%g)", .sym("epsilon"), atkinson_eps),
                                paste0("atkinson_", atkinson_eps)),
       ede    = stats::setNames(sprintf("Equal-equivalent wealth (%s=%g)", .sym("epsilon"), atkinson_eps),
                                paste0("ede_", atkinson_eps)))
}

#' Check the share/Atkinson parameters
#' @noRd
.check_index_args <- function(series, top_pct, bottom_pct, atkinson_eps) {
  ok_pct <- function(x) is.numeric(x) && length(x) == 1 && x > 0 && x < 100
  if ("top_share" %in% series && !ok_pct(top_pct))
    stop("'top_pct' must be a single number between 0 and 100 (e.g. 10 for the top 10%).")
  if ("bottom_share" %in% series && !ok_pct(bottom_pct))
    stop("'bottom_pct' must be a single number between 0 and 100 (e.g. 50 for the poorest half).")
  if (any(c("atkinson", "ede") %in% series) &&
      !(is.numeric(atkinson_eps) && length(atkinson_eps) >= 1 && all(atkinson_eps > 0)))
    stop("'atkinson_eps' must be one or more positive numbers, e.g. c(0.5, 1, 2).")
  invisible(TRUE)
}

#' Per-grave wealth values of the plotted subset for one case record
#' @noRd
.record_wealth <- function(rec, graves, measure) {
  if (rec$is_physical) return(rec$sizes[!is.na(rec$sizes)])
  pd  <- rec$prestige_df
  col <- switch(measure, combined = "combined_total", prestige = "prestige_total",
                tot = "raw_TOT")
  if (is.null(pd) || is.null(pd[[col]])) return(NULL)
  keep <- switch(graves,
    adults    = if ("is_adult_grave" %in% names(pd)) pd$is_adult_grave else rep(TRUE, nrow(pd)),
    subadults = if ("is_subadult_only" %in% names(pd)) pd$is_subadult_only else rep(FALSE, nrow(pd)),
    all       = rep(TRUE, nrow(pd)))
  v <- pd[[col]][keep %in% TRUE]
  if (measure == "tot") v <- v + 1
  v[!is.na(v)]
}

#' Warn when the top-x% share rests on very few graves
#' @noRd
.warn_top_share <- function(long_df, top_pct, top_label) {
  sel <- long_df$measure == top_label & !is.na(long_df$value)
  if (!any(sel)) return(invisible(NULL))
  k <- long_df$n[sel] * top_pct / 100
  few <- k < 3
  if (any(few)) {
    warning(top_label, " rests on fewer than 3 graves in: ",
            paste(sprintf("%s (n=%d -> %.2g graves)", long_df$case[sel][few],
                          as.integer(long_df$n[sel][few]), k[few]), collapse = "; "),
            ". The value then depends on one or two individual graves (with < 1 grave ",
            "it is only a fraction of a single grave's share), so it is very ",
            "unstable; differences between such cases are hard to interpret.", call. = FALSE)
  }
  invisible(NULL)
}


#' Build the long-format plot rows for one case record
#'
#' Internal helper for [plot_gini_comparison()]. Pulls the requested
#' `series` values (and CIs, where available) for one case, either from the
#' count-data [inequality_summary()] `grid` (filtered to `measure`/`graves`)
#' or directly from the physical-measure Gini + size vector.
#'
#' Absolute Gini gets a confidence interval only for count-data cases where
#' the bias simulation was run (`run_bias_sim = TRUE` in [count_pipeline()])
#' *and* the requested `measure`/`graves` combination matches what that
#' simulation actually targets -- `"combined"` on the main (adults, or all
#' graves if there was no age separation) subset. Every other series/case
#' combination is a point estimate with no CI, which is a real limitation
#' worth knowing about, not a bug: the `grid` in [inequality_summary()]
#' gives Gini bootstrap CIs, but absolute Gini, mean wealth, and median
#' wealth have no analogous CI machinery built for arbitrary graves/measure
#' choices.
#'
#' @noRd
.comparison_value_rows <- function(rec, series, graves, measure, top_pct = 10,
                                   bottom_pct = 50, atkinson_eps = c(0.5, 1, 2)) {
  labs_ix <- .index_labels(top_pct, bottom_pct, atkinson_eps)
  base <- data.frame(
    dataset = rec$dataset %||% NA_character_,
    case    = rec$case,
    group   = rec$group,
    n       = rec$n,
    date    = rec$date %||% NA_real_,
    stringsAsFactors = FALSE
  )
  out <- list()

  if (rec$is_physical) {
    g        <- rec$gini
    mean_v   <- mean(rec$sizes, na.rm = TRUE)
    median_v <- median(rec$sizes, na.rm = TRUE)

    if ("gini" %in% series)
      out$gini <- cbind(base, data.frame(
        measure = "Gini", value = unname(g["gini"]),
        lower = unname(g["lower"]), upper = unname(g["upper"])
      ))
    if ("abs_gini" %in% series)
      out$abs_gini <- cbind(base, data.frame(
        measure = "Absolute Gini", value = unname(g["gini"]) * mean_v,
        lower = NA_real_, upper = NA_real_
      ))
    if ("mean_wealth" %in% series)
      out$mean_wealth <- cbind(base, data.frame(
        measure = "Mean wealth", value = mean_v, lower = NA_real_, upper = NA_real_
      ))
    if ("median_wealth" %in% series)
      out$median_wealth <- cbind(base, data.frame(
        measure = "Median wealth", value = median_v, lower = NA_real_, upper = NA_real_
      ))
    out <- c(out, .extra_wealth_rows(base, series, mean_v, median_v,
                                     min(rec$sizes, na.rm = TRUE),
                                     max(rec$sizes, na.rm = TRUE)))

  } else {
    grid <- rec$inequality$grid
    if (is.null(grid)) return(NULL)
    row <- grid[grid$measure == measure & grid$graves == graves, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]
    # n of the grave subset actually plotted (e.g. adults only), not the
    # group's total number of graves
    if (!is.null(row$n) && !is.na(row$n)) base$n <- row$n

    if ("gini" %in% series)
      out$gini <- cbind(base, data.frame(
        measure = "Gini", value = row$gini, lower = row$lower, upper = row$upper
      ))
    if ("abs_gini" %in% series) {
      abs_lower <- NA_real_
      abs_upper <- NA_real_
      # Bias-simulation CI only applies to the "combined" measure on the
      # main subset the simulation actually ran on -- adults-only when age
      # separation was used, otherwise all graves (in which case "all" and
      # "adults" are the same subset).
      bsel <- .bias_for(rec, graves)
      bias_applicable <- !is.null(bsel) && measure == "combined"
      if (bias_applicable) {
        sim <- c(
          bsel$ginis_split_prestige_sqrt * bsel$means_split_prestige_sqrt,
          bsel$ginis_lump_prestige_sqrt  * bsel$means_lump_prestige_sqrt
        )
        bounds <- stats::quantile(sim, probs = c(0.10, 0.90), na.rm = TRUE)
        abs_lower <- unname(bounds[1])
        abs_upper <- unname(bounds[2])
      }
      out$abs_gini <- cbind(base, data.frame(
        measure = "Absolute Gini", value = row$abs_gini,
        lower = abs_lower, upper = abs_upper
      ))
    }
    if ("mean_wealth" %in% series)
      out$mean_wealth <- cbind(base, data.frame(
        measure = "Mean wealth", value = row$mean_wealth, lower = NA_real_, upper = NA_real_
      ))
    if ("median_wealth" %in% series)
      out$median_wealth <- cbind(base, data.frame(
        measure = "Median wealth", value = row$median_wealth, lower = NA_real_, upper = NA_real_
      ))
    out <- c(out, .extra_wealth_rows(base, series, row$mean_wealth, row$median_wealth,
                                     row$min_wealth %||% NA_real_,
                                     row$max_wealth %||% NA_real_))
  }

  if ("top_share" %in% series) {
    v <- .record_wealth(rec, graves, measure)
    if (!is.null(v) && length(v) > 0)
      out$top_share <- cbind(base, data.frame(
        measure = labs_ix$top, value = .top_share(v, top_pct),
        lower = NA_real_, upper = NA_real_))
  }
  if (any(c("bottom_share", "atkinson", "ede") %in% series)) {
    v <- .record_wealth(rec, graves, measure)
    if (!is.null(v) && length(v) > 0) {
      if ("bottom_share" %in% series)
        out$bottom_share <- cbind(base, data.frame(
          measure = labs_ix$bottom, value = .bottom_share(v, bottom_pct),
          lower = NA_real_, upper = NA_real_))
      if ("atkinson" %in% series)
        for (k in seq_along(atkinson_eps))
          out[[names(labs_ix$atk)[k]]] <- cbind(base, data.frame(
            measure = unname(labs_ix$atk[k]), value = .atkinson(v, atkinson_eps[k]),
            lower = NA_real_, upper = NA_real_))
      if ("ede" %in% series)
        for (k in seq_along(atkinson_eps))
          out[[names(labs_ix$ede)[k]]] <- cbind(base, data.frame(
            measure = unname(labs_ix$ede[k]), value = .ede(v, atkinson_eps[k]),
            lower = NA_real_, upper = NA_real_))
    }
  }

  if (length(out) == 0) return(NULL)

  # Bias-simulation IQR (split + lump draws pooled), for the optional ribbon.
  # Only where the simulation matches what is plotted: count data, same
  # measure, and the grave subset the simulation ran on.
  for (nm in names(out)) { out[[nm]]$bias_lo <- NA_real_; out[[nm]]$bias_hi <- NA_real_ }
  bsel <- if (!rec$is_physical) .bias_for(rec, graves) else NULL
  if (!is.null(bsel)) {
    {
      sfx   <- switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")
      g_sim <- c(bsel[[paste0("ginis_split_", sfx)]], bsel[[paste0("ginis_lump_", sfx)]])
      m_sim <- c(bsel[[paste0("means_split_", sfx)]], bsel[[paste0("means_lump_", sfx)]])
      iqr <- function(x) unname(stats::quantile(x, c(0.25, 0.75), na.rm = TRUE))
      if (!is.null(out$gini) && length(g_sim) > 0) {
        q <- iqr(g_sim); out$gini$bias_lo <- q[1]; out$gini$bias_hi <- q[2]
      }
      if (!is.null(out$abs_gini) && length(g_sim) == length(m_sim) && length(g_sim) > 0) {
        q <- iqr(g_sim * m_sim); out$abs_gini$bias_lo <- q[1]; out$abs_gini$bias_hi <- q[2]
      }
      # Wealth-level series: recomputed from the per-grave wealth of every
      # simulation (needs store_wealth = TRUE, the default).
      ws  <- bsel$wealth_scores
      col <- switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")
      if (!is.null(ws) && !is.null(col)) {
        sims <- Filter(Negate(is.null), c(ws$split, ws$lump))
        stat_fns <- list(
          mean_wealth       = function(x) mean(x),
          median_wealth     = function(x) stats::median(x),
          max_wealth        = function(x) max(x),
          min_wealth        = function(x) min(x),
          mean_median_diff  = function(x) mean(x) - stats::median(x),
          mean_median_ratio = function(x) {
            m <- stats::median(x); if (isTRUE(m != 0)) mean(x) / m else NA_real_ },
          top_share         = function(x) .top_share(x, top_pct),
          bottom_share      = function(x) .bottom_share(x, bottom_pct))
        for (k in seq_along(atkinson_eps)) local({
          e <- atkinson_eps[k]
          stat_fns[[paste0("atkinson_", e)]] <<- function(x) .atkinson(x, e)
          stat_fns[[paste0("ede_", e)]]      <<- function(x) .ede(x, e)
        })
        for (nm in intersect(names(stat_fns), names(out))) {
          v <- vapply(sims, function(s) stat_fns[[nm]](s[[col]][!is.na(s[[col]])]),
                      numeric(1))
          if (length(v) > 0 && any(!is.na(v))) {
            q <- iqr(v); out[[nm]]$bias_lo <- q[1]; out[[nm]]$bias_hi <- q[2]
          }
        }
      }
    }
  }
  do.call(rbind, out)
}



#' Rows for the mean-median difference/ratio and min/max wealth series
#'
#' `mean_median_diff` = mean - median (in wealth units); `mean_median_ratio`
#' = mean / median (unit-free, so comparable across datasets whose wealth
#' scales differ); NA if the median is 0. Min/max need a pipeline result
#' made with a package version whose `inequality_summary()` grid stores
#' them; older results give NA.
#' @noRd
.extra_wealth_rows <- function(base, series, mean_v, median_v, min_v, max_v) {
  mk <- function(label, v) cbind(base, data.frame(measure = label, value = v,
                                                  lower = NA_real_, upper = NA_real_))
  out <- list()
  if ("mean_median_diff" %in% series)
    out$mean_median_diff <- mk("Mean - median", mean_v - median_v)
  if ("mean_median_ratio" %in% series)
    out$mean_median_ratio <- mk("Mean / median",
                                if (isTRUE(median_v != 0)) mean_v / median_v else NA_real_)
  if ("max_wealth" %in% series) out$max_wealth <- mk("Max wealth", max_v)
  if ("min_wealth" %in% series) out$min_wealth <- mk("Min wealth", min_v)
  out
}

#' Refuse to mix count-data and physical-measure cases in one comparison
#'
#' Count-data wealth (a prestige + sqrt-count score, no inherent unit) and
#' physical-measure wealth (m2, m3, cm, ...) are not the same kind of
#' quantity -- their Absolute Gini/mean/median values live on genuinely
#' different, incommensurable scales. Earlier versions of this package let
#' [plot_gini_comparison()] plot both at once (faceted, with a shared
#' rescaled secondary axis for Absolute Gini etc.), but that shared axis is
#' driven by whichever type happens to have the larger magnitude, visually
#' squashing the other -- a scale distortion, not just an aesthetic
#' nitpick. There is no general fix for this in plain ggplot2 (a single
#' secondary axis can't carry two different unit systems), so mixing is no
#' longer supported here: call [plot_gini_comparison()]/[plot_gini_timeline()]
#' once per data type and combine the two images yourselves if you want them
#' side by side.
#'
#' @param records Output of `.gather_comparison_records()`.
#' @noRd
.assert_single_data_type <- function(records) {
  is_phys <- vapply(records, function(r) isTRUE(r$is_physical), logical(1))
  if (any(is_phys) && any(!is_phys))
    stop("This pipeline_result mixes count-data and physical-measure cases. ",
         "Their Absolute Gini/wealth values are on different, incommensurable ",
         "scales (a prestige+count score vs. a physical measure like m2/m3/cm), ",
         "so plotting them together would distort one or the other on a shared ",
         "axis. Call this function once per data type instead, e.g. ",
         "plot_gini_comparison(count_result) and plot_gini_comparison(phys_result) ",
         "separately, and combine the two images afterwards if you want them ",
         "side by side.")
}


#' Compare wealth-inequality statistics across cases
#'
#' Takes the output of [count_pipeline()] **or** [phys_pipeline()] (not a
#' mix of both -- see `.assert_single_data_type()`) and
#' produces a point + line (+ error bar, where available) plot comparing
#' one or more statistics across cases, with cases on the x-axis.
#'
#' @param pipeline_result A [count_pipeline()] or [phys_pipeline()] result,
#'   **or** a list of several such results of the **same** data type -- e.g.
#'   `list(SiteA = res_a, SiteB = res_b)` for two [count_pipeline()] calls on
#'   datasets whose columns didn't fit one [count_pipeline()] call. Mixing a
#'   count-data result with a physical-measure result (in one call, or
#'   across two calls passed in a list) raises an error -- their Absolute
#'   Gini/wealth values are on different, incommensurable scales, so call
#'   this function once per data type and combine the images yourself
#'   instead. When the data spans more than one dataset -- whether because a
#'   single pipeline call was itself given multiple datasets (see `data` in
#'   [count_pipeline()]), or because a list of several results is passed
#'   here, or both -- the plot is split into one facet panel per dataset,
#'   each with its own x-axis (`scales = "free_x"`), since different
#'   datasets may have entirely different group sets.
#' @param series Character vector selecting which statistics to plot, any
#'   subset of `c("gini", "abs_gini", "mean_wealth", "median_wealth")`.
#'   Default `c("gini", "abs_gini")` (the previous default behaviour).
#'   Also available: `"mean_median_diff"` (mean - median wealth, in wealth
#'   units; how far a few rich graves pull the mean above the typical grave),
#'   `"mean_median_ratio"` (mean / median; the same skew but unit-free, so
#'   it can be compared between datasets whose wealth scales differ),
#'   `"max_wealth"` and `"min_wealth"`, and `"top_share"`: the share of
#'   total wealth held by the richest `top_pct` percent of graves (drawn on
#'   the 0-1 left axis next to the Gini). A warning is given for cases where
#'   that top group is fewer than 3 graves (n * top_pct / 100 < 3).
#'   `"bottom_share"`: the share held by the poorest `bottom_pct` percent
#'   (default 50, the poorer half). `"atkinson"`: Atkinson index for each
#'   value in `atkinson_eps` (default 0.5, 1, 2); the larger epsilon, the
#'   more the index reacts to the poor end. Shares and Atkinson indices run
#'   0-1 and go on the left axis with the Gini. Using a top, a bottom and a
#'   summary measure together shows whether a ranking of cases holds across
#'   the whole distribution -- needed when Lorenz curves cross (see
#'   [plot_lorenz_comparison()]).
#'   `"ede"`: equally distributed equivalent wealth, mean x (1 - Atkinson)
#'   for each value in `atkinson_eps` -- the wealth per grave that, shared
#'   equally, would count as much as the actual distribution. It is in
#'   wealth units (right axis) and combines level and inequality: a rich
#'   but very unequal case can come out lower than a poorer, more equal
#'   one. Only comparable within a dataset (same wealth scale).
#'   `"abs_gini"` is the Absolute Gini (G x mean wealth); `"mean_wealth"`/
#'   `"median_wealth"` plot the mean/median wealth (or physical measure)
#'   value per case as their own point+line series.
#' @param graves For count-data cases: which age group's statistics to
#'   plot -- `"adults"` (default), `"all"` (all graves pooled), or
#'   `"subadults"`. Pulled from the `grid` element of [inequality_summary()]
#'   (computed inside [count_pipeline()]), so this requires no extra
#'   computation -- it was already there. Ignored for physical-measure
#'   cases (no age concept). `"subadults"` will be `NA` for any case where
#'   age-separation columns weren't present in the source data, or fewer
#'   than 2 subadult rows existed -- this prints as a gap in the line, not
#'   an error.
#' @param measure For count-data cases: which Gini measure to use --
#'   `"combined"` (default, prestige + sqrt-count bonus), `"prestige"`, or
#'   `"tot"`. Ignored for physical-measure cases.
#' @param case_order Optional character vector giving the left-to-right
#'   order of cases by their `group` name (e.g. `c("Early", "Middle",
#'   "Late")`), applied within every facet. If `NULL` (default), cases keep
#'   the order returned by the pipeline. Cases omitted from `case_order` are
#'   appended at the end.
#' @param title Optional plot title. For count-data cases, a note on which
#'   age group is plotted ("(adults)", "(all ages)",
#'   "(subadults)", from `graves` above) is appended to this -- it is not a
#'   default that disappears once you set your own title. No note is added
#'   when every case is a physical-measure case.
#' @param show_n Logical. If TRUE (default), each x-axis tick is labelled
#'   `"<group>\n(n=<n>)"` (two lines), showing the sample size behind each case.
#' @param top_pct Percentage for `series = "top_share"`, e.g. 10 (default)
#'   for the top 10% or 1 for the top 1%. The top group is n * top_pct / 100
#'   graves; a fractional grave counts with its fraction. Needs n >= 100 /
#'   top_pct to cover even one whole grave; below about 3 graves the value
#'   is decided by one or two individual graves and is very unstable (a
#'   warning lists such cases).
#' @param bottom_pct Percentage for `series = "bottom_share"` (default 50).
#' @param atkinson_eps Inequality-aversion values for `series = "atkinson"`,
#'   one series per value (default `c(0.5, 1, 2)`).
#' @param scales Axis sharing between dataset panels, passed to
#'   [ggplot2::facet_wrap()]: `"free_x"` (default; shared y-axis) or
#'   `"free"` (each dataset its own y-axis). Use `"free"` for wealth-level
#'   series (mean, median, max, equal-equivalent wealth...) when the datasets'
#'   wealth scales are not comparable, so neither dataset is squeezed and no
#'   level comparison is suggested. Not possible when 0-1 series (Gini,
#'   shares, Atkinson) and wealth-level series are combined on two axes,
#'   since the right axis is one transformation for all panels; the y-axis
#'   then stays shared (with a message).
#' @param show_ci Logical. If TRUE (default), draws the error bars: the
#'   bootstrap confidence interval for Gini (at the pipeline's `conf_level`),
#'   and for Absolute Gini the 10-90% range of the bias simulations. Set
#'   FALSE to hide them, e.g. when showing `bias_band` instead.
#' @param bias_band Logical. If TRUE, draws the interquartile range (middle
#'   50%) of the category-bias simulations (split and lump runs pooled) as a
#'   shaded ribbon behind each line, for every series. Gini and Absolute Gini
#'   need `run_bias_sim = TRUE` in [count_pipeline()]; the wealth series (mean,
#'   median, max, min, mean - median, mean / median) also need
#'   `store_wealth = TRUE` (the default); only drawn where the
#'   simulation matches the plotted `measure` and the grave subset it ran on
#'   (adults when age separation was used, otherwise all graves). A series
#'   with a single case gets no ribbon (a ribbon needs two points). Default
#'   FALSE.
#' @details
#' When `"gini"` is among `series`, the left axis is Gini's natural \[0, 1\]
#' scale. Any other selected series (`abs_gini`, `mean_wealth`,
#' `median_wealth`) is unbounded, so it's drawn on a second, rescaled axis --
#' the standard ggplot2 dual-axis trick (there's really only one underlying
#' drawing scale; the right-hand labels are relabelled back to original
#' units via [ggplot2::sec_axis()]). When **more than one** unbounded series
#' is selected at once (e.g. both `abs_gini` and `mean_wealth`), they are
#' pooled onto *one shared* rescaled axis using their combined range -- the
#' right-hand axis label then names all of them, but only one of them can be
#' read exactly off that axis at a glance; treat the right-hand axis as
#' approximate in that case, or call this function once per series instead.
#' If `"gini"` is not in `series` at all, no rescaling happens -- all
#' selected series share one plain axis in their own units, which is exact
#' but can look odd if their magnitudes differ a lot (e.g. mean wealth and
#' absolute Gini).
#'
#' Being a normal [ggplot2::ggplot] object (not a pre-rendered image),
#' every element this function sets -- title, axis labels, colours, theme --
#' can be overridden by the caller afterwards using standard ggplot2 syntax,
#' e.g. `plot_gini_comparison(result) + ggplot2::labs(title = "My title")`
#' or `+ ggplot2::theme(legend.position = "right")`. Adding a
#' `labs()`/`scale_*()`/`theme()` call after the function call replaces only
#' the component(s) named in that call; everything else is left as-is.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' plot_gini_comparison(res)
#' plot_gini_comparison(res, series = c("gini", "top_share", "bottom_share"),
#'                      top_pct = 10, bottom_pct = 50)
#' plot_gini_comparison(res, series = "gini", bias_band = TRUE)
#' plot_gini_comparison(res, graves = "subadults")   # subadult graves only
#' @export
plot_gini_comparison <- function(pipeline_result,
                                  series     = c("gini", "abs_gini"),
                                  graves     = c("adults", "all", "subadults"),
                                  measure    = c("combined", "prestige", "tot"),
                                  case_order = NULL,
                                  title      = NULL,
                                  show_n     = TRUE,
                                  bias_band  = FALSE,
                                  show_ci    = TRUE,
                                  top_pct    = 10,
                                  bottom_pct = 50,
                                  atkinson_eps = c(0.5, 1, 2),
                                  scales     = c("free_x", "free")) {

  graves  <- match.arg(graves)
  scales  <- match.arg(scales)
  measure <- match.arg(measure)
  series  <- unique(series)

  valid_series <- c("gini", "abs_gini", "mean_wealth", "median_wealth",
                    "mean_median_diff", "mean_median_ratio", "max_wealth", "min_wealth",
                    "top_share", "bottom_share", "atkinson", "ede")
  .check_index_args(series, top_pct, bottom_pct, atkinson_eps)
  labs_ix   <- .index_labels(top_pct, bottom_pct, atkinson_eps)
  top_label <- labs_ix$top
  bad <- setdiff(series, valid_series)
  if (length(bad) > 0)
    stop("Unknown series: ", paste(bad, collapse = ", "),
         ". Must be one or more of: ", paste(valid_series, collapse = ", "))
  if (length(series) == 0)
    stop("'series' must include at least one of: ", paste(valid_series, collapse = ", "))

  records <- .gather_comparison_records(pipeline_result)
  if (length(records) == 0) stop("No cases found in pipeline_result.")
  .assert_single_data_type(records)

  row_list <- lapply(records, .comparison_value_rows,
                      series = series, graves = graves, measure = measure,
                      top_pct = top_pct, bottom_pct = bottom_pct,
                      atkinson_eps = atkinson_eps)
  row_list <- Filter(Negate(is.null), row_list)
  if (length(row_list) == 0)
    stop("No data available for the requested series/graves/measure combination.")

  long_df <- do.call(rbind, row_list)
  if (!show_ci) long_df$lower <- long_df$upper <- NA_real_   # hide error bars
  .warn_top_share(long_df, top_pct, top_label)
  .warn_top_share(long_df, bottom_pct, labs_ix$bottom)

  # Title age-group note: only meaningful if at least one case is count data.
  any_count <- any(!vapply(records, function(r) r$is_physical, logical(1)))
  age_label <- if (any_count) {
    .age_label(graves)
  } else ""

  # Resolve case order by group name, then build the n-annotated x-axis label
  # while preserving that order (ties broken by dataset so each facet's own
  # group order is stable).
  group_levels   <- .order_cases(long_df$group, case_order)
  long_df$group  <- factor(long_df$group, levels = group_levels)
  long_df        <- long_df[order(long_df$group), , drop = FALSE]
  long_df$case_label <- if (show_n) {
    sprintf("%s\n(n=%d)", as.character(long_df$group), long_df$n)
  } else {
    as.character(long_df$group)
  }
  long_df$case_label <- factor(long_df$case_label, levels = unique(long_df$case_label))

  if (!"dataset" %in% names(long_df) || all(is.na(long_df$dataset))) {
    long_df$dataset <- "all"
    multi_dataset <- FALSE
  } else {
    long_df$dataset[is.na(long_df$dataset)] <- "(untagged)"
    multi_dataset <- length(unique(long_df$dataset)) > 1
  }

  # Fixed series order/colours/shapes -- not alphabetical, so the legend
  # reads Gini, Absolute Gini, Mean wealth, Median wealth regardless of how
  # `series` was specified.
  measure_levels_all  <- c("Gini", "Absolute Gini", "Mean wealth", "Median wealth",
                            "Mean - median", "Mean / median", "Max wealth", "Min wealth")
  measure_colours_all <- c("Gini" = "#2ca25f", "Absolute Gini" = "#d6604d",
                            "Mean wealth" = "#3182bd", "Median wealth" = "#e6550d",
                            "Mean - median" = "#756bb1", "Mean / median" = "#54278f",
                            "Max wealth" = "#8c510a", "Min wealth" = "#01665e")
  measure_shapes_all  <- c("Gini" = 16, "Absolute Gini" = 18,
                            "Mean wealth" = 17, "Median wealth" = 15,
                            "Mean - median" = 8, "Mean / median" = 4,
                            "Max wealth" = 2, "Min wealth" = 6)
  # Top-x% share: label depends on top_pct; bounded 0-1, so it shares the
  # left (Gini) axis instead of the rescaled right axis.
  idx_labels <- c(top_label, labs_ix$bottom, unname(labs_ix$atk))
  measure_levels_all  <- c(measure_levels_all[1], idx_labels, measure_levels_all[-1])
  measure_colours_all[top_label]      <- "#c51b8a"
  measure_shapes_all[top_label]       <- 1
  measure_colours_all[labs_ix$bottom] <- "#66a61e"
  measure_shapes_all[labs_ix$bottom]  <- 0
  atk_cols <- grDevices::colorRampPalette(c("#014636", "#3690c0"))(max(2, length(labs_ix$atk)))
  measure_colours_all[unname(labs_ix$atk)] <- atk_cols[seq_along(labs_ix$atk)]
  measure_shapes_all[unname(labs_ix$atk)]  <- c(3, 7, 9, 10, 12, 13)[(seq_along(labs_ix$atk) - 1) %% 6 + 1]
  # Equal-equivalent wealth: unbounded (wealth units), so right axis
  measure_levels_all <- c(measure_levels_all, unname(labs_ix$ede))
  ede_cols <- grDevices::colorRampPalette(c("#7f2704", "#fd8d3c"))(max(2, length(labs_ix$ede)))
  measure_colours_all[unname(labs_ix$ede)] <- ede_cols[seq_along(labs_ix$ede)]
  measure_shapes_all[unname(labs_ix$ede)]  <- c(25, 24, 23, 22, 21, 14)[(seq_along(labs_ix$ede) - 1) %% 6 + 1]

  present <- intersect(measure_levels_all, unique(long_df$measure))
  long_df$measure <- factor(long_df$measure, levels = present)

  bounded       <- intersect(c("Gini", idx_labels), present)
  has_gini      <- length(bounded) > 0           # any series on the 0-1 left axis
  unbounded     <- setdiff(present, bounded)
  left_name     <- if (identical(bounded, "Gini")) "Gini coefficient" else
                     if (length(bounded) == 1) paste(bounded, "(0-1)") else
                     "Gini / shares / Atkinson (0-1)"
  has_unbounded <- length(unbounded) > 0

  if (has_gini && has_unbounded) {
    sel <- long_df$measure %in% unbounded
    rng <- range(c(long_df$value[sel], long_df$lower[sel], long_df$upper[sel],
                   if (bias_band) c(long_df$bias_lo[sel], long_df$bias_hi[sel])),
                 na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) rng <- c(0, 1)
    to_unit <- function(x) (x - rng[1]) / diff(rng)
    to_orig <- function(x) x * diff(rng) + rng[1]

    long_df$value[sel] <- to_unit(long_df$value[sel])
    long_df$lower[sel] <- to_unit(long_df$lower[sel])
    long_df$upper[sel] <- to_unit(long_df$upper[sel])
    long_df$bias_lo[sel] <- to_unit(long_df$bias_lo[sel])
    long_df$bias_hi[sel] <- to_unit(long_df$bias_hi[sel])

    right_name <- if (length(unbounded) == 1) {
      switch(unbounded,
             "Absolute Gini" = "Absolute Gini (G x mean wealth)",
             unbounded)
    } else {
      paste0(paste(unbounded, collapse = " / "), " (shared rescaled axis -- approximate)")
    }
    y_scale <- ggplot2::scale_y_continuous(
      name     = left_name,
      sec.axis = ggplot2::sec_axis(~ to_orig(.), name = right_name)
    )
  } else if (has_gini) {
    y_scale <- ggplot2::scale_y_continuous(name = left_name)
  } else {
    y_name <- paste(present, collapse = " / ")
    if (length(present) > 1 && nchar(y_name) > 35) {
      unitless <- "Mean / median"
      y_name <- if (!any(present %in% unitless)) "Wealth per grave (see legend)" else
        "Value (see legend)"
    }
    y_scale <- ggplot2::scale_y_continuous(name = y_name)
  }

  p <- ggplot2::ggplot(long_df,
                        ggplot2::aes(x = case_label, y = value, colour = measure,
                                     shape = measure, group = measure)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title  = paste0(title %||% "Comparison across cases", age_label),
      x      = NULL,
      colour = NULL,
      shape  = NULL
    ) +
    ggplot2::scale_colour_manual(values = measure_colours_all[present], breaks = present) +
    ggplot2::scale_shape_manual(values = measure_shapes_all[present], breaks = present) +
    y_scale

  has_ci <- !all(is.na(long_df$lower))
  if (has_ci) {
    p <- p + ggplot2::geom_errorbar(
      data  = long_df[!is.na(long_df$lower), ],
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.3, linewidth = 0.7
    )
  }

  # Colour each axis title to match the series it belongs to, when there's
  # a dual axis to disambiguate.
  if (has_gini && has_unbounded) {
    right_colour <- if (length(unbounded) == 1) {
      measure_colours_all[[unbounded]]
    } else {
      "grey30"
    }
    p <- p + ggplot2::theme(
      axis.title.y      = ggplot2::element_text(colour = if (length(bounded) == 1)
                                                      measure_colours_all[[bounded]] else "grey30"),
      axis.title.y.right = ggplot2::element_text(colour = right_colour)
    )
  }

  if (bias_band) {
    band_df <- long_df[!is.na(long_df$bias_lo), , drop = FALSE]
    if (nrow(band_df) == 0) {
      message("bias_band = TRUE, but no bias simulation results match this plot ",
              "(run count_pipeline(..., run_bias_sim = TRUE); only the simulated ",
              "measure and grave subset get a band).")
    } else {
      band <- ggplot2::geom_ribbon(
        data = band_df,
        ggplot2::aes(x = case_label, ymin = bias_lo, ymax = bias_hi, fill = measure,
                     group = interaction(measure, dataset)),
        alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
      )
      p$layers <- c(band, p$layers)                      # draw behind lines/points
      p <- p + ggplot2::scale_fill_manual(values = measure_colours_all[present]) +
        ggplot2::labs(caption = "Shaded band: IQR of category-bias simulations (split + lump)")
    }
  }

  if (multi_dataset) {
    if (scales == "free" && has_gini && has_unbounded) {
      message("scales = \"free\" is not possible with a second (rescaled) y-axis; ",
              "the y-axis stays shared. Plot the wealth-level series without the ",
              "0-1 series to give each dataset its own axis.")
      scales <- "free_x"
    }
    p <- p + ggplot2::facet_wrap(~ dataset, scales = scales)
  }

  # Gini's [0,1] bound is conceptual, not guaranteed by a bootstrap CI in
  # every edge case -- coord_cartesian() clips the *view* at the panel edges
  # without dropping any geom_errorbar() row, unlike scale_y_continuous(limits=).
  # Only meaningful when Gini is actually being drawn on this axis.
  if (has_gini) p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))

  p
}


#' Compare wealth-inequality statistics across cases on an absolute timeline
#'
#' A companion to [plot_gini_comparison()] for when cases should share one
#' continuous, absolute-date x-axis instead of being arranged as discrete,
#' evenly-spaced categories -- e.g. comparing Corded Ware Culture (phased
#' Early/Middle/Late) against a later Unetice culture (also phased
#' Early/Middle/Late) on the same timeline, using each phase's own
#' calendar date rather than treating "phase" as a bare category.
#'
#' **Dates.** This requires every case to carry a date, which is *not*
#' computed here -- it has to already be in the `pipeline_result`, via the
#' `date_col` argument of [count_pipeline()]/[phys_pipeline()]. If you
#' didn't pass `date_col` when building the pipeline result, every case's
#' date is missing and this function stops with an error; use
#' [plot_gini_comparison()] instead, which doesn't need dates.
#'
#' **How cases are grouped into series (colour).** Two ways, used
#' automatically depending on how `pipeline_result` was built -- there is
#' no separate argument for this:
#' \itemize{
#'   \item If `pipeline_result` came from a multi-dataset call (a named
#'     list passed to [count_pipeline()]/[phys_pipeline()], or a named
#'     list of pipeline results passed to this function), the dataset name
#'     itself is the series -- e.g. `list(CWC = cwc_df, Unetice = un_df)`
#'     needs no special naming convention inside the data.
#'   \item Otherwise (a single dataset, no outer list), the series is
#'     parsed from each case's group label by splitting at the first
#'     occurrence of `series_sep` (default `"_"`) and keeping the part
#'     before it -- so group labels `"CWC_early"`, `"CWC_middle"`,
#'     `"Unetice_early"` and `"Unetice_late"` in a single dataset's
#'     `group_col` parse into series `"CWC"` and `"Unetice"`. A group label
#'     with no `series_sep` in it becomes its own singleton series (no
#'     error, just no grouping benefit for that case). Because this is a
#'     plain string split, a culture name that itself contains an
#'     underscore (e.g. `"Bell_Beaker_early"`) will parse wrongly (series
#'     `"Bell"`, not `"Bell_Beaker"`) -- pick a `series_sep` that doesn't
#'     collide with your own names, or avoid underscores inside a culture
#'     name.
#' }
#' @param pipeline_result A [count_pipeline()]/[phys_pipeline()] result (or
#'   a named list of such results), built with `date_col` set. See
#'   [plot_gini_comparison()] for the general shape this accepts.
#' @param series One or more statistics to plot: `"gini"` (default),
#'   `"abs_gini"`, `"mean_wealth"`, `"median_wealth"`, `"mean_median_diff"`,
#'   `"mean_median_ratio"`, `"max_wealth"`, `"min_wealth"`, `"top_share"`
#'   (with `top_pct`), `"bottom_share"` (with `bottom_pct`), `"atkinson"`
#'   (with `atkinson_eps`; one line per value), `"ede"` (equal-equivalent
#'   wealth, with `atkinson_eps`); see [plot_gini_comparison()]. With more than one,
#'   `layout` decides how they are combined in the one figure.
#' @param panel_groups Optional named list that puts several statistics in
#'   one panel, e.g. `list("Wealth per grave" = c("median_wealth",
#'   "mean_wealth"), "Wealth shares" = c("bottom_share", "top_share"))`.
#'   Names become the panel titles; values are entries of `series`.
#'   Statistics in a panel share its y-axis and are told apart by line type
#'   and point shape (colour still shows the dataset/culture). Statistics in
#'   `series` but in no group get a panel of their own. Group only
#'   statistics with the same unit: wealth levels with wealth levels, shares
#'   and indices (0-1) with each other. Used with `layout = "panels"`.
#' @param layout Only used with more than one `series`. `"panels"`
#'   (default): one panel per statistic, stacked, sharing the date axis,
#'   each with its own y-axis (Gini and top share fixed at 0-1) -- still one
#'   figure (panels a, b, ...). `"dual_axis"`: all statistics in one panel;
#'   the first `series` uses the left axis, all others are rescaled together
#'   onto it and read off the right axis; line type and point shape
#'   distinguish the statistics, colour the cultures/datasets. With a dual
#'   axis the rescaling decides where lines cross, so crossings and relative
#'   heights between the two sides carry no meaning (said in the caption).
#' @param graves For count-data cases: `"adults"` (default), `"all"`, or
#'   `"subadults"`. Ignored for physical-measure cases. See
#'   [plot_gini_comparison()].
#' @param measure For count-data cases: `"combined"` (default),
#'   `"prestige"`, or `"tot"`. Ignored for physical-measure cases. See
#'   [plot_gini_comparison()].
#' @param mode `"separate"` (default) draws one coloured line per series,
#'   each connecting only that series' own cases in date order -- valid
#'   whether or not series overlap in time. `"combined"` draws **one**
#'   line through *every* case pooled together in date order (points are
#'   still coloured by series, for reference), representing a single
#'   continuous trend across cultures/periods -- this only makes sense
#'   when the series' date ranges don't overlap (e.g. CWC ends before
#'   Unetice begins). If they do overlap, the single line has to revisit
#'   the same stretch of time twice and will zig-zag; a warning is issued
#'   in that case, but the plot is still drawn (use `mode = "separate"`
#'   instead if that's not what you want).
#' @param series_sep Separator used to parse the series from a group label
#'   when `pipeline_result` is a single, non-list dataset (see above).
#'   Default `"_"`. Ignored when the series is taken from a dataset name
#'   instead.
#' @param title Optional plot title.
#' @param top_pct Percentage for `series = "top_share"`, e.g. 10 (default)
#'   for the top 10% or 1 for the top 1%. The top group is n * top_pct / 100
#'   graves; a fractional grave counts with its fraction. Needs n >= 100 /
#'   top_pct to cover even one whole grave; below about 3 graves the value
#'   is decided by one or two individual graves and is very unstable (a
#'   warning lists such cases).
#' @param bottom_pct Percentage for `series = "bottom_share"` (default 50).
#' @param atkinson_eps Inequality-aversion values for `series = "atkinson"`,
#'   one series per value (default `c(0.5, 1, 2)`).
#' @param show_ci Logical. If TRUE (default), draws the error bars: the
#'   bootstrap confidence interval for Gini (at the pipeline's `conf_level`),
#'   and for Absolute Gini the 10-90% range of the bias simulations. Set
#'   FALSE to hide them, e.g. when showing `bias_band` instead.
#' @param bounded_axis `"fixed"` (default): bounded statistics (Gini, top
#'   and bottom share, Atkinson) are drawn on their full 0-1 range, so
#'   heights can be compared directly and small differences are not
#'   exaggerated. `"free"`: their y-axis is zoomed to the data, which makes
#'   small differences visible; the axis then does not start at 0, and a
#'   note saying so is added to the caption. With `layout = "dual_axis"`
#'   only the left (first) series is affected.
#' @param bias_band Logical. If TRUE, draws the interquartile range (middle
#'   50%) of the category-bias simulations (split and lump runs pooled) as a
#'   shaded ribbon behind each line, for every series. Gini and Absolute Gini
#'   need `run_bias_sim = TRUE` in [count_pipeline()]; the wealth series (mean,
#'   median, max, min, mean - median, mean / median) also need
#'   `store_wealth = TRUE` (the default); only drawn where the
#'   simulation matches the plotted `measure` and the grave subset it ran on
#'   (adults when age separation was used, otherwise all graves). A series
#'   with a single case gets no ribbon (a ribbon needs two points). Default
#'   FALSE.
#' @return A [ggplot2::ggplot] object, with calendar dates formatted as
#'   "<n> BCE"/"<n> CE" on the x-axis.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' # Gini over time, one line per culture, with the bias-simulation band
#' plot_gini_timeline(res, bias_band = TRUE)
#'
#' # Several statistics, one panel each; zoom the 0-1 axes to the data
#' plot_gini_timeline(res, series = c("gini", "top_share", "median_wealth", "mean_wealth"),
#'                    bias_band = TRUE, bounded_axis = "free")
#'
#' # All statistics in one panel with a second axis
#' plot_gini_timeline(res, series = c("gini", "median_wealth"), layout = "dual_axis")
#' @export
plot_gini_timeline <- function(pipeline_result,
                                series     = c("gini", "abs_gini", "mean_wealth", "median_wealth",
                                               "mean_median_diff", "mean_median_ratio",
                                               "max_wealth", "min_wealth", "top_share",
                                               "bottom_share", "atkinson", "ede"),
                                graves     = c("adults", "all", "subadults"),
                                measure    = c("combined", "prestige", "tot"),
                                mode       = c("separate", "combined"),
                                series_sep = "_",
                                title      = NULL,
                                bias_band  = FALSE,
                                show_ci    = TRUE,
                                top_pct    = 10,
                                layout     = c("panels", "dual_axis"),
                                bottom_pct = 50,
                                atkinson_eps = c(0.5, 1, 2),
                                bounded_axis = c("fixed", "free"),
                                panel_groups = NULL) {

  if (missing(series)) series <- "gini"
  series  <- unique(match.arg(series, several.ok = TRUE))
  layout  <- match.arg(layout)
  bounded_axis <- match.arg(bounded_axis)
  .check_index_args(series, top_pct, bottom_pct, atkinson_eps)
  labs_ix <- .index_labels(top_pct, bottom_pct, atkinson_eps)
  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  mode    <- match.arg(mode)

  records <- .gather_comparison_records(pipeline_result)
  if (length(records) == 0) stop("No cases found in pipeline_result.")
  .assert_single_data_type(records)

  row_list <- lapply(records, .comparison_value_rows,
                      series = series, graves = graves, measure = measure,
                      top_pct = top_pct, bottom_pct = bottom_pct,
                      atkinson_eps = atkinson_eps)
  row_list <- Filter(Negate(is.null), row_list)
  if (length(row_list) == 0)
    stop("No data available for the requested series/graves/measure combination.")

  long_df <- do.call(rbind, row_list)
  if (!show_ci) long_df$lower <- long_df$upper <- NA_real_   # hide error bars
  .warn_top_share(long_df, top_pct, labs_ix$top)
  .warn_top_share(long_df, bottom_pct, labs_ix$bottom)
  multi_series <- length(unique(as.character(long_df$measure))) > 1

  if (!"date" %in% names(long_df) || all(is.na(long_df$date)))
    stop("plot_gini_timeline() requires a date for every case. Re-run ",
         "count_pipeline()/phys_pipeline() with `date_col` set to a numeric ",
         "date column (negative = BCE, positive = CE), then try again. ",
         "If you don't have dates, use plot_gini_comparison() instead.")

  missing_dates <- is.na(long_df$date)
  if (any(missing_dates)) {
    warning("Dropping cases with no date: ",
            paste(unique(long_df$case[missing_dates]), collapse = ", "))
    long_df <- long_df[!missing_dates, , drop = FALSE]
  }
  if (nrow(long_df) == 0) stop("No cases with a date remain to plot.")

  # Series/colour grouping: dataset name when pipeline_result actually spans
  # more than one distinct dataset (the same test plot_gini_comparison()
  # uses for faceting) -- a single, non-list dataset always carries one
  # placeholder dataset value internally, never NA, so NA-ness alone can't
  # be used to detect "no real dataset tagging" here. Otherwise, the series
  # is parsed from the group label by splitting at series_sep.
  parse_prefix <- function(x, sep) {
    parts <- strsplit(x, sep, fixed = TRUE)[[1]]
    if (length(parts) < 2) x else parts[1]
  }
  uses_dataset_as_series <- "dataset" %in% names(long_df) &&
    length(unique(long_df$dataset[!is.na(long_df$dataset)])) > 1
  long_df$series_group <- if (uses_dataset_as_series) {
    long_df$dataset
  } else {
    vapply(long_df$group, parse_prefix, character(1), sep = series_sep)
  }

  series_groups <- unique(long_df$series_group)
  if (mode == "combined" && length(series_groups) > 1) {
    ranges <- lapply(series_groups, function(sg) range(long_df$date[long_df$series_group == sg]))
    overlap_found <- FALSE
    for (i in seq_along(ranges)) {
      for (j in seq_along(ranges)) {
        if (i < j && ranges[[i]][1] <= ranges[[j]][2] && ranges[[j]][1] <= ranges[[i]][2]) {
          overlap_found <- TRUE
        }
      }
    }
    if (overlap_found)
      warning("mode = \"combined\" draws one line through all cases sorted by date, ",
              "but the date ranges of ", paste(series_groups, collapse = ", "),
              " overlap -- the line will zig-zag back and forth in time where they do. ",
              "Consider mode = \"separate\" instead.")
  }

  date_fmt <- function(x) ifelse(x < 0, paste0(abs(round(x)), " BCE"), paste0(round(x), " CE"))

  # ---- Several statistics: panels or dual axis --------------------------
  bounded_labels <- c("Gini", labs_ix$top, labs_ix$bottom, unname(labs_ix$atk))
  series_labels <- list(gini = "Gini", abs_gini = "Absolute Gini",
                     mean_wealth = "Mean wealth", median_wealth = "Median wealth",
                     mean_median_diff = "Mean - median", mean_median_ratio = "Mean / median",
                     max_wealth = "Max wealth", min_wealth = "Min wealth",
                     top_share = labs_ix$top, bottom_share = labs_ix$bottom,
                     atkinson = unname(labs_ix$atk), ede = unname(labs_ix$ede))
  lv <- intersect(unlist(series_labels[series], use.names = FALSE),
                  unique(as.character(long_df$measure)))
  lv <- c(lv, setdiff(unique(as.character(long_df$measure)), lv))   # order as in `series`
  long_df$measure <- factor(long_df$measure, levels = lv)
  dual <- multi_series && layout == "dual_axis" && length(lv) > 1
  panels <- multi_series && !dual
  dual_note <- NULL

  # Optional grouping of several statistics into one panel
  grouped <- panels && !is.null(panel_groups)
  if (!is.null(panel_groups) && !panels)
    message("panel_groups is only used with layout = \"panels\" and more than one series.")
  if (grouped) {
    if (!is.list(panel_groups) || is.null(names(panel_groups)) || any(names(panel_groups) == ""))
      stop("panel_groups must be a named list, e.g. list(Wealth = c(\"median_wealth\", \"mean_wealth\")).")
    bad <- setdiff(unlist(panel_groups), names(series_labels))
    if (length(bad)) stop("Unknown statistic(s) in panel_groups: ", paste(bad, collapse = ", "))
    lab2panel <- character(0)
    for (pn in names(panel_groups))
      for (key in panel_groups[[pn]]) {
        if (!key %in% series) next
        for (lb in series_labels[[key]]) lab2panel[lb] <- pn
      }
    rest <- setdiff(lv, names(lab2panel))
    lab2panel[rest] <- rest
    pan_lv <- unique(unname(lab2panel[lv]))
    long_df$panel <- factor(unname(lab2panel[as.character(long_df$measure)]), levels = pan_lv)
    mixed <- vapply(pan_lv, function(pn) {
      m <- names(lab2panel)[lab2panel == pn]
      length(unique(m %in% bounded_labels)) > 1 }, logical(1))
    if (any(mixed))
      warning("Panel(s) ", paste(pan_lv[mixed], collapse = ", "), " mix 0-1 statistics with ",
              "wealth levels on one axis; group statistics with the same unit.", call. = FALSE)
  }
  if (dual) {
    selL <- long_df$measure == lv[1]
    selR <- !selL
    num_cols <- c("value", "lower", "upper", "bias_lo", "bias_hi")
    rng_of <- function(sel) {
      v <- unlist(long_df[sel, intersect(num_cols, names(long_df))])
      r <- range(v, na.rm = TRUE)
      if (!all(is.finite(r)) || diff(r) == 0) r <- c(0, 1)
      r
    }
    rngL <- if (lv[1] %in% bounded_labels && bounded_axis == "fixed") c(0, 1) else rng_of(selL)
    rngR <- rng_of(selR)
    to_left  <- function(x) (x - rngR[1]) / diff(rngR) * diff(rngL) + rngL[1]
    to_right <- function(x) (x - rngL[1]) / diff(rngL) * diff(rngR) + rngR[1]
    for (cl in intersect(num_cols, names(long_df)))
      long_df[[cl]][selR] <- to_left(long_df[[cl]][selR])
    dual_note <- paste0("Right axis (", paste(lv[-1], collapse = " / "),
                        ") rescaled onto the left; where lines of the two sides ",
                        "cross has no meaning.")
  }
  long_df$line_group <- if (dual || grouped) interaction(long_df$series_group, long_df$measure, drop = TRUE)
                        else long_df$series_group

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = date, y = value))

  if (dual || grouped) {
    if (mode == "separate") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(colour = series_group, group = line_group,
                                        linetype = measure), linewidth = 0.7) +
        ggplot2::geom_point(ggplot2::aes(colour = series_group, shape = measure), size = 3)
    } else {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(group = measure, linetype = measure),
                           colour = "grey40", linewidth = 0.7) +
        ggplot2::geom_point(ggplot2::aes(colour = series_group, shape = measure), size = 3)
    }
  } else if (mode == "separate") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(colour = series_group, group = series_group),
                          linewidth = 0.7) +
      ggplot2::geom_point(ggplot2::aes(colour = series_group), size = 3)
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = 1), colour = "grey40", linewidth = 0.7) +
      ggplot2::geom_point(ggplot2::aes(colour = series_group), size = 3)
  }

  has_ci <- !all(is.na(long_df$lower))
  if (has_ci) {
    ci_df <- long_df[!is.na(long_df$lower), , drop = FALSE]
    if (mode == "separate") {
      p <- p + ggplot2::geom_errorbar(
        data = ci_df,
        ggplot2::aes(ymin = lower, ymax = upper, colour = series_group),
        width = 0, linewidth = 0.6, show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_errorbar(
        data = ci_df,
        ggplot2::aes(ymin = lower, ymax = upper),
        colour = "grey40", width = 0, linewidth = 0.6
      )
    }
  }

  if (bias_band) {
    band_df <- long_df[!is.na(long_df$bias_lo), , drop = FALSE]
    if (nrow(band_df) == 0) {
      message("bias_band = TRUE, but no bias simulation results match this plot ",
              "(run count_pipeline(..., run_bias_sim = TRUE); only the simulated ",
              "measure and grave subset get a band).")
    } else {
      band_df <- band_df[order(band_df$line_group, band_df$date), , drop = FALSE]
      band <- if (mode == "separate") {
        list(
          ggplot2::geom_ribbon(data = band_df,
            ggplot2::aes(x = date, ymin = bias_lo, ymax = bias_hi,
                         fill = series_group, group = line_group),
            alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE),
          # also a short thick bar at each case, so that a band is visible
          # even where the neighbouring case has none (no ribbon to draw)
          ggplot2::geom_linerange(data = band_df,
            ggplot2::aes(x = date, ymin = bias_lo, ymax = bias_hi,
                         colour = series_group),
            linewidth = 3, alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE))
      } else {
        ggplot2::geom_ribbon(data = band_df,
          ggplot2::aes(x = date, ymin = bias_lo, ymax = bias_hi, group = line_group),
          fill = "grey50", alpha = 0.2, inherit.aes = FALSE)
      }
      p$layers <- c(band, p$layers)                      # draw behind lines/points
    }
  }

  y_label <- if (panels) NULL else as.character(lv[1])

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title    = paste0(title %||% "Timeline comparison",
                        .age_label(graves)),
      x        = NULL,
      y        = y_label,
      colour   = "Series",
      linetype = NULL, shape = NULL,
      caption  = paste(c(
        if (mode == "combined")
          "Line shows one combined trend across all series, ordered by date; points coloured by series.",
        if (bias_band && any(!is.na(long_df$bias_lo)))
          "Shaded band: IQR of category-bias simulations (split + lump).",
        dual_note),
        collapse = "\n")
    )

  if (dual) {
    p <- p + ggplot2::scale_x_continuous(labels = date_fmt) +
      ggplot2::scale_y_continuous(
        name = as.character(lv[1]),
        sec.axis = ggplot2::sec_axis(~ to_right(.), name = paste(lv[-1], collapse = " / ")))
    if (lv[1] %in% bounded_labels && bounded_axis == "fixed")
      p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
  } else if (panels) {
    # Bounded statistics (Gini, top share) always show the full 0-1 range
    bnd <- intersect(lv, bounded_labels)
    if (grouped) {
      # a panel is fixed to 0-1 only if all its statistics are bounded
      bnd_pan <- pan_lv[vapply(pan_lv, function(pn)
        all(names(lab2panel)[lab2panel == pn] %in% bounded_labels), logical(1))]
      if (length(bnd_pan) > 0 && bounded_axis == "fixed")
        p <- p + ggplot2::geom_blank(
          data = data.frame(panel = factor(rep(bnd_pan, each = 2), levels = pan_lv),
                            date = stats::median(long_df$date), value = c(0, 1)),
          ggplot2::aes(x = date, y = value), inherit.aes = FALSE)
    } else if (length(bnd) > 0 && bounded_axis == "fixed")
      p <- p + ggplot2::geom_blank(
        data = data.frame(measure = factor(rep(bnd, each = 2), levels = lv),
                          date = stats::median(long_df$date), value = c(0, 1)),
        ggplot2::aes(x = date, y = value), inherit.aes = FALSE)
    p <- p + ggplot2::scale_x_continuous(labels = date_fmt) +
      ggplot2::facet_grid(if (grouped) panel ~ . else measure ~ ., scales = "free_y", switch = "y") +
      ggplot2::theme(strip.placement = "outside",
                     strip.background = ggplot2::element_blank(),
                     strip.text.y.left = ggplot2::element_text(angle = 90))
  } else {
    p <- p + ggplot2::scale_x_continuous(labels = date_fmt)
    if (lv[1] %in% bounded_labels && bounded_axis == "fixed")
      p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
  }

  if (bounded_axis == "free" && any(lv %in% bounded_labels)) {
    zoomed <- intersect(lv, bounded_labels)
    if (dual) zoomed <- intersect(lv[1], bounded_labels)
    if (length(zoomed) > 0) {
      note <- paste0("Y-axis zoomed to the data for ", paste(zoomed, collapse = ", "),
                     " (does not start at 0).")
      old_cap <- p$labels$caption
      p <- p + ggplot2::labs(caption = if (is.null(old_cap) || !nzchar(trimws(old_cap))) note
                                       else paste(old_cap, note, sep = "\n"))
    }
  }

  p
}


#' Overlay Lorenz curves for multiple cases
#'
#' Takes the output of [count_pipeline()] and draws one Lorenz curve per
#' case, coloured by case, with each case's Gini in the legend. With several
#' datasets (e.g. `count_pipeline(list(SK = ..., GAC = ...))`), each dataset
#' gets its own panel by default.
#'
#' @param pipeline_result Output from [count_pipeline()] (must have
#'   `$by_group` entries containing `prestige_df`).
#' @param title Optional plot title.
#' @param cases Character vector of case names (keys of
#'   `pipeline_result$by_group`, e.g. `"SK: 2480-2300"`) to include.
#'   Default: all cases.
#' @param case_order Optional character vector giving the order of cases in
#'   the legend and colour ramp. Matches either group labels (e.g.
#'   `c("2800-2620", "2620-2480")`) or full case names. If `NULL` (default),
#'   the [count_pipeline()] order is used. Cases omitted are appended.
#' @param graves `"adults"` (default), `"all"` or `"subadults"`: which graves
#'   the curves (and the Gini in the legend) are based on.
#' @param measure `"combined"` (default; prestige + sqrt-count bonus),
#'   `"prestige"` or `"tot"` (TOT + 1).
#' @param facet_datasets Logical. With more than one dataset, draw one panel
#'   per dataset (default TRUE). FALSE overlays all curves in one panel.
#' @param mark_crossings Logical. Mark the points where two curves in the
#'   same panel cross (default TRUE). Where curves cross, the cases are not
#'   ranked the same by every inequality measure: one is more unequal at the
#'   bottom, the other at the top (Atkinson 1970), so a Gini ranking between
#'   them depends on the Gini's weighting. Compare with a bottom share, a top
#'   share and Atkinson indices ([plot_gini_comparison()]).
#' @param crossing_tol Curves closer than this (in cumulative wealth share)
#'   count as touching, not crossing (default 0.005), so near-identical
#'   curves don't produce a cloud of marks.
#' @param edge Crossings in the outer `edge` share of the population (default
#'   0.1, i.e. below 10% and above 90%) are drawn hollow: they rest on the
#'   few poorest or richest graves and are easily produced by one grave more
#'   or less, especially in small cases.
#' @return A [ggplot2::ggplot] object. The crossings found are attached as
#'   attribute `"crossings"` (`attr(p, "crossings")`): panel, the two cases,
#'   position (`cum_pop`, `cum_wealth`) and whether it lies in the edge zone.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' plot_lorenz_comparison(res)                           # one panel per culture
#' plot_lorenz_comparison(res, facet_datasets = FALSE)   # all in one panel
#' attr(plot_lorenz_comparison(res), "crossings")        # where curves cross
#' @export
plot_lorenz_comparison <- function(pipeline_result, title = NULL, cases = NULL,
                                   case_order = NULL,
                                   graves  = c("adults", "all", "subadults"),
                                   measure = c("combined", "prestige", "tot"),
                                   facet_datasets = TRUE,
                                   mark_crossings = TRUE,
                                   crossing_tol   = 0.005,
                                   edge           = 0.1) {
  graves  <- match.arg(graves)
  measure <- match.arg(measure)

  by_group <- pipeline_result$by_group
  if (is.null(by_group) || length(by_group) == 0)
    stop("No per-group results found. Run count_pipeline() with count data.")

  if (!is.null(cases)) by_group <- by_group[names(by_group) %in% cases]
  if (length(by_group) == 0) stop("No matching cases found.")

  measure_prefix <- switch(measure, combined = "^combined", prestige = "^prestige",
                           tot = "^TOT")
  info <- lapply(names(by_group), function(cs) {
    g  <- by_group[[cs]]
    pd <- g$prestige_df
    if (is.null(pd)) return(NULL)
    ld <- lorenz_data(pd, graves = graves)
    ld <- ld[grepl(measure_prefix, as.character(ld$measure)), , drop = FALSE]
    if (nrow(ld) == 0) return(NULL)
    grid <- g$inequality$grid
    gi <- if (!is.null(grid)) grid$gini[grid$measure == measure & grid$graves == graves] else numeric(0)
    ld$case    <- cs
    ld$dataset <- g$dataset %||% "all"
    list(ld = ld, case = cs, group = as.character(g$group %||% cs),
         dataset = g$dataset %||% "all",
         gini = if (length(gi) > 0) gi[1] else NA_real_)
  })
  info <- Filter(Negate(is.null), info)
  if (length(info) == 0) stop("No Lorenz data available.")

  keys    <- vapply(info, `[[`, "", "case")
  grp_lab <- vapply(info, `[[`, "", "group")
  ds      <- vapply(info, `[[`, "", "dataset")
  ginis   <- vapply(info, `[[`, 0, "gini")

  # Order: case_order may name groups ("2480-2300") or full cases ("SK: 2480-2300")
  ord_keys <- if (is.null(case_order)) keys else {
    pos <- match(keys, case_order)
    pos[is.na(pos)] <- match(grp_lab[is.na(pos)], case_order)
    leftover <- keys[is.na(pos)]
    if (length(leftover) > 0)
      warning("Cases not listed in case_order (appended at the end): ",
              paste(leftover, collapse = ", "))
    keys[order(is.na(pos), pos, seq_along(keys))]
  }

  multi_ds <- length(unique(ds)) > 1
  use_facets <- multi_ds && isTRUE(facet_datasets)
  lab_text <- paste0(if (use_facets) grp_lab else keys,
                     ifelse(is.na(ginis), "", paste0(" (G=", sprintf("%.2f", ginis), ")")))
  names(lab_text) <- keys

  ld_all <- do.call(rbind, lapply(info, `[[`, "ld"))
  ld_all$case <- factor(ld_all$case, levels = ord_keys)

  graves_label <- .age_label(graves)
  p <- ggplot2::ggplot(ld_all,
                       ggplot2::aes(x = cum_pop, y = cum_wealth, colour = case)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey70") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_viridis_d(labels = lab_text[ord_keys], end = 0.9) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::labs(
      title  = paste0(title %||% "Lorenz curves by case", graves_label),
      x      = "Cumulative population share",
      y      = "Cumulative wealth share",
      colour = "Case"
    )
  # ---- Crossings between curves in the same panel ----
  cross_df <- NULL
  if (isTRUE(mark_crossings)) {
    grid_x <- seq(0, 1, length.out = 2001)
    interp <- lapply(split(ld_all, ld_all$case, drop = TRUE), function(d) {
      d <- d[order(d$cum_pop, d$cum_wealth), ]
      stats::approx(d$cum_pop, d$cum_wealth, xout = grid_x, ties = max, rule = 2)$y
    })
    panel_of <- stats::setNames(if (use_facets) ds else rep("all", length(keys)), keys)
    found <- list()
    for (pn in unique(panel_of)) {
      ks <- intersect(names(interp), names(panel_of)[panel_of == pn])
      if (length(ks) < 2) next
      for (a in seq_len(length(ks) - 1)) for (b in (a + 1):length(ks)) {
        d   <- interp[[ks[a]]] - interp[[ks[b]]]
        sgn <- ifelse(abs(d) < crossing_tol, 0, sign(d))
        idx <- which(sgn != 0)
        if (length(idx) < 2) next
        flips <- which(diff(sgn[idx]) != 0)
        for (f in flips) {
          i1 <- idx[f]; i2 <- idx[f + 1]
          # first grid point between the two signed points where |d| is smallest
          j  <- (i1:i2)[which.min(abs(d[i1:i2]))]
          found[[length(found) + 1]] <- data.frame(
            dataset = if (use_facets) pn else "all", case_a = ks[a], case_b = ks[b],
            cum_pop = grid_x[j], cum_wealth = interp[[ks[a]]][j],
            edge_zone = grid_x[j] < edge || grid_x[j] > 1 - edge,
            stringsAsFactors = FALSE)
        }
      }
    }
    if (length(found) > 0) {
      cross_df <- do.call(rbind, found)
      p <- p +
        ggplot2::geom_point(data = cross_df[!cross_df$edge_zone, , drop = FALSE],
                            ggplot2::aes(x = cum_pop, y = cum_wealth),
                            inherit.aes = FALSE, shape = 21, size = 2.8, stroke = 0.9,
                            colour = "black", fill = "red") +
        ggplot2::geom_point(data = cross_df[cross_df$edge_zone, , drop = FALSE],
                            ggplot2::aes(x = cum_pop, y = cum_wealth),
                            inherit.aes = FALSE, shape = 21, size = 2.8, stroke = 0.9,
                            colour = "grey40", fill = NA) +
        ggplot2::labs(caption = paste0(
          "Red: curves cross -- the two cases are ranked differently at the bottom\n",
          "and at the top. Hollow: crossing in the outer ", round(100 * edge),
          "% (few graves, unstable)."))
    }
  }

  if (use_facets) p <- p + ggplot2::facet_wrap(~ dataset)
  attr(p, "crossings") <- cross_df
  p
}


#' Comparative boxplot of wealth per case
#'
#' One box per case (phase/group) of a [count_pipeline()] result, with the
#' individual graves drawn on top as jittered points and the mean added as a
#' white diamond. The line inside the box is the median, so the distance
#' between line and diamond shows how far rich graves pull the mean above
#' the typical grave. Cases from different datasets are drawn in separate
#' panels.
#'
#' @param pipeline_result Output of [count_pipeline()].
#' @param measure Wealth measure: `"combined"` (default; prestige +
#'   sqrt-count bonus), `"prestige"`, or `"tot"` (TOT + 1).
#' @param graves `"adults"` (default), `"all"`, or `"subadults"`, as in
#'   [plot_gini_comparison()].
#' @param relative `"none"` (default) plots wealth in each dataset's own
#'   units. `"median"` or `"mean"` divides every grave by its case's median
#'   (or mean), so 1 = the typical (or average) grave of that case. Use this
#'   to compare the *shape* of distributions between datasets whose wealth
#'   scales differ (e.g. because their grave-good columns differ).
#' @param log_scale Logical. Log10 y-axis, useful for strongly skewed
#'   wealth. Default FALSE.
#' @param show_points Logical. Draw the individual graves. Default TRUE.
#' @param case_order Optional character vector of group names giving the
#'   left-to-right order, as in [plot_gini_comparison()].
#' @param scales Facet axis scaling across datasets, passed to
#'   [ggplot2::facet_wrap()]. Default NULL: `"free"` for raw values (each
#'   dataset its own y-axis) and `"free_x"` when `relative` is used (shared
#'   y-axis, since values are then comparable).
#' @param title Optional plot title.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' plot_wealth_boxplot(res)
#' plot_wealth_boxplot(res, relative = "median")   # shapes comparable across datasets
#' plot_wealth_boxplot(res, log_scale = TRUE, show_points = TRUE)
#' @export
plot_wealth_boxplot <- function(pipeline_result,
                                measure     = c("combined", "prestige", "tot"),
                                graves      = c("adults", "all", "subadults"),
                                relative    = c("none", "median", "mean"),
                                log_scale   = FALSE,
                                show_points = TRUE,
                                case_order  = NULL,
                                scales      = NULL,
                                title       = NULL) {
  measure  <- match.arg(measure)
  graves   <- match.arg(graves)
  relative <- match.arg(relative)

  by_group <- pipeline_result$by_group
  if (is.null(by_group) || length(by_group) == 0)
    stop("No per-group results found. Pass the output of count_pipeline().")
  col <- switch(measure, combined = "combined_total", prestige = "prestige_total",
                tot = "raw_TOT")

  rows <- lapply(names(by_group), function(cs) {
    g  <- by_group[[cs]]
    pd <- g$prestige_df
    if (is.null(pd) || is.null(pd[[col]])) return(NULL)
    keep <- switch(graves,
      adults    = if ("is_adult_grave" %in% names(pd)) pd$is_adult_grave else rep(TRUE, nrow(pd)),
      subadults = if ("is_subadult_only" %in% names(pd)) pd$is_subadult_only else rep(FALSE, nrow(pd)),
      all       = rep(TRUE, nrow(pd)))
    v <- pd[[col]][keep %in% TRUE]
    if (measure == "tot") v <- v + 1
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NULL)
    ref <- switch(relative, none = 1, median = stats::median(v), mean = mean(v))
    if (!is.finite(ref) || ref == 0) {
      warning("Case '", cs, "': ", relative, " is 0 or missing; left out.")
      return(NULL)
    }
    data.frame(dataset = g$dataset %||% "all", case = cs,
               group = as.character(g$group %||% cs), value = v / ref,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || nrow(df) == 0)
    stop("No wealth values available for measure = '", measure,
         "' and graves = '", graves, "'.")

  # x labels "<group>\n(n=..)", ordered by group (case_order), then dataset
  grp_levels <- .order_cases(df$group, case_order)
  n_case     <- table(df$case)
  df$label   <- sprintf("%s\n(n=%d)", df$group, as.integer(n_case[df$case]))
  df$label   <- factor(df$label,
                       levels = unique(df$label[order(match(df$group, grp_levels))]))
  means <- stats::aggregate(value ~ dataset + label, data = df, FUN = mean)
  multi <- length(unique(df$dataset)) > 1
  if (is.null(scales)) scales <- if (relative == "none") "free" else "free_x"

  measure_lab <- switch(measure,
    combined = paste0("Combined wealth (prestige + ", .sym("sqrt_count"), ")"),
    prestige = "Prestige", tot = "TOT + 1")
  y_lab <- switch(relative,
    none   = measure_lab,
    median = paste0(measure_lab, ", relative to case median (1 = median grave)"),
    mean   = paste0(measure_lab, ", relative to case mean (1 = mean)"))
  graves_lab <- .age_label(graves)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = value))
  if (show_points)
    p <- p + ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.35,
                                  size = 1.2, colour = "steelblue")
  p <- p +
    ggplot2::geom_boxplot(outlier.shape = NA, fill = "grey85", alpha = 0.5,
                          width = 0.6, linewidth = 0.5) +
    ggplot2::geom_point(data = means, ggplot2::aes(x = label, y = value),
                        shape = 23, size = 3.5, fill = "white", colour = "black",
                        stroke = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title   = paste0(title %||% "Wealth distribution by case", graves_lab),
      x = NULL, y = y_lab,
      caption = paste0("Box: median (line) and interquartile range; whiskers: 1.5 x IQR; ",
                       "diamond: mean", if (show_points) "; points: graves (jittered sideways)" else "")
    )
  if (log_scale) p <- p + ggplot2::scale_y_log10()
  if (relative != "none")
    p <- p + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50")
  if (multi) p <- p + ggplot2::facet_wrap(~ dataset, scales = scales)
  p
}


#' Summary table of inequality results across cases
#'
#' Returns the [count_pipeline()]/[phys_pipeline()] summary data frame
#' formatted for easy reading, with optional rounding.
#'
#' @param pipeline_result Output from [count_pipeline()] or [phys_pipeline()].
#' @param digits Number of decimal places (default 3).
#' @return A data frame with one row per case.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' cases_summary(res)
#' @export
cases_summary <- function(pipeline_result, digits = 3) {
  df <- pipeline_result$summary
  if (nrow(df) == 0) {
    message("No cases processed.")
    return(invisible(df))
  }
  num_cols      <- sapply(df, is.numeric)
  df[num_cols]  <- lapply(df[num_cols], round, digits = digits)
  df
}


# The bias simulation matching the plotted grave subset, or NULL: the main
# simulation for its own subset (adults with age separation, otherwise all
# graves), the subadult one (count_pipeline(bias_ages = "both")) for subadults.
.bias_for <- function(rec, graves) {
  main <- if (is.na(rec$inequality$n_adult %||% NA)) "all" else "adults"
  if (identical(graves, main)) return(rec$bias)
  if (identical(graves, "subadults")) return(rec$bias_subadults)
  NULL
}
