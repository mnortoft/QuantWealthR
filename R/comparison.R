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
#' a single [count_pipeline()]/[phys_pipeline()] result; a named list of such
#' results (e.g. `list(Counts = count_res, Sizes = phys_res)`, or several
#' [count_pipeline()] results for sites with incompatible columns that were
#' therefore analysed in separate calls); or a mix of both, since each
#' result already carries its own `dataset`/`case` tagging internally when
#' *it* was given multiple datasets at the pipeline stage.
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
        gini        = g$gini,
        sizes       = g$sizes,
        date        = g$date     %||% NA_real_,
        date_min    = g$date_min %||% NA_real_,
        date_max    = g$date_max %||% NA_real_
      )
    }
  }
  records
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
.comparison_value_rows <- function(rec, series, graves, measure) {
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

  } else {
    grid <- rec$inequality$grid
    if (is.null(grid)) return(NULL)
    row <- grid[grid$measure == measure & grid$graves == graves, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]

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
      main_graves <- if (is.na(rec$inequality$n_adult)) "all" else "adults"
      bias_applicable <- !is.null(rec$bias) && measure == "combined" &&
        graves == main_graves
      if (bias_applicable) {
        sim <- c(
          rec$bias$ginis_split_prestige_sqrt * rec$bias$means_split_prestige_sqrt,
          rec$bias$ginis_lump_prestige_sqrt  * rec$bias$means_lump_prestige_sqrt
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
  }

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}


#' Compare wealth-inequality statistics across cases
#'
#' Takes the output of [count_pipeline()] and/or [phys_pipeline()] and
#' produces a point + line (+ error bar, where available) plot comparing
#' one or more statistics across cases, with cases on the x-axis.
#'
#' @param pipeline_result A [count_pipeline()] or [phys_pipeline()] result,
#'   **or** a list of such results to plot together -- e.g.
#'   `list(Counts = count_res, Sizes = phys_res)` to compare count-data and
#'   physical-measure cases side by side, or several separate
#'   [count_pipeline()] calls for datasets whose columns didn't fit one
#'   [count_pipeline()] call. When the data spans more than one dataset --
#'   whether because a single pipeline call was itself given multiple
#'   datasets (see `data` in [count_pipeline()]), or because a list of
#'   several results is passed here, or both -- the plot is split into one
#'   facet panel per dataset, each with its own x-axis (`scales = "free_x"`),
#'   since different datasets may have entirely different group sets.
#' @param series Character vector selecting which statistics to plot, any
#'   subset of `c("gini", "abs_gini", "mean_wealth", "median_wealth")`.
#'   Default `c("gini", "abs_gini")` (the previous default behaviour).
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
#'   age group is plotted ("(adults only)", "(all graves)", "(subadults
#'   only)", from `graves` above) is appended to this -- it is not a
#'   default that disappears once you set your own title. No note is added
#'   when every case is a physical-measure case.
#' @param show_n Logical. If TRUE (default), each x-axis tick is labelled
#'   `"<group> (n=<n>)"`, showing the sample size behind each case.
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
#' @export
#' @examples
#' \dontrun{
#' result <- count_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_gini_comparison(result)
#' plot_gini_comparison(result, series = "gini", graves = "all")
#' plot_gini_comparison(result, series = c("gini", "mean_wealth"))
#'
#' # Multiple datasets, faceted automatically
#' result2 <- count_pipeline(list(SiteA = df_a, SiteB = df_b),
#'                            group_col = "phase", id_col = "grave")
#' plot_gini_comparison(result2)
#'
#' # Count data and physical-measure data together
#' plot_gini_comparison(list(Counts = result, Sizes = phys_result))
#' }
plot_gini_comparison <- function(pipeline_result,
                                  series     = c("gini", "abs_gini"),
                                  graves     = c("adults", "all", "subadults"),
                                  measure    = c("combined", "prestige", "tot"),
                                  case_order = NULL,
                                  title      = NULL,
                                  show_n     = TRUE) {

  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  series  <- unique(series)

  valid_series <- c("gini", "abs_gini", "mean_wealth", "median_wealth")
  bad <- setdiff(series, valid_series)
  if (length(bad) > 0)
    stop("Unknown series: ", paste(bad, collapse = ", "),
         ". Must be one or more of: ", paste(valid_series, collapse = ", "))
  if (length(series) == 0)
    stop("'series' must include at least one of: ", paste(valid_series, collapse = ", "))

  records <- .gather_comparison_records(pipeline_result)
  if (length(records) == 0) stop("No cases found in pipeline_result.")

  row_list <- lapply(records, .comparison_value_rows,
                      series = series, graves = graves, measure = measure)
  row_list <- Filter(Negate(is.null), row_list)
  if (length(row_list) == 0)
    stop("No data available for the requested series/graves/measure combination.")

  long_df <- do.call(rbind, row_list)

  # Title age-group note: only meaningful if at least one case is count data.
  any_count <- any(!vapply(records, function(r) r$is_physical, logical(1)))
  age_label <- if (any_count) {
    switch(graves,
           adults    = " (adults only)",
           all       = " (all graves)",
           subadults = " (subadults only)")
  } else ""

  # Resolve case order by group name, then build the n-annotated x-axis label
  # while preserving that order (ties broken by dataset so each facet's own
  # group order is stable).
  group_levels   <- .order_cases(long_df$group, case_order)
  long_df$group  <- factor(long_df$group, levels = group_levels)
  long_df        <- long_df[order(long_df$group), , drop = FALSE]
  long_df$case_label <- if (show_n) {
    sprintf("%s (n=%d)", as.character(long_df$group), long_df$n)
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
  measure_levels_all  <- c("Gini", "Absolute Gini", "Mean wealth", "Median wealth")
  measure_colours_all <- c("Gini" = "#2ca25f", "Absolute Gini" = "#d6604d",
                            "Mean wealth" = "#3182bd", "Median wealth" = "#e6550d")
  measure_shapes_all  <- c("Gini" = 16, "Absolute Gini" = 18,
                            "Mean wealth" = 17, "Median wealth" = 15)

  present <- intersect(measure_levels_all, unique(long_df$measure))
  long_df$measure <- factor(long_df$measure, levels = present)

  has_gini      <- "Gini" %in% present
  unbounded     <- setdiff(present, "Gini")
  has_unbounded <- length(unbounded) > 0

  if (has_gini && has_unbounded) {
    sel <- long_df$measure %in% unbounded
    rng <- range(c(long_df$value[sel], long_df$lower[sel], long_df$upper[sel]),
                 na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) rng <- c(0, 1)
    to_unit <- function(x) (x - rng[1]) / diff(rng)
    to_orig <- function(x) x * diff(rng) + rng[1]

    long_df$value[sel] <- to_unit(long_df$value[sel])
    long_df$lower[sel] <- to_unit(long_df$lower[sel])
    long_df$upper[sel] <- to_unit(long_df$upper[sel])

    right_name <- if (length(unbounded) == 1) {
      switch(unbounded,
             "Absolute Gini" = "Absolute Gini (G x mean wealth)",
             "Mean wealth"   = "Mean wealth",
             "Median wealth" = "Median wealth")
    } else {
      paste0(paste(unbounded, collapse = " / "), " (shared rescaled axis -- approximate)")
    }
    y_scale <- ggplot2::scale_y_continuous(
      name     = "Gini coefficient",
      sec.axis = ggplot2::sec_axis(~ to_orig(.), name = right_name)
    )
  } else if (has_gini) {
    y_scale <- ggplot2::scale_y_continuous(name = "Gini coefficient")
  } else {
    y_scale <- ggplot2::scale_y_continuous(name = paste(present, collapse = " / "))
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
      axis.title.y      = ggplot2::element_text(colour = measure_colours_all[["Gini"]]),
      axis.title.y.right = ggplot2::element_text(colour = right_colour)
    )
  }

  if (multi_dataset)
    p <- p + ggplot2::facet_wrap(~ dataset, scales = "free_x")

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
#' @param series **A single** statistic to plot -- one of `"gini"`
#'   (default), `"abs_gini"`, `"mean_wealth"`, `"median_wealth"`. Unlike
#'   [plot_gini_comparison()], this does not accept more than one at a
#'   time: with a continuous date axis there's no categorical x position to
#'   anchor a second, rescaled axis to, so plotting two differently-scaled
#'   series together would just look like noise. Call this function once
#'   per statistic if you need more than one.
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
#' @return A [ggplot2::ggplot] object, with calendar dates formatted as
#'   "<n> BCE"/"<n> CE" on the x-axis.
#' @export
#' @examples
#' \dontrun{
#' # Single dataset, culture encoded in the group label, with a date column
#' df <- data.frame(
#'   case  = c("CWC_early", "CWC_middle", "CWC_late",
#'             "Unetice_early", "Unetice_middle", "Unetice_late"),
#'   date  = c(-2800, -2600, -2400, -2300, -2100, -1900)
#' )
#' # (each "case" here would really be a phase with many graves; date_col
#' # is a per-row column in the real grave-level data, not per phase)
#' result <- count_pipeline(graves_df, group_col = "phase",
#'                           date_col = "date")
#' plot_gini_timeline(result)                       # series = "CWC"/"Unetice"
#' plot_gini_timeline(result, mode = "combined")     # one trend line, if dates don't overlap
#'
#' # Multiple datasets -- dataset name is the series, no parsing needed
#' result2 <- count_pipeline(list(CWC = cwc_df, Unetice = un_df),
#'                            group_col = "phase", date_col = "date")
#' plot_gini_timeline(result2)
#' }
plot_gini_timeline <- function(pipeline_result,
                                series     = c("gini", "abs_gini", "mean_wealth", "median_wealth"),
                                graves     = c("adults", "all", "subadults"),
                                measure    = c("combined", "prestige", "tot"),
                                mode       = c("separate", "combined"),
                                series_sep = "_",
                                title      = NULL) {

  series  <- match.arg(series)
  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  mode    <- match.arg(mode)

  records <- .gather_comparison_records(pipeline_result)
  if (length(records) == 0) stop("No cases found in pipeline_result.")

  row_list <- lapply(records, .comparison_value_rows,
                      series = series, graves = graves, measure = measure)
  row_list <- Filter(Negate(is.null), row_list)
  if (length(row_list) == 0)
    stop("No data available for the requested series/graves/measure combination.")

  long_df <- do.call(rbind, row_list)

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

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = date, y = value))

  if (mode == "separate") {
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

  y_label <- as.character(long_df$measure[1])

  p <- p +
    ggplot2::scale_x_continuous(labels = date_fmt) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title    = title %||% "Timeline comparison",
      x        = NULL,
      y        = y_label,
      colour   = "Series",
      caption  = if (mode == "combined")
        "Line shows one combined trend across all series, ordered by date; points coloured by series."
      else NULL
    )

  if (series == "gini") p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))

  p
}


#' Overlay Lorenz curves for multiple cases
#'
#' Takes the output of [count_pipeline()] and draws one Lorenz curve per
#' case, coloured by case.
#'
#' @param pipeline_result Output from [count_pipeline()] (must have
#'   `$by_group` entries containing `prestige_df`).
#' @param title Optional plot title.
#' @param cases Character vector of case names to include.  Default: all cases.
#' @param case_order Optional character vector giving the order of cases in the
#'   legend (e.g. `c("Early", "Middle", "Late")`).  If `NULL` (default), the
#'   [count_pipeline()] order is used.  Cases omitted are appended at the end.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' result <- count_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_lorenz_comparison(result)
#' }
plot_lorenz_comparison <- function(pipeline_result, title = NULL, cases = NULL,
                                   case_order = NULL) {

  by_group <- pipeline_result$by_group
  if (is.null(by_group) || length(by_group) == 0)
    stop("No per-group results found. Run count_pipeline() with count data.")

  if (!is.null(cases)) by_group <- by_group[names(by_group) %in% cases]
  if (length(by_group) == 0) stop("No matching cases found.")

  ld_list <- lapply(names(by_group), function(grp) {
    pres_df <- by_group[[grp]]$prestige_df
    if (is.null(pres_df)) return(NULL)
    ld       <- lorenz_data(pres_df)
    ld$case  <- grp
    ld
  })
  ld_list <- Filter(Negate(is.null), ld_list)
  if (length(ld_list) == 0) stop("No Lorenz data available.")

  # Resolve case order (user-supplied case_order, else pipeline order)
  ord_cases <- .order_cases(names(by_group), case_order)

  ld_all <- do.call(rbind, ld_list)
  ld_all$case <- factor(ld_all$case, levels = ord_cases)

  # Only show the combined measure (prestige + sqrt-count bonus) for clarity.
  # Matched by prefix rather than the exact decorated label (which contains
  # a unicode sqrt sign set in inequality.R) to avoid duplicating that
  # special character here.
  ld_combined <- ld_all[grepl("^combined", as.character(ld_all$measure)), , drop = FALSE]

  # Annotate with Gini values from summary, keeping the resolved case order
  summary_df <- pipeline_result$summary
  if (!is.null(summary_df) && "gini_combined" %in% names(summary_df)) {
    summary_df <- summary_df[match(ord_cases, summary_df$group), , drop = FALSE]
    labels <- paste0(summary_df$group, " (G=", round(summary_df$gini_combined, 2), ")")
    ld_combined$case <- factor(ld_combined$case, levels = ord_cases,
                                labels = labels)
  }

  ggplot2::ggplot(ld_combined,
                   ggplot2::aes(x = cum_pop, y = cum_wealth, colour = case)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey70") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::labs(
      title  = title %||% "Lorenz curves by case",
      x      = "Cumulative population share",
      y      = "Cumulative wealth share",
      colour = "Case"
    )
}


#' Summary table of inequality results across cases
#'
#' Returns the [count_pipeline()]/[phys_pipeline()] summary data frame
#' formatted for easy reading, with optional rounding.
#'
#' @param pipeline_result Output from [count_pipeline()] or [phys_pipeline()].
#' @param digits Number of decimal places (default 3).
#' @return A data frame with one row per case.
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
