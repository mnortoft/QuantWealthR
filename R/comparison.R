# ============================================================================
# Multi-case comparison functions
# Work with qw_pipeline() output to compare cases side by side
# ============================================================================

#' Parse "\[low-high\]" Gini CI strings into numeric lower/upper bounds
#'
#' Internal helper shared by both branches of [plot_gini_comparison()] so
#' the CI string format (e.g. `"[0.42-0.61]"`) is only parsed one way.
#' Uses a single `<number><non-number-separator><number>` match per string,
#' which is safe against CI strings that contain extra digits elsewhere
#' (the previous physical-measure branch used a greedier, less precise
#' regex than the count branch; this is now the same regex for both).
#'
#' @param ci_strings Character vector, e.g. `df$gini_ci`.
#' @param n Expected number of rows (`nrow(df)`); used to sanity-check that
#'   every row produced exactly one match.
#' @return A list with `lower` and `upper` numeric vectors, or `NULL` if the
#'   number of matches didn't equal `n` (parsing failed / format mismatch).
#' @noRd
.parse_gini_ci <- function(ci_strings, n) {
  ci <- regmatches(ci_strings, regexpr("[0-9.]+[^0-9.]+[0-9.]+", ci_strings))
  if (length(ci) != n) return(NULL)
  parts <- strsplit(ci, "[^0-9.]+")
  list(
    lower = as.numeric(sapply(parts, `[[`, 1)),
    upper = as.numeric(sapply(parts, `[[`, 2))
  )
}


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


#' Compare Gini coefficients across cases
#'
#' Takes the output of [qw_pipeline()] and produces a point + error bar plot
#' showing the Gini coefficient (with 80% CI when available) for each case,
#' with cases on the x-axis and Gini on the y-axis.  Works for both count data
#' and physical measure data.
#'
#' @param pipeline_result Output from [qw_pipeline()].
#' @param measure For count data: which Gini to plot -- `"combined"` (default,
#'   prestige + sqrt-count bonus), `"prestige"`, or `"tot"`.  Ignored for
#'   physical measure pipelines.
#' @param case_order Optional character vector giving the left-to-right order
#'   of cases (e.g. `c("Early", "Middle", "Late")`).  If `NULL` (default),
#'   cases keep the order returned by [qw_pipeline()] -- the factor-level order
#'   of the grouping column, or its order of first appearance if not a factor.
#'   Cases omitted from `case_order` are appended at the end.
#' @param title Optional plot title.
#' @param abs_gini Logical. If TRUE, also shows the absolute Gini (G x mean
#'   wealth) as a secondary point in a different colour. Default FALSE.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_gini_comparison(result)
#' }
plot_gini_comparison <- function(pipeline_result,
                                  measure    = c("combined", "prestige", "tot"),
                                  case_order = NULL,
                                  title      = NULL,
                                  abs_gini   = FALSE) {
  measure <- match.arg(measure)

  df <- pipeline_result$summary
  if (nrow(df) == 0) stop("No cases in pipeline result.")

  # Detect data type (count vs physical)
  is_physical <- "gini" %in% names(df) && !"gini_combined" %in% names(df)

  if (is_physical) {
    plot_df <- data.frame(
      case  = df$group,
      gini  = df$gini,
      lower = NA_real_,
      upper = NA_real_,
      stringsAsFactors = FALSE
    )
    # Parse CI string "[low-high]" if present
    if ("gini_ci" %in% names(df) && !all(is.na(df$gini_ci))) {
      ci_bounds <- .parse_gini_ci(df$gini_ci, nrow(df))
      if (!is.null(ci_bounds)) {
        plot_df$lower <- ci_bounds$lower
        plot_df$upper <- ci_bounds$upper
      }
    }
    y_lab <- "Gini coefficient"

  } else {
    gini_col <- switch(measure,
      combined = "gini_combined",
      prestige = "gini_prestige",
      tot      = "gini_tot"
    )
    if (!gini_col %in% names(df))
      stop(sprintf("Column '%s' not found in pipeline summary.", gini_col))

    plot_df <- data.frame(
      case  = df$group,
      gini  = df[[gini_col]],
      lower = NA_real_,
      upper = NA_real_,
      stringsAsFactors = FALSE
    )

    # Parse CI string
    if ("gini_ci" %in% names(df) && !all(is.na(df$gini_ci))) {
      ci_bounds <- .parse_gini_ci(df$gini_ci, nrow(df))
      if (!is.null(ci_bounds)) {
        plot_df$lower <- ci_bounds$lower
        plot_df$upper <- ci_bounds$upper
      }
    }

    y_lab <- switch(measure,
      combined = "Gini (prestige + \u221Acounts)",
      prestige = "Gini (prestige only)",
      tot      = "Gini (TOT + 1)"
    )
  }

  # Add abs_gini column if requested
  if (abs_gini && "abs_gini" %in% names(df)) {
    plot_df$abs_g <- df$abs_gini
  }

  # Order cases: user-supplied case_order if given, else the pipeline order
  # (which respects the factor levels of the grouping column).
  lvls <- .order_cases(plot_df$case, case_order)
  plot_df$case <- factor(plot_df$case, levels = lvls)

  has_ci <- !all(is.na(plot_df$lower))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = case, y = gini)) +
    ggplot2::geom_point(colour = "#2ca25f", size = 3) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title %||% "Gini comparison across cases",
      x     = NULL,
      y     = y_lab
    )

  if (has_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.3, colour = "#2ca25f", linewidth = 0.7
    )
  }

  if (abs_gini && "abs_g" %in% names(plot_df)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = abs_g),
      colour = "#d6604d", size = 3, shape = 18
    ) + ggplot2::labs(caption = "Green = Gini | Red diamond = Absolute Gini (G \u00D7 mean wealth)")
  }

  p + ggplot2::geom_hline(yintercept = 0, colour = "grey80") +
    ggplot2::coord_cartesian(ylim = c(0, NA))
}


#' Overlay Lorenz curves for multiple cases
#'
#' Takes the output of [qw_pipeline()] and draws one Lorenz curve per case,
#' coloured by case.
#'
#' @param pipeline_result Output from [qw_pipeline()] with count data (must
#'   have `$by_group` entries containing `prestige_df`).
#' @param title Optional plot title.
#' @param cases Character vector of case names to include.  Default: all cases.
#' @param case_order Optional character vector giving the order of cases in the
#'   legend (e.g. `c("Early", "Middle", "Late")`).  If `NULL` (default), the
#'   [qw_pipeline()] order is used.  Cases omitted are appended at the end.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_lorenz_comparison(result)
#' }
plot_lorenz_comparison <- function(pipeline_result, title = NULL, cases = NULL,
                                   case_order = NULL) {

  by_group <- pipeline_result$by_group
  if (is.null(by_group) || length(by_group) == 0)
    stop("No per-group results found. Run qw_pipeline() with count data.")

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
  ord_cases