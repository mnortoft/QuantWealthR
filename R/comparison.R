# ============================================================================
# Multi-case comparison functions
# Work with qw_pipeline() output to compare cases side by side
# ============================================================================

#' Compare Gini coefficients across cases
#'
#' Takes the output of [qw_pipeline()] and produces a point + error bar plot
#' showing the Gini coefficient (with 80% CI when available) for each case,
#' ordered by Gini value.  Works for both count data and physical measure data.
#'
#' @param pipeline_result Output from [qw_pipeline()].
#' @param measure For count data: which Gini to plot — `"combined"` (default,
#'   prestige + sqrt-count bonus), `"prestige"`, or `"tot"`.  Ignored for
#'   physical measure pipelines.
#' @param order_by `"gini"` (default, ascending) or `"case"` (original order).
#' @param title Optional plot title.
#' @param abs_gini Logical. If TRUE, also shows the absolute Gini (G × mean
#'   wealth) as a secondary point in a different colour. Default FALSE.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_gini_comparison(result)
#' }
plot_gini_comparison <- function(pipeline_result,
                                  measure  = c("combined", "prestige", "tot"),
                                  order_by = c("gini", "case"),
                                  title    = NULL,
                                  abs_gini = FALSE) {
  measure  <- match.arg(measure)
  order_by <- match.arg(order_by)

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
    # Parse CI string "[low–high]" if present
    if ("gini_ci" %in% names(df) && !all(is.na(df$gini_ci))) {
      ci <- regmatches(df$gini_ci,
                       regexpr("[0-9.]+.+[0-9.]+", df$gini_ci))
      if (length(ci) == nrow(df)) {
        parts  <- strsplit(ci, "[^0-9.]+")
        plot_df$lower <- as.numeric(sapply(parts, `[[`, 1))
        plot_df$upper <- as.numeric(sapply(parts, `[[`, 2))
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
      ci <- regmatches(df$gini_ci,
                       regexpr("[0-9.]+[^0-9.]+[0-9.]+", df$gini_ci))
      if (length(ci) == nrow(df)) {
        parts       <- strsplit(ci, "[^0-9.]+")
        plot_df$lower <- as.numeric(sapply(parts, `[[`, 1))
        plot_df$upper <- as.numeric(sapply(parts, `[[`, 2))
      }
    }

    y_lab <- switch(measure,
      combined = "Gini (prestige + √counts)",
      prestige = "Gini (prestige only)",
      tot      = "Gini (TOT + 1)"
    )
  }

  # Add abs_gini column if requested
  if (abs_gini && "abs_gini" %in% names(df)) {
    plot_df$abs_g <- df$abs_gini
  }

  # Order cases
  if (order_by == "gini") {
    lvls <- plot_df$case[order(plot_df$gini)]
  } else {
    lvls <- plot_df$case
  }
  plot_df$case <- factor(plot_df$case, levels = lvls)

  has_ci <- !all(is.na(plot_df$lower))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = gini, y = case)) +
    ggplot2::geom_point(colour = "#2ca25f", size = 3) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title %||% "Gini comparison across cases",
      x     = y_lab,
      y     = NULL
    )

  if (has_ci) {
    p <- p + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = lower, xmax = upper),
      height = 0.3, colour = "#2ca25f", linewidth = 0.7
    )
  }

  if (abs_gini && "abs_g" %in% names(plot_df)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = abs_g),
      colour = "#d6604d", size = 3, shape = 18
    ) + ggplot2::labs(caption = "Green = Gini | Red diamond = Absolute Gini (G × mean wealth)")
  }

  p + ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
    ggplot2::scale_x_continuous(limits = c(0, NA))
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
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave")
#' plot_lorenz_comparison(result)
#' }
plot_lorenz_comparison <- function(pipeline_result, title = NULL, cases = NULL) {

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

  ld_all <- do.call(rbind, ld_list)
  ld_all$case <- factor(ld_all$case, levels = names(by_group))

  # Only show the combined measure (prestige + sqrt counts) for clarity
  ld_combined <- ld_all[ld_all$measure == "combined (prestige + √counts)", ]

  # Annotate with Gini values from summary
  summary_df <- pipeline_result$summary
  if (!is.null(summary_df) && "gini_combined" %in% names(summary_df)) {
    labels <- paste0(summary_df$group, " (G=", round(summary_df$gini_combined, 2), ")")
    lvls   <- summary_df$group
    ld_combined$case <- factor(ld_combined$case, levels = lvls,
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
#' Returns the [qw_pipeline()] summary data frame formatted for easy
#' reading, with optional rounding.
#'
#' @param pipeline_result Output from [qw_pipeline()].
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
