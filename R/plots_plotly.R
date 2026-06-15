# ============================================================================
# Interactive (plotly) plot functions
# ============================================================================

#' Interactive prestige boxplot
#' @inheritParams plot_prestige
#' @return A plotly widget.
#' @export
plot_prestige_interactive <- function(prestige_df, method = "bayesian",
                                       case = NULL, dates = NULL,
                                       title = NULL, label = NULL) {
  p <- plot_prestige(prestige_df, method = method, case = case, dates = dates,
                     title = title, label = label)
  plotly::ggplotly(p)
}

#' Interactive Lorenz curve
#' @inheritParams plot_lorenz
#' @return A plotly widget.
#' @export
plot_lorenz_interactive <- function(prestige_df, ineq_summary = NULL,
                                      case = NULL, dates = NULL,
                                      method = "Bayesian (empirical)") {
  p <- plot_lorenz(prestige_df, ineq_summary = ineq_summary,
                   case = case, dates = dates, method = method)
  plotly::ggplotly(p)
}

#' Interactive wealth density curves
#' @inheritParams plot_density
#' @return A plotly widget.
#' @export
plot_density_interactive <- function(prestige_df,
                                      graves = c("all", "adults", "subadults"),
                                      case = NULL, dates = NULL) {
  graves <- match.arg(graves)
  p  <- plot_density(prestige_df, graves = graves, case = case, dates = dates)
  pw <- plotly::ggplotly(p)
  for (i in seq_along(pw$x$data)) {
    tr <- pw$x$data[[i]]
    if (!is.null(tr$fill) && tr$fill != "none") {
      tr$fill <- "none"; tr$fillcolor <- "rgba(0,0,0,0)"
    }
    pw$x$data[[i]] <- tr
  }
  pw
}

#' Interactive decile plot
#' @inheritParams plot_decile
#' @return A plotly widget.
#' @export
plot_decile_interactive <- function(prestige_df, case = NULL, dates = NULL) {
  p <- plot_decile(prestige_df, case = case, dates = dates)
  plotly::ggplotly(p, tooltip = c("fill", "y"))
}

#' Interactive category bias simulation plot
#'
#' @param bias_result Output from [simulate_category_bias()].
#' @param case Character. Case label.
#' @param dates Character. Date range string.
#' @return A plotly widget.
#' @export
plot_bias_interactive <- function(bias_result, case = NULL, dates = NULL) {
  # Pass empty title to plot_bias so ggplot2 doesn't add a title that
  # duplicates the plotly annotation we add below
  p  <- plot_bias(bias_result, case = NULL, dates = NULL,
                  .title_override = "")
  pw <- plotly::ggplotly(p, tooltip = "text")

  n_sims <- bias_result$n_sims
  n_orig <- bias_result$n_original_cols

  iqr_low <- c(
    stats::quantile(c(bias_result$ginis_split_TOT, bias_result$ginis_lump_TOT), 0.25),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige, bias_result$ginis_lump_prestige), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt), 0.25)
  )
  iqr_high <- c(
    stats::quantile(c(bias_result$ginis_split_TOT, bias_result$ginis_lump_TOT), 0.75),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige, bias_result$ginis_lump_prestige), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt), 0.75)
  )
  abs_low <- c(
    stats::quantile(c(bias_result$ginis_split_TOT*bias_result$means_split_TOT,
                      bias_result$ginis_lump_TOT*bias_result$means_lump_TOT), 0.25),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt*bias_result$means_split_TOT_sqrt,
                      bias_result$ginis_lump_TOT_sqrt*bias_result$means_lump_TOT_sqrt), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige*bias_result$means_split_prestige,
                      bias_result$ginis_lump_prestige*bias_result$means_lump_prestige), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt*bias_result$means_split_prestige_sqrt,
                      bias_result$ginis_lump_prestige_sqrt*bias_result$means_lump_prestige_sqrt), 0.25)
  )
  abs_high <- c(
    stats::quantile(c(bias_result$ginis_split_TOT*bias_result$means_split_TOT,
                      bias_result$ginis_lump_TOT*bias_result$means_lump_TOT), 0.75),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt*bias_result$means_split_TOT_sqrt,
                      bias_result$ginis_lump_TOT_sqrt*bias_result$means_lump_TOT_sqrt), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige*bias_result$means_split_prestige,
                      bias_result$ginis_lump_prestige*bias_result$means_lump_prestige), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt*bias_result$means_split_prestige_sqrt,
                      bias_result$ginis_lump_prestige_sqrt*bias_result$means_lump_prestige_sqrt), 0.75)
  )
  orig_abs <- c(
    bias_result$orig_gini_TOT * bias_result$orig_mean_TOT,
    bias_result$orig_gini_TOT_sqrt * bias_result$orig_mean_TOT_sqrt,
    bias_result$orig_gini_prestige * bias_result$orig_mean_prestige,
    bias_result$orig_gini_prestige_sqrt * bias_result$orig_mean_prestige_sqrt
  )

  split_min <- min(bias_result$total_splits); split_max <- max(bias_result$total_splits)
  lump_min  <- min(bias_result$total_lumped_cols); lump_max <- max(bias_result$total_lumped_cols)

  split_potential <- pmin(bias_result$adaptive_splits, bias_result$max_counts)
  eligible_idx    <- which(split_potential >= 2)
  if (length(eligible_idx) == 0) {
    top_split_text <- "Eligible to split (0): none"
  } else {
    ord <- eligible_idx[order(split_potential[eligible_idx], decreasing = TRUE)]
    top_split_text <- paste0("Eligible to split (", length(eligible_idx), "): ",
                              paste0(bias_result$col_names[ord], " (",
                                     split_potential[ord], ")", collapse = ", "))
  }

  unique_str <- unique(bias_result$matched_strings[!is.na(bias_result$matched_strings)])
  lump_str_text <- if (length(unique_str) > 0)
    paste0("Matched: ", paste(unique_str, collapse = ", "))
  else "No shared strings found"

  subtitle_text <- paste0(n_sims, " iterations | Original: ", n_orig,
                           " columns | Black = original Gini | Gray dashed = IQR (middle 50%)")
  ttl_parts <- "Category Bias Simulation"
  if (!is.null(case)  && nchar(case)  > 0) ttl_parts <- paste(ttl_parts, "-", case)
  if (!is.null(dates) && nchar(dates) > 0) ttl_parts <- paste(ttl_parts, "-", dates)

  # Per-facet IQR annotations — positioned just below each panel
  # (paper y: top facets ~0.95, bottom facets ~0.45, placed at bottom edge)
  facet_x <- c(0.01, 0.51, 0.01, 0.51)
  facet_y <- c(0.52, 0.52, 0.02, 0.02)   # bottom of each facet row
  facet_annotations <- lapply(1:4, function(i) {
    list(x = facet_x[i], y = facet_y[i], xref = "paper", yref = "paper",
         text = paste0("<b>IQR:</b> [", round(iqr_low[i], 3), " - ",
                       round(iqr_high[i], 3), "]  ",
                       "<b>Abs G:</b> [", round(abs_low[i], 1), " - ",
                       round(abs_high[i], 1), "] (orig: ", round(orig_abs[i], 1), ")"),
         showarrow = FALSE, font = list(size = 8, color = "gray40"),
         align = "left", xanchor = "left", yanchor = "top")
  })

  all_annotations <- c(
    list(
      list(x = 0.5, y = 1.18, xref = "paper", yref = "paper",
           text = paste0("<b>", ttl_parts, "</b><br><span style='font-size:11px;color:gray'>",
                         subtitle_text, "</span>"),
           showarrow = FALSE, font = list(size = 14), align = "center", xanchor = "center"),
      list(x = 0.02, y = 1.08, xref = "paper", yref = "paper",
           text = paste0("<b>Lump:</b> ", n_orig, " cols -> ", lump_min, "-", lump_max,
                         " cols (", bias_result$n_lump_groups, " groups)<br>",
                         "<span style='font-size:9px'>", lump_str_text, "</span>"),
           showarrow = FALSE, font = list(size = 10, color = "coral"),
           align = "left", xanchor = "left"),
      list(x = 0.98, y = 1.08, xref = "paper", yref = "paper",
           text = paste0("<b>Split:</b> ", n_orig, " cols -> ", split_min, "-", split_max,
                         " cols<br><span style='font-size:9px'>", top_split_text, "</span>"),
           showarrow = FALSE, font = list(size = 10, color = "steelblue"),
           align = "right", xanchor = "right")
    ),
    facet_annotations
  )

  pw |> plotly::layout(margin = list(t = 160, b = 100),
                        legend = list(orientation = "h", x = 0.3, y = -0.15),
                        annotations = all_annotations)
}

#' Interactive Lorenz curve for physical measure data
#' @inheritParams plot_physical_lorenz
#' @return A plotly widget.
#' @export
plot_physical_lorenz_interactive <- function(sizes, ineq = NULL,
                                              data_type = "Physical measure",
                                              case = NULL, dates = NULL) {
  p <- plot_physical_lorenz(sizes, ineq = ineq, data_type = data_type,
                             case = case, dates = dates)
  plotly::ggplotly(p)
}

#' Interactive density plot for physical measure data
#' @inheritParams plot_physical_density
#' @return A plotly widget.
#' @export
plot_physical_density_interactive <- function(sizes, data_type = "Physical measure",
                                               case = NULL, dates = NULL) {
  p  <- plot_physical_density(sizes, data_type = data_type, case = case, dates = dates)
  pw <- plotly::ggplotly(p)
  for (i in seq_along(pw$x$data)) {
    tr <- pw$x$data[[i]]
    if (!is.null(tr$fill) && tr$fill != "none") {
      tr$fill <- "none"; tr$fillcolor <- "rgba(0,0,0,0)"
    }
    pw$x$data[[i]] <- tr
  }
  pw
}

#' Interactive decile plot for physical measure data
#' @inheritParams plot_physical_decile
#' @return A plotly widget.
#' @export
plot_physical_decile_interactive <- function(sizes, data_type = "Physical measure",
                                              case = NULL, dates = NULL, unit = "") {
  p <- plot_physical_decile(sizes, data_type = data_type,
                             case = case, dates = dates, unit = unit)
  plotly::ggplotly(p, tooltip = c("fill", "y"))
}

#' Combined interactive panel for physical measure data
#'
#' @param sizes Numeric vector of physical measure values.
#' @param data_type Character. Measure type label.
#' @param case Character. Case label.
#' @param dates Character. Date range string.
#' @param unit Character. Unit string (e.g. "m2").
#' @return A plotly subplot widget.
#' @export
plot_physical_panel <- function(sizes, data_type = "Physical measure",
                                 case = NULL, dates = NULL, unit = "") {
  ineq <- physical_measure_gini(sizes)
  p1 <- plot_physical_lorenz_interactive(sizes, ineq = ineq, data_type = data_type,
                                          case = case, dates = dates)
  p2 <- plot_physical_density_interactive(sizes, data_type = data_type,
                                           case = case, dates = dates)
  p3 <- plot_physical_decile_interactive(sizes, data_type = data_type,
                                          case = case, dates = dates, unit = unit)
  plotly::subplot(p1, p2, p3, nrows = 1, shareY = FALSE,
                   titleX = TRUE, titleY = TRUE) |>
    plotly::layout(showlegend = FALSE)
}

#' Combined interactive inequality panel (Lorenz + density + decile)
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param ineq_summary Output from [inequality_summary()] (computed if NULL).
#' @param case Character. Case label.
#' @param dates Character. Date range string.
#' @param method Character. Method label for the annotation.
#' @return A plotly subplot widget.
#' @export
plot_inequality_panel <- function(prestige_df, ineq_summary = NULL,
                                   case = NULL, dates = NULL,
                                   method = "Bayesian (empirical)") {
  if (is.null(ineq_summary)) ineq_summary <- inequality_summary(prestige_df)
  p1 <- plotly::ggplotly(plot_lorenz(prestige_df, ineq_summary = ineq_summary,
                                      case = case, dates = dates, method = method))
  p2_raw <- plot_density(prestige_df, case = case, dates = dates)
  p2 <- plotly::ggplotly(p2_raw)
  for (i in seq_along(p2$x$data)) {
    tr <- p2$x$data[[i]]
    if (!is.null(tr$fill) && tr$fill != "none") {
      tr$fill <- "none"; tr$fillcolor <- "rgba(0,0,0,0)"
    }
    p2$x$data[[i]] <- tr
  }
  p3 <- plotly::ggplotly(plot_decile(prestige_df, case = case, dates = dates),
                          tooltip = c("fill", "y"))
  plotly::subplot(p1, p2, p3, nrows = 1, shareY = FALSE,
                   titleX = TRUE, titleY = TRUE) |>
    plotly::layout(showlegend = FALSE)
}

#' Interactive Gini comparison across cases
#' @inheritParams plot_gini_comparison
#' @return A plotly widget.
#' @export
plot_gini_comparison_interactive <- function(pipeline_result,
                                              measure  = c("combined", "prestige", "tot"),
                                              order_by = c("gini", "case"),
                                              title    = NULL, abs_gini = FALSE) {
  p <- plot_gini_comparison(pipeline_result, measure = match.arg(measure),
                             order_by = match.arg(order_by),
                             title = title, abs_gini = abs_gini)
  plotly::ggplotly(p)
}

#' Interactive Lorenz curve comparison across cases
#' @inheritParams plot_lorenz_comparison
#' @return A plotly widget.
#' @export
plot_lorenz_comparison_interactive <- function(pipeline_result,
                                                title = NULL, cases = NULL) {
  p <- plot_lorenz_comparison(pipeline_result, title = title, cases = cases)
  plotly::ggplotly(p)
}
