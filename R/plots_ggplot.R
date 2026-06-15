# ============================================================================
# ggplot2 static plot functions
# ============================================================================

#' Plot prestige estimates per artefact type (Bayesian or mean)
#'
#' Produces a boxplot of TOT scores per type with Bayesian credible intervals
#' (or mean ± SD for the mean method) overlaid.
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param method `"bayesian"` (default) or `"mean"`.
#' @param title Optional plot title.  If NULL, a default is used.
#' @param label Optional label (e.g. "Bell Beaker, 2500–2200 BCE") added as
#'   subtitle.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' pres <- calculate_prestige(my_df)
#' plot_prestige(pres)
#' }
plot_prestige <- function(prestige_df, method = "bayesian",
                           case = NULL, dates = NULL,
                           title = NULL, label = NULL) {

  # Use stored attributes from calculate_prestige() when available —
  # this mirrors how the app uses reactives so the plot exactly matches.
  tot_range  <- attr(prestige_df, "plot_tot_range")
  sub_types  <- attr(prestige_df, "subadult_types") %||% character(0)
  stored_method <- attr(prestige_df, "method") %||% method

  # Fall back to recomputation if attributes are missing (e.g. data was
  # modified after calculate_prestige())
  if (is.null(tot_range)) {
    count_cols <- identify_count_columns(prestige_df)
    count_cols <- setdiff(count_cols, c("raw_TOT", "prestige_total",
                                         "combined_total", "adult_count",
                                         "subadult_count"))
    # Drop NA-named or empty-named columns (can appear from CSV trailing fields)
    count_cols <- count_cols[!is.na(count_cols) & nchar(count_cols) > 0]
    df_counts  <- prestige_df[, c(names(prestige_df)[1], count_cols), drop = FALSE]
    tot_range  <- get_TOT_range(df_counts)
  }

  # Drop any rows whose Type is NA (guards against malformed column names)
  tot_range <- tot_range[!is.na(tot_range$Type), ]

  # Add display labels — subadult-exclusive types get a "*" prefix
  tot_range$Type_display <- ifelse(
    tot_range$Type %in% sub_types,
    paste0("*", tot_range$Type),
    as.character(tot_range$Type)
  )

  if (stored_method == "bayesian") {
    bayes_df <- attr(prestige_df, "plot_bayes")

    # Recompute if attribute missing
    if (is.null(bayes_df)) {
      has_age  <- "is_adult_grave" %in% names(prestige_df)
      count_cols <- setdiff(identify_count_columns(prestige_df),
                             c("raw_TOT", "prestige_total", "combined_total",
                               "adult_count", "subadult_count"))
      count_cols <- count_cols[!is.na(count_cols) & nchar(count_cols) > 0]
      rows_to_use <- if (has_age) prestige_df$is_adult_grave else TRUE
      bayes_df <- bayesian_prestige(get_TOT_range(
        prestige_df[rows_to_use, c(names(prestige_df)[1], count_cols), drop = FALSE]
      ))
    }

    bayes_df <- bayes_df[!is.na(bayes_df$Type), ]
    bayes_df$Type_display <- ifelse(
      bayes_df$Type %in% sub_types,
      paste0("*", bayes_df$Type),
      as.character(bayes_df$Type)
    )

    # Order: non-subadult types ascending by bayes_est, then subadult types
    lvls <- bayes_df |>
      dplyr::mutate(is_sub = grepl("^\\*", Type_display)) |>
      dplyr::arrange(is_sub, bayes_est) |>
      dplyr::pull(Type_display)

    tot_range$Type_display <- factor(tot_range$Type_display, levels = lvls)
    bayes_df$Type_display  <- factor(bayes_df$Type_display,  levels = lvls)

    x_lab <- if (length(sub_types) > 0)
      "Grave good type (* = subadults only)" else "Artefact type"

    p <- ggplot2::ggplot() +
      ggplot2::scale_y_continuous(limits = c(1, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::geom_boxplot(
        data = tot_range,
        ggplot2::aes(x = Type_display, y = TOT),
        outlier.shape = NA
      ) +
      ggplot2::geom_jitter(
        data = tot_range,
        ggplot2::aes(x = Type_display, y = TOT),
        width = 0.1, height = 0.1, size = 0.7, alpha = 0.5
      ) +
      ggplot2::geom_errorbar(
        data = bayes_df,
        ggplot2::aes(x = Type_display, ymin = lowerCI, ymax = upperCI),
        width = 0.1, colour = "steelblue"
      ) +
      ggplot2::geom_point(
        data = bayes_df,
        ggplot2::aes(x = Type_display, y = bayes_est),
        colour = "steelblue", size = 1.5
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        axis.title.y = ggplot2::element_text(colour = "#d6604d", face = "bold")
      ) +
      ggplot2::labs(
        title    = .prestige_title(title, "Bayesian prestige estimates (empirical prior)",
                                   case, dates),
        subtitle = label,
        x        = x_lab,
        y        = "Total object types (TOT)"
      )

  } else {
    means_df  <- attr(prestige_df, "plot_means")
    if (is.null(means_df)) means_df <- get_TOT_means(tot_range)
    means_df  <- means_df[!is.na(means_df$Type), ]

    tot_range <- merge(tot_range, means_df, by = "Type")
    lvls <- means_df |> dplyr::arrange(mean) |> dplyr::pull(Type)
    tot_range$Type_display <- factor(tot_range$Type_display, levels = lvls)

    p <- ggplot2::ggplot(tot_range,
                          ggplot2::aes(x = Type_display, y = TOT)) +
      ggplot2::scale_y_continuous(limits = c(1, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, height = 0.1, size = 0.7, alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(y = mean),
                           colour = "darkgreen", size = 2) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        axis.title.y = ggplot2::element_text(colour = "#d6604d", face = "bold")
      ) +
      ggplot2::labs(
        title    = .prestige_title(title, "Mean prestige estimates", case, dates),
        subtitle = label,
        x        = "Artefact type",
        y        = "Total object types (TOT)"
      )
  }
  p
}

# Internal helper: build prestige plot title from case/dates
.prestige_title <- function(title, default, case, dates) {
  ttl <- title %||% default
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, "\n", case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, dates)
  ttl
}


#' Plot Lorenz curves for three wealth measures
#'
#' Draws Lorenz curves for the combined, prestige-only, and TOT+1 wealth
#' measures, matching the layout of the QuantWealth app.  Optionally overlays
#' the full Gini annotation panel from [inequality_summary()].
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param ineq_summary Output from [inequality_summary()].  When supplied, adds
#'   the coloured Gini annotation (Absolute Gini, G values, mean/median,
#'   sample size, subadult Gini) to the plot interior, exactly as in the app.
#' @param case Character. Culture or period name (e.g. `"Baden Budakalász"`).
#'   Appended to the title after " — ".
#' @param dates Character. Date range string (e.g. `"3360 - 2890 BCE"` or the
#'   output of [format_dates()]).  Appended after case.
#' @param method Character label shown in the annotation, e.g.
#'   `"Bayesian (empirical)"`.  Ignored when `ineq_summary` is NULL.
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' \dontrun{
#' pres <- calculate_prestige(df)
#' ineq <- inequality_summary(pres)
#' plot_lorenz(pres, ineq, case = "Baden Budakalász",
#'             dates = format_dates(-3360, -2890))
#' }
plot_lorenz <- function(prestige_df, ineq_summary = NULL,
                         case = NULL, dates = NULL,
                         method = "Bayesian (empirical)") {
  ld <- lorenz_data(prestige_df)

  # Build title exactly as the app does
  ttl <- "Lorenz Curve and Ginis"
  if (!is.null(case) && nchar(case) > 0)
    ttl <- paste(ttl, "—", case)
  if (!is.null(dates) && nchar(dates) > 0)
    ttl <- paste(ttl, "—", dates)

  colours <- c(
    "combined (prestige + √counts)" = "#2ca25f",
    "prestige only"                       = "#2166ac",
    "TOT + 1"                             = "#d6604d"
  )
  ltypes <- c(
    "combined (prestige + √counts)" = "solid",
    "prestige only"                       = "dashed",
    "TOT + 1"                             = "dashed"
  )

  p <- ggplot2::ggplot(ld, ggplot2::aes(x = cum_pop, y = cum_wealth,
                                          colour = measure, linetype = measure)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_linetype_manual(values = ltypes) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title  = ttl,
      x      = "Sample population %",
      y      = "Prestige sum %",
      colour = NULL, linetype = NULL
    )

  # Gini annotation — mirrors the app's html_label block
  if (!is.null(ineq_summary)) {
    g  <- ineq_summary$gini_combined
    gp <- ineq_summary$gini_prestige
    gt <- ineq_summary$gini_tot

    ci_str <- function(gvec) {
      if (!is.na(gvec["lower"]))
        paste0(" ±", round((gvec["upper"] - gvec["lower"]) / 2, 2))
      else ""
    }

    n_label <- if (!is.na(ineq_summary$n_adult %||% NA))
      paste0(ineq_summary$n_adult, " adults")
    else as.character(ineq_summary$n)

    # y positions for stacked annotation lines (data coordinates 0–1)
    y0 <- 0.97; dy <- 0.055

    p <- p +
      ggplot2::annotate("text", x = 0.02, y = y0,
        label = paste0("Absolute Gini (G×mean): ",
                       round(ineq_summary$abs_gini_combined, 2)),
        hjust = 0, vjust = 1, size = 3.5,
        colour = "#2ca25f", fontface = "bold") +
      ggplot2::annotate("text", x = 0.02, y = y0 - dy,
        label = paste0("G (prestige+√count): ",
                       round(g["gini"], 2), ci_str(g)),
        hjust = 0, vjust = 1, size = 3.2, colour = "#2ca25f") +
      ggplot2::annotate("text", x = 0.02, y = y0 - 2 * dy,
        label = paste0("G (prestige only): ",
                       round(gp["gini"], 2), ci_str(gp)),
        hjust = 0, vjust = 1, size = 3.2, colour = "#2166ac") +
      ggplot2::annotate("text", x = 0.02, y = y0 - 3 * dy,
        label = paste0("TOT G: ",
                       round(gt["gini"], 2), ci_str(gt)),
        hjust = 0, vjust = 1, size = 3.2, colour = "#d6604d") +
      ggplot2::annotate("text", x = 0.02, y = y0 - 4 * dy,
        label = paste0("Mean/Median Wealth: ",
                       round(ineq_summary$mean_wealth, 2), "/",
                       round(ineq_summary$median_wealth, 2)),
        hjust = 0, vjust = 1, size = 3.2, colour = "black") +
      ggplot2::annotate("text", x = 0.02, y = y0 - 5 * dy,
        label = paste0("Sample Size: ", n_label),
        hjust = 0, vjust = 1, size = 3.2, colour = "black")

    # Subadult line (orange) — only if computed
    gs <- ineq_summary$gini_subadult
    if (!is.na(gs["gini"] %||% NA)) {
      sub_abs <- round(gs["gini"] * ineq_summary$mean_wealth_subadult, 2)
      p <- p + ggplot2::annotate("text", x = 0.02, y = y0 - 6 * dy,
        label = paste0("Subadult G: ", round(gs["gini"], 2),
                       " | Abs: ", sub_abs,
                       " | n=", ineq_summary$n_subadult),
        hjust = 0, vjust = 1, size = 3.2, colour = "orange")
      p <- p + ggplot2::annotate("text", x = 0.02, y = y0 - 7 * dy,
        label = paste0("Method: ", method),
        hjust = 0, vjust = 1, size = 3.2, colour = "black")
    } else {
      p <- p + ggplot2::annotate("text", x = 0.02, y = y0 - 6 * dy,
        label = paste0("Method: ", method),
        hjust = 0, vjust = 1, size = 3.2, colour = "black")
    }
  }
  p
}


#' Plot wealth density curves for three wealth measures
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param graves Which graves to include: `"all"` (default, matches app),
#'   `"adults"`, or `"subadults"`.  The choice is shown in the plot title.
#' @param case Character. Culture or period name added to the title.
#' @param dates Character. Date range string added to the title.
#' @return A [ggplot2::ggplot] object.
#' @export
plot_density <- function(prestige_df, graves = c("all", "adults", "subadults"),
                          case = NULL, dates = NULL) {
  graves  <- match.arg(graves)
  has_age <- "is_adult_grave" %in% names(prestige_df)

  df <- if (!has_age || graves == "all") {
    prestige_df
  } else if (graves == "adults") {
    prestige_df[prestige_df$is_adult_grave, ]
  } else {
    prestige_df[prestige_df$is_subadult_only, ]
  }

  # Title: coloured HTML via ggtext, matching the app's colour scheme
  graves_label <- switch(graves,
    all       = "",
    adults    = " (adults only)",
    subadults = " (subadults only)"
  )
  ttl <- paste0(
    "Density curves: ",
    "<span style='color:#2166ac'>**Prestige**</span>, ",
    "<span style='color:#2ca25f'>+√count</span>, ",
    "<span style='color:#d6604d'>TOT+1</span>",
    graves_label
  )
  if (!is.null(case) && nchar(case) > 0)
    ttl <- paste(ttl, "—", case)
  if (!is.null(dates) && nchar(dates) > 0)
    ttl <- paste(ttl, "—", dates)

  max_tot      <- max(df$raw_TOT + 1,   na.rm = TRUE)

  max_prestige <- max(df$prestige_total, na.rm = TRUE)
  max_combined <- max(df$combined_total, na.rm = TRUE)

  ggplot2::ggplot() +
    ggplot2::stat_density(data = data.frame(x = df$raw_TOT + 1), ggplot2::aes(x = x),
      geom = "line", colour = "#d6604d", linetype = "dashed", linewidth = 0.6) +
    ggplot2::stat_density(data = data.frame(x = df$prestige_total), ggplot2::aes(x = x),
      geom = "line", colour = "#2166ac", linetype = "dashed", linewidth = 0.6) +
    ggplot2::stat_density(data = data.frame(x = df$combined_total), ggplot2::aes(x = x),
      geom = "line", colour = "#2ca25f", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = max_tot,      colour = "#d6604d", linetype = "dotted", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = max_prestige, colour = "#2166ac", linetype = "dotted", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = max_combined, colour = "#2ca25f", linetype = "dotted", linewidth = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggtext::element_markdown()) +
    ggplot2::labs(title = ttl, x = "Value", y = "Density")
}


#' Plot decile wealth distribution
#' @param prestige_df Output from [calculate_prestige()].
#' @param case Character. Case/site/period label shown on the x-axis.
#' @param dates Character. Date range string.
#' @return A [ggplot2::ggplot] object.
#' @export
plot_decile <- function(prestige_df, case = NULL, dates = NULL) {
  dd <- decile_data(prestige_df)
  max_w <- max(if ("is_adult_grave" %in% names(prestige_df))
    prestige_df$combined_total[prestige_df$is_adult_grave]
    else prestige_df$combined_total, na.rm = TRUE)

  x_parts <- c()
  if (!is.null(case)  && nchar(case)  > 0) x_parts <- c(x_parts, case)
  if (!is.null(dates) && nchar(dates) > 0) x_parts <- c(x_parts, dates)
  x_name <- if (length(x_parts) > 0) paste(x_parts, collapse = "\n") else ""
  x_lab  <- paste0(if (nchar(x_name) > 0) paste0(x_name, "\n") else "",
                   "(max: ", round(max_w, 2), ")")

  ggplot2::ggplot(dd, ggplot2::aes(x = 1, y = total_wealth, fill = wealth_rank)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = mid_point, label = n_in_decile), size = 3, colour = "black") +
    ggplot2::scale_x_continuous(breaks = 1, labels = x_lab) +
    ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(title = "Decile distribution", x = NULL,
                  y = "Proportion of total wealth", fill = "Decile")
}


#' Plot category bias simulation results (2x2 faceted jitter plot)
#' @param bias_result Output from [simulate_category_bias()].
#' @param case Character. Case/period label for the title.
#' @param dates Character. Date range string.
#' @return A [ggplot2::ggplot] object.
#' @export
plot_bias <- function(bias_result, case = NULL, dates = NULL,
                       .title_override = NULL) {
  n_sims <- bias_result$n_sims

  hist_data <- data.frame(
    Gini = c(bias_result$ginis_split_TOT, bias_result$ginis_lump_TOT,
             bias_result$ginis_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt,
             bias_result$ginis_split_prestige, bias_result$ginis_lump_prestige,
             bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt),
    Columns = rep(c(bias_result$total_splits, bias_result$total_lumped_cols), 4),
    Direction = factor(rep(c("Split","Lump"), times = 4, each = n_sims), levels = c("Lump","Split")),
    Method = factor(rep(c("TOT+1","TOT+1+√count","Mean Prestige+1","Mean Prestige+1+√count"),
                        each = n_sims * 2),
                    levels = c("TOT+1","TOT+1+√count","Mean Prestige+1","Mean Prestige+1+√count"))
  )

  orig_lines <- data.frame(
    Method = factor(c("TOT+1","TOT+1+√count","Mean Prestige+1","Mean Prestige+1+√count"),
                    levels = levels(hist_data$Method)),
    orig_gini = c(bias_result$orig_gini_TOT, bias_result$orig_gini_TOT_sqrt,
                  bias_result$orig_gini_prestige, bias_result$orig_gini_prestige_sqrt)
  )

  iqr_per_method <- data.frame(
    Method = factor(c("TOT+1","TOT+1+√count","Mean Prestige+1","Mean Prestige+1+√count"),
                    levels = levels(hist_data$Method)),
    iqr_low = c(
      stats::quantile(c(bias_result$ginis_split_TOT,           bias_result$ginis_lump_TOT),           0.25),
      stats::quantile(c(bias_result$ginis_split_TOT_sqrt,      bias_result$ginis_lump_TOT_sqrt),      0.25),
      stats::quantile(c(bias_result$ginis_split_prestige,      bias_result$ginis_lump_prestige),      0.25),
      stats::quantile(c(bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt), 0.25)
    ),
    iqr_high = c(
      stats::quantile(c(bias_result$ginis_split_TOT,           bias_result$ginis_lump_TOT),           0.75),
      stats::quantile(c(bias_result$ginis_split_TOT_sqrt,      bias_result$ginis_lump_TOT_sqrt),      0.75),
      stats::quantile(c(bias_result$ginis_split_prestige,      bias_result$ginis_lump_prestige),      0.75),
      stats::quantile(c(bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt), 0.75)
    )
  )

  n_orig <- bias_result$n_original_cols
  split_min <- min(bias_result$total_splits);      split_max <- max(bias_result$total_splits)
  lump_min  <- min(bias_result$total_lumped_cols); lump_max  <- max(bias_result$total_lumped_cols)

  unique_str <- unique(bias_result$matched_strings[!is.na(bias_result$matched_strings)])
  matched_text <- if (length(unique_str) > 0) paste(unique_str, collapse = ", ") else "none"

  split_potential <- pmin(bias_result$adaptive_splits, bias_result$max_counts)
  eligible_idx    <- which(split_potential >= 2)
  split_eligible_text <- if (length(eligible_idx) == 0) {
    "none eligible"
  } else {
    ord  <- eligible_idx[order(split_potential[eligible_idx], decreasing = TRUE)]
    paste0(bias_result$col_names[ord], " (", split_potential[ord], ")", collapse = ", ")
  }

  iter_info <- paste0(n_sims, " iterations | Original: ", n_orig,
                      " columns | Black = original Gini | Gray dashed = IQR (middle 50%)")

  subtitle_md <- paste0(
    "<span style='color:gray40'>", iter_info, "</span><br>",
    "<span style='color:coral'><b>Lump:</b> ", n_orig, " cols → ", lump_min, "-", lump_max,
    " cols (", bias_result$n_lump_groups, " groups)<br>Matched: ", matched_text, "</span>"
  )
  caption_md <- paste0(
    "<span style='color:steelblue'><b>Split:</b> ", n_orig, " cols → ",
    split_min, "-", split_max, " cols<br>", split_eligible_text, "</span>"
  )

  ttl <- if (!is.null(.title_override)) {
    .title_override
  } else {
    t <- "Category Bias Simulation"
    if (!is.null(case)  && nchar(case)  > 0) t <- paste(t, "—", case)
    if (!is.null(dates) && nchar(dates) > 0) t <- paste(t, "—", dates)
    t
  }

  abs_orig <- c(bias_result$orig_gini_TOT * bias_result$orig_mean_TOT,
                bias_result$orig_gini_TOT_sqrt * bias_result$orig_mean_TOT_sqrt,
                bias_result$orig_gini_prestige * bias_result$orig_mean_prestige,
                bias_result$orig_gini_prestige_sqrt * bias_result$orig_mean_prestige_sqrt)
  abs_low <- c(
    stats::quantile(c(bias_result$ginis_split_TOT*bias_result$means_split_TOT, bias_result$ginis_lump_TOT*bias_result$means_lump_TOT), 0.25),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt*bias_result$means_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt*bias_result$means_lump_TOT_sqrt), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige*bias_result$means_split_prestige, bias_result$ginis_lump_prestige*bias_result$means_lump_prestige), 0.25),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt*bias_result$means_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt*bias_result$means_lump_prestige_sqrt), 0.25)
  )
  abs_high <- c(
    stats::quantile(c(bias_result$ginis_split_TOT*bias_result$means_split_TOT, bias_result$ginis_lump_TOT*bias_result$means_lump_TOT), 0.75),
    stats::quantile(c(bias_result$ginis_split_TOT_sqrt*bias_result$means_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt*bias_result$means_lump_TOT_sqrt), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige*bias_result$means_split_prestige, bias_result$ginis_lump_prestige*bias_result$means_lump_prestige), 0.75),
    stats::quantile(c(bias_result$ginis_split_prestige_sqrt*bias_result$means_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt*bias_result$means_lump_prestige_sqrt), 0.75)
  )

  facet_labels <- data.frame(
    Method = factor(c("TOT+1","TOT+1+√count","Mean Prestige+1","Mean Prestige+1+√count"),
                    levels = levels(hist_data$Method)),
    label = paste0(
      "IQR: [", round(iqr_per_method$iqr_low, 3), "-", round(iqr_per_method$iqr_high, 3), "]\n",
      "Abs G: [", round(abs_low, 1), "-", round(abs_high, 1),
      "] (orig: ", round(abs_orig, 1), ")"
    ),
    # Position below the jitter area (y scale is -0.5 to 0.5)
    x_pos = 0.01,
    y_pos = -0.42
  )

  ggplot2::ggplot(hist_data, ggplot2::aes(x = Gini, y = 0, colour = Direction)) +
    ggplot2::geom_jitter(height = 0.4, alpha = 0.5, size = 1.5) +
    ggplot2::geom_vline(data = orig_lines, ggplot2::aes(xintercept = orig_gini),
                         colour = "black", linetype = "solid", linewidth = 1) +
    ggplot2::geom_vline(data = iqr_per_method, ggplot2::aes(xintercept = iqr_low),
                         colour = "darkgray", linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_vline(data = iqr_per_method, ggplot2::aes(xintercept = iqr_high),
                         colour = "darkgray", linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_text(data = facet_labels,
                        ggplot2::aes(x = x_pos, y = y_pos, label = label),
                        inherit.aes = FALSE, hjust = 0, vjust = 0,
                        size = 2.5, colour = "gray40", lineheight = 0.9) +
    ggplot2::scale_colour_manual(values = c(Split = "steelblue", Lump = "coral")) +
    ggplot2::facet_wrap(~ Method, ncol = 2) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(limits = c(-0.5, 0.5), breaks = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text         = ggplot2::element_text(size = 10, face = "bold"),
      plot.title         = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle      = ggtext::element_markdown(size = 9, hjust = 0, lineheight = 1.3),
      plot.caption       = ggtext::element_markdown(size = 8, hjust = 1),
      legend.position    = "bottom",
      axis.text.y        = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = ttl, subtitle = subtitle_md, caption = caption_md,
                  x = "Simulated Gini", y = NULL, colour = "Direction")
}


#' Plot Lorenz curve for physical measure data
#' @param sizes Numeric vector of physical measure values.
#' @param ineq Optional output from [physical_measure_gini()].
#' @param data_type Character label for the measure type.
#' @param case Character. Case/site/period label.
#' @param dates Character. Date range string.
#' @return A [ggplot2::ggplot] object.
#' @export
plot_physical_lorenz <- function(sizes, ineq = NULL, data_type = "Physical measure",
                                  case = NULL, dates = NULL) {
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  g_val <- if (!is.null(ineq)) ineq else physical_measure_gini(sizes, conf_level = NA)
  gini  <- round(g_val["gini"], 2)
  mean_s <- mean(sizes)
  abs_g  <- round(gini * mean_s, 2)
  ci_str <- if (!is.na(g_val["lower"] %||% NA))
    paste0(" ±", round((g_val["upper"] - g_val["lower"]) / 2, 2)) else ""

  ttl <- paste(data_type, "Lorenz Curve")
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, "—", case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, "—", dates)

  ggplot2::ggplot(data.frame(Value = sizes), ggplot2::aes(Value)) +
    gglorenz::stat_lorenz(colour = "purple", geom = "path", linetype = "solid", linewidth = 1) +
    ggplot2::geom_abline(colour = "grey50") +
    ggplot2::annotate("text", x = 0.05, y = 0.90,
                       label = paste0("Abs Gini: ", abs_g, "\nGini: ", gini, ci_str,
                                      "\nMean/Median: ", round(mean_s, 2), "/",
                                      round(median(sizes), 2), "\nn = ", length(sizes)),
                       hjust = 0, vjust = 1, size = 3.2, colour = "purple") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = ttl, x = "Sample population %", y = paste(data_type, "%"))
}


#' Plot density curve for physical measure data
#' @param sizes Numeric vector of physical measure values.
#' @param data_type Character label for the measure.
#' @param case Character. Case/site/period label.
#' @param dates Character. Date range string.
#' @return A [ggplot2::ggplot] object.
#' @export
plot_physical_density <- function(sizes, data_type = "Physical measure",
                                   case = NULL, dates = NULL) {
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  ttl <- paste(data_type, "Distribution")
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, "—", case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, "—", dates)
  ggplot2::ggplot(data.frame(Value = sizes), ggplot2::aes(x = Value)) +
    ggplot2::stat_density(geom = "line", colour = "purple", linewidth = 1) +
    ggplot2::geom_vline(xintercept = max(sizes), colour = "purple",
                         linetype = "dotted", linewidth = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = ttl, x = data_type, y = "Density")
}


#' Plot decile distribution for physical measure data
#' @param sizes Numeric vector of physical measure values.
#' @param data_type Character label for the measure.
#' @param case Character. Case/site/period label.
#' @param dates Character. Date range string.
#' @param unit Character. Unit string (e.g. "m2").
#' @return A [ggplot2::ggplot] object.
#' @export
plot_physical_decile <- function(sizes, data_type = "Physical measure",
                                  case = NULL, dates = NULL, unit = "") {
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  max_v <- max(sizes)
  dd <- data.frame(Value = sizes) |>
    dplyr::arrange(Value) |>
    dplyr::mutate(wealth_rank = dplyr::ntile(Value, 10),
                  wealth_share = Value / sum(Value)) |>
    dplyr::group_by(wealth_rank) |>
    dplyr::summarise(total_wealth = round(sum(wealth_share), 2),
                     n_in_decile  = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(wealth_rank) |>
    dplyr::mutate(cumulative_wealth = cumsum(total_wealth),
                  mid_point = cumulative_wealth - total_wealth / 2,
                  wealth_rank = factor(wealth_rank, levels = rev(1:10)))

  x_parts <- c()
  if (!is.null(case)  && nchar(case)  > 0) x_parts <- c(x_parts, case)
  if (!is.null(dates) && nchar(dates) > 0) x_parts <- c(x_parts, dates)
  x_name <- paste(x_parts, collapse = "\n")
  unit_str <- if (nchar(unit) > 0) paste0(" ", unit) else ""
  x_lab <- paste0(if (nchar(x_name) > 0) paste0(x_name, "\n") else "",
                   "(max: ", round(max_v, 2), unit_str, ")")

  ggplot2::ggplot(dd, ggplot2::aes(x = 1, y = total_wealth, fill = wealth_rank)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = mid_point, label = n_in_decile),
                        size = 3, colour = "black") +
    ggplot2::scale_x_continuous(breaks = 1, labels = x_lab) +
    ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(title = "Decile distribution", x = NULL,
                  y = "Proportion of total", fill = "Decile")
}
