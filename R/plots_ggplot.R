# ============================================================================
# ggplot2 static plot functions
# ============================================================================

#' Plot prestige estimates per artefact type (Bayesian or mean)
#'
#' Produces a boxplot of TOT scores per type with Bayesian credible intervals
#' (or mean +/- SD for the mean method) overlaid.
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param method `"bayesian"` (default) or `"mean"`.
#' @param case Character. Case/period label appended to the title (after
#'   `title`/the default, before `dates`). Ignored if `title` makes the
#'   title long enough already -- there is no automatic wrapping.
#' @param dates Character. Date range string appended to the title after
#'   `case`.
#' @param title Optional plot title.  If NULL, a default is used.
#' @param label Optional label (e.g. "Bell Beaker, 2500-2200 BCE") added as
#'   subtitle.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' plot_prestige(pres, case = "Example site")
#' @export
plot_prestige <- function(prestige_df, method = "bayesian",
                           case = NULL, dates = NULL,
                           title = NULL, label = NULL) {

  # Use stored attributes from calculate_prestige() when available --
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

  # Add display labels -- subadult-exclusive types get a "*" prefix
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
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      # coord_cartesian() clips the *view* at y = 0 (TOT counts can't be
      # negative) without dropping data -- unlike scale_y_continuous(limits=),
      # which would silently delete any geom_errorbar() row whose lowerCI
      # (a Bayesian credible interval bound, which can mathematically dip
      # below 0) falls outside the range, removing the whole error bar.
      ggplot2::coord_cartesian(ylim = c(0, NA)) +
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
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      # See comment in the bayesian branch above: coord_cartesian() clips the
      # view without dropping rows whose values fall outside it.
      ggplot2::coord_cartesian(ylim = c(0, NA)) +
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
#' @param case Character. Culture or period name (e.g. `"Baden Budakalasz"`).
#'   Appended to the title after " -- ".
#' @param dates Character. Date range string (e.g. `"3360 - 2890 BCE"` or the
#'   output of [format_dates()]).  Appended after case.
#' @param method Character label shown in the annotation, e.g.
#'   `"Bayesian (empirical)"`.  Ignored when `ineq_summary` is NULL.
#' @param graves Which graves to use: `"adults"` (default, matching prior
#'   behaviour), `"all"`, or `"subadults"`. Ignored if `prestige_df` has no
#'   `is_adult_grave` column (age separation was not used).
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' ineq <- inequality_summary(pres)
#' plot_lorenz(pres, ineq, case = "Example site", dates = format_dates(-2800, -2500))
#' @export
plot_lorenz <- function(prestige_df, ineq_summary = NULL,
                         case = NULL, dates = NULL,
                         method = "Bayesian (empirical)",
                         graves = c("adults", "all", "subadults")) {
  graves <- match.arg(graves)
  ld <- lorenz_data(prestige_df, graves = graves)

  # Build title exactly as the app does
  graves_label <- .age_label(graves)
  ttl <- paste0("Lorenz Curve and Ginis", graves_label)
  if (!is.null(case) && nchar(case) > 0)
    ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0)
    ttl <- paste(ttl, .sym("dash"), dates)

  lev     <- levels(ld$measure)          # combined / prestige only / TOT + 1
  colours <- stats::setNames(c("#2ca25f", "#2166ac", "#d6604d"), lev)
  ltypes  <- stats::setNames(c("solid", "dashed", "dashed"), lev)

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

  # Gini annotation -- mirrors the app's html_label block
  if (!is.null(ineq_summary)) {
    g  <- ineq_summary$gini_combined
    gp <- ineq_summary$gini_prestige
    gt <- ineq_summary$gini_tot

    ci_str <- function(gvec) {
      if (!is.na(gvec["lower"]))
        paste0(" \u00B1", round((gvec["upper"] - gvec["lower"]) / 2, 2))
      else ""
    }

    n_label <- if (!is.na(ineq_summary$n_adult %||% NA))
      paste0(ineq_summary$n_adult, " adults")
    else as.character(ineq_summary$n)

    # y positions for stacked annotation lines (data coordinates 0-1)
    y0 <- 0.97; dy <- 0.055

    p <- p +
      ggplot2::annotate("text", x = 0.02, y = y0,
        label = paste0("Absolute Gini (G\u00D7mean): ",
                       round(ineq_summary$abs_gini_combined, 2)),
        hjust = 0, vjust = 1, size = 3.5,
        colour = "#2ca25f", fontface = "bold") +
      ggplot2::annotate("text", x = 0.02, y = y0 - dy,
        label = paste0(paste0("G (prestige+", .sym("sqrt_count"), "): "),
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

    # Subadult line (orange) -- only if computed
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
#' @param graves Which graves to include: `"adults"` (default), `"all"`,
#'   or `"subadults"`.  The choice is shown in the plot title.
#' @param case Character. Culture or period name added to the title.
#' @param dates Character. Date range string added to the title.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' plot_density(pres)
#' plot_density(pres, graves = "all", case = "Example site")
#' @export
plot_density <- function(prestige_df, graves = c("adults", "all", "subadults"),
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
  graves_label <- .age_label(graves)
  ttl <- paste0(
    "Density curves: ",
    "<span style='color:#2166ac'>**Prestige**</span>, ",
    paste0("<span style='color:#2ca25f'>+", .sym("sqrt_count"), "</span>, "),
    "<span style='color:#d6604d'>TOT+1</span>",
    graves_label
  )
  if (!is.null(case) && nchar(case) > 0)
    ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0)
    ttl <- paste(ttl, .sym("dash"), dates)

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
#' @param graves Which graves to use: `"adults"` (default, matching prior
#'   behaviour), `"all"`, or `"subadults"`. Ignored if `prestige_df` has no
#'   `is_adult_grave` column (age separation was not used).
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' plot_decile(pres, case = "Example site")
#' @export
plot_decile <- function(prestige_df, case = NULL, dates = NULL,
                         graves = c("adults", "all", "subadults")) {
  graves  <- match.arg(graves)
  dd      <- decile_data(prestige_df, graves = graves)
  has_age <- "is_adult_grave" %in% names(prestige_df)

  max_subset <- if (!has_age || graves == "all") {
    prestige_df$combined_total
  } else if (graves == "adults") {
    prestige_df$combined_total[prestige_df$is_adult_grave]
  } else {
    prestige_df$combined_total[prestige_df$is_subadult_only]
  }
  max_w <- max(max_subset, na.rm = TRUE)

  graves_label <- .age_label(graves)

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
    ggplot2::labs(title = paste0("Decile distribution", graves_label), x = NULL,
                  y = "Proportion of total wealth", fill = "Decile")
}


#' Get the bias simulation result from a plot function's input
#'
#' Accepts a `"qw_bias"` object (from [simulate_category_bias()]) or a
#' `"qw_group"` object (one entry of `count_pipeline()$by_group`, whose
#' `$bias` is used). Anything else -- in particular a whole
#' `count_pipeline()` result -- stops with a message saying what to pass
#' instead. Unlabelled lists are accepted only if they look like bias
#' results made before these classes were introduced.
#' @noRd
.as_bias_result <- function(x) {
  if (inherits(x, "qw_bias")) return(x)
  if (inherits(x, "qw_group")) {
    if (is.null(x$bias))
      stop(sprintf("Group '%s' has no bias simulation. Re-run count_pipeline(..., run_bias_sim = TRUE).",
                   x$case))
    return(x$bias)
  }
  if (inherits(x, "qw_pipeline")) {
    stop("Expected a bias simulation result or one group from `$by_group`, ",
         "but got the whole count_pipeline() result. Select one group first, e.g. ",
         sprintf("x$by_group[[\"%s\"]].", names(x$by_group)[1]))
  }
  # Results saved before the "qw_bias" class existed
  if (is.list(x) && !is.null(x$ginis_split_TOT)) return(x)
  stop("Expected output of simulate_category_bias() or one group from ",
       "count_pipeline()$by_group.")
}


#' Plot category bias simulation results (2x2 faceted jitter plot)
#'
#' The grave subset (`"adults"`, `"all"`, or `"subadults"`) is fixed when you
#' call [simulate_category_bias()] via its `graves` argument, not here -- this
#' function just labels which subset was used in the title. To compare
#' adults vs subadults, run [simulate_category_bias()] twice with different
#' `graves` values and plot each result separately.
#'
#' @param bias_result Output from [simulate_category_bias()], or one group
#'   from `count_pipeline()$by_group` (its `$bias` is used).
#' @param case Character. Case/period label for the title. When
#'   `bias_result` is a pipeline group and `case` is NULL, the group's label
#'   is used.
#' @param dates Character. Date range string.
#' @param .title_override Internal use only. If supplied, replaces the
#'   auto-generated title entirely (bypassing `case`/`dates`/grave-subset
#'   labelling). Not currently called anywhere in the package; exposed as a
#'   hook for callers that need full control over the title.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' set.seed(1)
#' bias <- simulate_category_bias(df, pres, n_sims = 50)
#' plot_bias(bias, case = "Example site", dates = format_dates(-2800, -2500))
#' @export
plot_bias <- function(bias_result, case = NULL, dates = NULL,
                       .title_override = NULL) {
  if (is.null(case) && inherits(bias_result, "qw_group")) case <- bias_result$case
  bias_result <- .as_bias_result(bias_result)
  n_sims <- bias_result$n_sims

  hist_data <- data.frame(
    Gini = c(bias_result$ginis_split_TOT, bias_result$ginis_lump_TOT,
             bias_result$ginis_split_TOT_sqrt, bias_result$ginis_lump_TOT_sqrt,
             bias_result$ginis_split_prestige, bias_result$ginis_lump_prestige,
             bias_result$ginis_split_prestige_sqrt, bias_result$ginis_lump_prestige_sqrt),
    Columns = rep(c(bias_result$total_splits, bias_result$total_lumped_cols), 4),
    Direction = factor(rep(c("Split","Lump"), times = 4, each = n_sims), levels = c("Lump","Split")),
    Method = factor(rep(c("TOT+1",paste0("TOT+1+", .sym("sqrt_count")),"Mean Prestige+1",paste0("Mean Prestige+1+", .sym("sqrt_count"))),
                        each = n_sims * 2),
                    levels = c("TOT+1",paste0("TOT+1+", .sym("sqrt_count")),"Mean Prestige+1",paste0("Mean Prestige+1+", .sym("sqrt_count"))))
  )

  orig_lines <- data.frame(
    Method = factor(c("TOT+1",paste0("TOT+1+", .sym("sqrt_count")),"Mean Prestige+1",paste0("Mean Prestige+1+", .sym("sqrt_count"))),
                    levels = levels(hist_data$Method)),
    orig_gini = c(bias_result$orig_gini_TOT, bias_result$orig_gini_TOT_sqrt,
                  bias_result$orig_gini_prestige, bias_result$orig_gini_prestige_sqrt)
  )

  iqr_per_method <- data.frame(
    Method = factor(c("TOT+1",paste0("TOT+1+", .sym("sqrt_count")),"Mean Prestige+1",paste0("Mean Prestige+1+", .sym("sqrt_count"))),
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
    "<span style='color:coral'><b>Lump:</b> ", n_orig, paste0(" cols ", .sym("arrow"), " "), lump_min, "-", lump_max,
    " cols (", bias_result$n_lump_groups, " groups)<br>Matched: ", matched_text, "</span>"
  )
  caption_md <- paste0(
    "<span style='color:steelblue'><b>Split:</b> ", n_orig, paste0(" cols ", .sym("arrow"), " "),
    split_min, "-", split_max, " cols<br>", split_eligible_text, "</span>"
  )

  graves_label <- .age_label(bias_result$graves_used %||% "adults")

  ttl <- if (!is.null(.title_override)) {
    .title_override
  } else {
    t <- paste0("Category Bias Simulation", graves_label)
    if (!is.null(case)  && nchar(case)  > 0) t <- paste(t, .sym("dash"), case)
    if (!is.null(dates) && nchar(dates) > 0) t <- paste(t, .sym("dash"), dates)
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
    Method = factor(c("TOT+1",paste0("TOT+1+", .sym("sqrt_count")),"Mean Prestige+1",paste0("Mean Prestige+1+", .sym("sqrt_count"))),
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
    # width = 0: only jitter vertically (for visual separation). x is the
    # real Gini value -- jittering it horizontally (the geom_jitter default)
    # would shift points outside [0, 1] and have them silently dropped by
    # the scale_x_continuous() limits below.
    ggplot2::geom_jitter(width = 0, height = 0.4, alpha = 0.5, size = 1.5) +
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
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    # coord_cartesian() zooms the displayed window without dropping any
    # data point that happens to fall outside it (unlike limits= inside
    # scale_x/y_continuous, which is destructive and silently removes rows).
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.5, 0.5)) +
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


#' Category bias simulation results for every group (one measure)
#'
#' Multi-group version of [plot_bias()]: one panel per group of a
#' [count_pipeline()] result, showing the simulated Gini values for a single
#' wealth measure. Points are the split (blue) and lump (coral) simulations,
#' the black line is the observed Gini, and the grey dashed lines mark the
#' interquartile range (middle 50%) of all simulations. The text in each
#' panel gives that IQR, the observed Gini, and how many columns the split
#' and lump scenarios produced.
#'
#' The x-axis (Gini, 0-1) is shared by all panels, so the spread caused by
#' category-recording decisions can be compared directly between groups.
#'
#' @param pipeline_result Output of [count_pipeline()] run with
#'   `run_bias_sim = TRUE`.
#' @param measure Which wealth measure to show: `"combined"` (default;
#'   prestige + sqrt-count bonus), `"prestige"` (prestige only), `"tot"`
#'   (TOT + 1) or `"tot_sqrt"` (TOT + 1 + sqrt-count bonus).
#' @param groups Optional character vector of group names (keys of
#'   `pipeline_result$by_group`) to include, in plotting order. Default NULL
#'   plots all groups in pipeline order.
#' @param ncol Integer. Number of panel columns (default 1, so the Gini
#'   axes line up vertically).
#' @return A [ggplot2::ggplot] object.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' plot_bias_groups(res)
#' plot_bias_groups(res, measure = "tot")
#' @export
plot_bias_groups <- function(pipeline_result,
                             measure = c("combined", "prestige", "tot", "tot_sqrt"),
                             groups = NULL, ncol = 1) {
  measure <- match.arg(measure)
  if (!inherits(pipeline_result, "qw_pipeline"))
    stop("pipeline_result must be the output of count_pipeline(). ",
         "For a single group or bias result, use plot_bias().")

  by_group <- pipeline_result$by_group
  if (!is.null(groups)) {
    unknown <- setdiff(groups, names(by_group))
    if (length(unknown) > 0)
      stop("Unknown group(s): ", paste(unknown, collapse = ", "),
           ". Available: ", paste(names(by_group), collapse = ", "))
    by_group <- by_group[groups]
  }

  has_bias <- vapply(by_group, function(g) !is.null(g$bias), logical(1))
  if (!any(has_bias))
    stop("No group has a bias simulation. Re-run count_pipeline(..., run_bias_sim = TRUE).")
  if (!all(has_bias))
    message("Skipping group(s) without a bias simulation: ",
            paste(names(by_group)[!has_bias], collapse = ", "))
  by_group <- by_group[has_bias]

  suffix <- switch(measure, combined = "prestige_sqrt", prestige = "prestige",
                   tot = "TOT", tot_sqrt = "TOT_sqrt")
  measure_label <- switch(measure,
    combined = paste0("Mean Prestige+1+", .sym("sqrt_count")),
    prestige = "Mean Prestige+1",
    tot      = "TOT+1",
    tot_sqrt = paste0("TOT+1+", .sym("sqrt_count"))
  )

  pts <- do.call(rbind, lapply(names(by_group), function(cs) {
    b <- by_group[[cs]]$bias
    split <- b[[paste0("ginis_split_", suffix)]]
    lump  <- b[[paste0("ginis_lump_",  suffix)]]
    data.frame(case = cs, Gini = c(split, lump),
               Direction = rep(c("Split", "Lump"), c(length(split), length(lump))))
  }))

  per_group <- do.call(rbind, lapply(names(by_group), function(cs) {
    b   <- by_group[[cs]]$bias
    all <- c(b[[paste0("ginis_split_", suffix)]], b[[paste0("ginis_lump_", suffix)]])
    q   <- stats::quantile(all, c(0.25, 0.75), na.rm = TRUE)
    og  <- b[[paste0("orig_gini_", suffix)]]
    data.frame(
      case = cs, orig_gini = og, iqr_low = q[1], iqr_high = q[2],
      label = paste0(
        "Observed: ", round(og, 3), " | IQR: [", round(q[1], 3), "-", round(q[2], 3), "]\n",
        "Split: ", b$n_original_cols, paste0(" ", .sym("arrow"), " "), min(b$total_splits), "-",
        max(b$total_splits), " cols | Lump: ", b$n_original_cols, paste0(" ", .sym("arrow"), " "),
        min(b$total_lumped_cols), "-", max(b$total_lumped_cols), " cols"
      ),
      x_pos = 0.01, y_pos = -0.42, row.names = NULL
    )
  }))

  pts$case       <- factor(pts$case, levels = names(by_group))
  pts$Direction  <- factor(pts$Direction, levels = c("Lump", "Split"))
  per_group$case <- factor(per_group$case, levels = names(by_group))

  graves <- unique(vapply(by_group, function(g) g$bias$graves_used %||% "adults",
                          character(1)))
  graves_label <- if (length(graves) == 1) {
    .age_label(graves)
  } else ""
  n_sims <- unique(vapply(by_group, function(g) as.numeric(g$bias$n_sims), numeric(1)))

  ggplot2::ggplot(pts, ggplot2::aes(x = Gini, y = 0, colour = Direction)) +
    ggplot2::geom_jitter(width = 0, height = 0.3, alpha = 0.5, size = 1.3) +
    ggplot2::geom_vline(data = per_group, ggplot2::aes(xintercept = orig_gini),
                        colour = "black", linewidth = 1) +
    ggplot2::geom_vline(data = per_group, ggplot2::aes(xintercept = iqr_low),
                        colour = "darkgray", linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_vline(data = per_group, ggplot2::aes(xintercept = iqr_high),
                        colour = "darkgray", linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_text(data = per_group,
                       ggplot2::aes(x = x_pos, y = y_pos, label = label),
                       inherit.aes = FALSE, hjust = 0, vjust = 0,
                       size = 2.5, colour = "gray40", lineheight = 0.9) +
    ggplot2::scale_colour_manual(values = c(Split = "steelblue", Lump = "coral")) +
    ggplot2::facet_wrap(~ case, ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.5, 0.5)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text         = ggplot2::element_text(size = 10, face = "bold"),
      plot.title         = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle      = ggplot2::element_text(size = 9, hjust = 0.5, colour = "gray40"),
      legend.position    = "bottom",
      axis.text.y        = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("Category Bias Simulation", graves_label, paste0(" ", .sym("dash"), " "), measure_label),
      subtitle = paste0(paste(n_sims, collapse = "/"), " iterations per group | ",
                        "Black = observed Gini | Gray dashed = IQR (middle 50%)"),
      x = "Gini", y = NULL, colour = "Direction"
    )
}


#' Plot wealth-score density curves across category bias simulations
#'
#' [plot_bias()] summarises each simulation as a single Gini value. This
#' function instead shows the full *shape* of the simulated wealth
#' distributions -- one faint density line per simulation, coloured by
#' whether categories were split or lumped in that simulation. Where the
#' lines overlap heavily, the distribution's shape is robust to recording
#' decisions; where they fan out, it is sensitive to them.
#'
#' Requires running [simulate_category_bias()] with `store_wealth = TRUE`,
#' which keeps per-grave wealth scores for every simulation. This is a large
#' object, so it is best used with modest `n_sims` and dataset sizes.
#'
#' The grave subset (`"adults"`, `"all"`, or `"subadults"`) is fixed when you
#' call [simulate_category_bias()] via its `graves` argument, not here -- this
#' function just labels which subset was used in the title. To compare
#' adults vs subadults, run [simulate_category_bias()] twice with different
#' `graves` values and plot each result separately.
#'
#' If `bias_result$wealth_scores$orig` is present (stored automatically by
#' [simulate_category_bias()] when `store_wealth = TRUE`), the observed
#' (non-simulated) distribution is overlaid as a bold black line, for
#' comparison against the simulated split/lump envelope.
#'
#' @param bias_result Output from [simulate_category_bias()] run with
#'   `store_wealth = TRUE`, or one group from `count_pipeline()$by_group`
#'   (its `$bias` is used).
#' @param measure Which wealth measure(s) to show: `"all"` (default, facets
#'   across all four measures used in [plot_bias()]), `"combined"`
#'   (prestige + sqrt-count bonus -- the main wealth measure), `"prestige"`
#'   (prestige only), or `"tot"` (TOT + 1).
#' @param case Character. Case label added to the title. When
#'   `bias_result` is a pipeline group and `case` is NULL, the group's label
#'   is used.
#' @param dates Character. Date range string added to the title.
#' @param alpha Numeric. Transparency of each simulation's density line
#'   (default 0.12). Lower values make the overall envelope easier to read
#'   when `n_sims` is large.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' set.seed(1)
#' bias <- simulate_category_bias(df, pres, n_sims = 30)
#' plot_bias_wealth_density(bias)
#' plot_bias_wealth_density(bias, measure = "combined")
#' @export
plot_bias_wealth_density <- function(bias_result,
                                      measure = c("all", "combined", "prestige", "tot"),
                                      case = NULL, dates = NULL, alpha = 0.12) {
  measure <- match.arg(measure)
  if (is.null(case) && inherits(bias_result, "qw_group")) case <- bias_result$case
  bias_result <- .as_bias_result(bias_result)
  if (is.null(bias_result$wealth_scores)) {
    stop("bias_result has no stored wealth scores. Re-run simulate_category_bias(",
         "..., store_wealth = TRUE) to enable this plot.")
  }

  label_map <- c(TOT = "TOT+1", TOT_sqrt = paste0("TOT+1+", .sym("sqrt_count")),
                 prestige = "Mean Prestige+1", prestige_sqrt = paste0("Mean Prestige+1+", .sym("sqrt_count")))

  build_long <- function(lst, direction) {
    do.call(rbind, lapply(seq_along(lst), function(i) {
      d <- lst[[i]]
      data.frame(
        sim           = i,
        direction     = direction,
        TOT           = d$TOT,
        TOT_sqrt      = d$TOT_sqrt,
        prestige      = d$prestige,
        prestige_sqrt = d$prestige_sqrt
      )
    }))
  }

  wide <- rbind(
    build_long(bias_result$wealth_scores$split, "Split"),
    build_long(bias_result$wealth_scores$lump,  "Lump")
  )
  wide$sim_id <- paste(wide$direction, wide$sim)

  has_orig <- !is.null(bias_result$wealth_scores$orig)
  if (!has_orig) {
    warning("bias_result has no stored original distribution (wealth_scores$orig). ",
            "Re-run simulate_category_bias(..., store_wealth = TRUE) to show it. ",
            "Plotting the simulations without it.")
  }

  if (measure == "all") {
    long <- tidyr::pivot_longer(wide, cols = c("TOT", "TOT_sqrt", "prestige", "prestige_sqrt"),
                                 names_to = "Method", values_to = "value")
    long$Method <- factor(label_map[long$Method],
                           levels = c("TOT+1", paste0("TOT+1+", .sym("sqrt_count")),
                                      "Mean Prestige+1", paste0("Mean Prestige+1+", .sym("sqrt_count"))))
    facet_layer <- ggplot2::facet_wrap(~ Method, ncol = 2, scales = "free")
    x_lab <- "Wealth value"

    if (has_orig) {
      orig_long <- tidyr::pivot_longer(bias_result$wealth_scores$orig,
                                        cols = c("TOT", "TOT_sqrt", "prestige", "prestige_sqrt"),
                                        names_to = "Method", values_to = "value")
      orig_long$Method <- factor(label_map[orig_long$Method], levels = levels(long$Method))
    }
  } else {
    col <- switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")
    long <- data.frame(sim_id = wide$sim_id, direction = wide$direction, value = wide[[col]])
    facet_layer <- NULL
    x_lab <- switch(measure,
      combined = paste0("Combined wealth (prestige + ", .sym("sqrt_count"), ")"),
      prestige = "Prestige only",
      tot      = "TOT + 1"
    )

    if (has_orig) {
      orig_long <- data.frame(value = bias_result$wealth_scores$orig[[col]])
    }
  }

  long$direction <- factor(long$direction, levels = c("Lump", "Split"))

  graves_label <- .age_label(bias_result$graves_used %||% "adults")

  ttl <- paste0("Wealth distribution across category bias simulations", graves_label)
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, .sym("dash"), dates)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = value, group = sim_id, colour = direction)) +
    ggplot2::stat_density(geom = "line", position = "identity",
                            alpha = alpha, linewidth = 0.4) +
    ggplot2::scale_colour_manual(values = c(Split = "steelblue", Lump = "coral")) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
    ) +
    ggplot2::labs(title = ttl, x = x_lab, y = "Density", colour = "Direction")

  if (has_orig) {
    p <- p + ggplot2::stat_density(
      data = orig_long, ggplot2::aes(x = value), inherit.aes = FALSE,
      geom = "line", position = "identity", colour = "black", linewidth = 1
    ) +
      ggplot2::labs(subtitle = "Black line = observed (original) distribution")
  }

  if (!is.null(facet_layer)) p <- p + facet_layer
  p
}


#' Wealth-score density curves across bias simulations, for every group
#'
#' Multi-group version of [plot_bias_wealth_density()]: one panel per group
#' of a [count_pipeline()] result, showing a single wealth measure. Each
#' panel has one faint density line per simulation, coloured by whether
#' categories were split or lumped, plus the observed distribution as a bold
#' black line. The point is to compare *how strongly* category-recording
#' decisions affect each group's distribution (how far the split/lump lines
#' fan out around the observed one), not the wealth values themselves.
#'
#' By default, axes are free per panel and show each group's own wealth
#' values. Prestige values are calculated separately within each group, so
#' a given wealth value does not mean the same thing in two panels.
#' With `relative = TRUE`, every group's scores (observed and simulated) are
#' divided by that group's observed mean wealth, so 1 = "average grave in
#' this group" in every panel and the axes can be shared. Dividing by the
#' *observed* mean (not each simulation's own mean) keeps any shift of the
#' simulated distributions caused by splitting/lumping visible.
#'
#' Note that density heights are in units of 1 / (x-axis units): with raw
#' values and a fixed y-axis, groups with a narrow wealth range get taller
#' curves than groups with a wide range, even if the shapes are identical.
#'
#' Requires `count_pipeline(..., run_bias_sim = TRUE)` with
#' `store_wealth = TRUE` (the default). Groups without stored wealth scores
#' are skipped with a message.
#'
#' @param pipeline_result Output of [count_pipeline()].
#' @param measure Which wealth measure to show: `"combined"` (default;
#'   prestige + sqrt-count bonus), `"prestige"` (prestige only), or `"tot"`
#'   (TOT + 1).
#' @param groups Optional character vector of group names (keys of
#'   `pipeline_result$by_group`) to include, in the order they should be
#'   plotted. Default NULL plots all groups in pipeline order.
#' @param alpha Numeric. Transparency of each simulation's density line
#'   (default 0.12).
#' @param relative Logical. If TRUE, divide each group's wealth scores by
#'   the group's observed mean wealth before plotting (default FALSE).
#' @param scales Axis scaling across panels, passed to
#'   [ggplot2::facet_wrap()]: `"free"`, `"free_x"` (fixed y-axis), `"free_y"`
#'   (fixed x-axis) or `"fixed"`. Default NULL means `"free"` for raw values
#'   and `"fixed"` when `relative = TRUE`.
#' @param ncol Integer. Number of panel columns (default NULL lets ggplot2
#'   choose).
#' @return A [ggplot2::ggplot] object.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE, n_bias_sims = 20, progress = FALSE)
#' plot_bias_wealth_density_groups(res)
#' plot_bias_wealth_density_groups(res, groups = c("A: A1", "B: B1"))
#' plot_bias_wealth_density_groups(res, relative = TRUE)   # shared relative scale
#' @export
plot_bias_wealth_density_groups <- function(pipeline_result,
                                             measure = c("combined", "prestige", "tot"),
                                             groups = NULL, alpha = 0.12,
                                             relative = FALSE, scales = NULL,
                                             ncol = NULL) {
  measure <- match.arg(measure)
  if (is.null(scales)) scales <- if (relative) "fixed" else "free"
  scales <- match.arg(scales, c("free", "free_x", "free_y", "fixed"))
  if (!inherits(pipeline_result, "qw_pipeline"))
    stop("pipeline_result must be the output of count_pipeline(). ",
         "For a single group or bias result, use plot_bias_wealth_density().")

  by_group <- pipeline_result$by_group
  if (!is.null(groups)) {
    unknown <- setdiff(groups, names(by_group))
    if (length(unknown) > 0)
      stop("Unknown group(s): ", paste(unknown, collapse = ", "),
           ". Available: ", paste(names(by_group), collapse = ", "))
    by_group <- by_group[groups]
  }

  has_ws <- vapply(by_group, function(g) !is.null(g$bias$wealth_scores), logical(1))
  if (!any(has_ws))
    stop("No group has stored bias wealth scores. Re-run count_pipeline(..., ",
         "run_bias_sim = TRUE, store_wealth = TRUE).")
  if (!all(has_ws))
    message("Skipping group(s) without stored bias wealth scores: ",
            paste(names(by_group)[!has_ws], collapse = ", "))
  by_group <- by_group[has_ws]

  col <- switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")

  # One row per grave per simulation per group
  sims <- do.call(rbind, lapply(names(by_group), function(cs) {
    ws <- by_group[[cs]]$bias$wealth_scores
    one_direction <- function(lst, direction) {
      do.call(rbind, lapply(seq_along(lst), function(i) {
        data.frame(case = cs, sim_id = paste(cs, direction, i),
                   direction = direction, value = lst[[i]][[col]])
      }))
    }
    rbind(one_direction(ws$split, "Split"), one_direction(ws$lump, "Lump"))
  }))
  orig <- do.call(rbind, lapply(names(by_group), function(cs) {
    o <- by_group[[cs]]$bias$wealth_scores$orig
    if (is.null(o)) return(NULL)
    data.frame(case = cs, value = o[[col]])
  }))

  if (relative) {
    for (cs in names(by_group)) {
      ref <- if (!is.null(orig) && any(orig$case == cs)) {
        mean(orig$value[orig$case == cs], na.rm = TRUE)
      } else {
        message("Group '", cs, "' has no stored observed distribution; ",
                "dividing by the mean of its simulated values instead.")
        mean(sims$value[sims$case == cs], na.rm = TRUE)
      }
      sims$value[sims$case == cs] <- sims$value[sims$case == cs] / ref
      if (!is.null(orig))
        orig$value[orig$case == cs] <- orig$value[orig$case == cs] / ref
    }
  }

  sims$case      <- factor(sims$case, levels = names(by_group))
  sims$direction <- factor(sims$direction, levels = c("Lump", "Split"))
  if (!is.null(orig)) orig$case <- factor(orig$case, levels = names(by_group))

  graves <- unique(vapply(by_group, function(g) g$bias$graves_used %||% "adults",
                          character(1)))
  graves_label <- if (length(graves) == 1) {
    .age_label(graves)
  } else ""
  x_lab <- switch(measure,
    combined = paste0("Combined wealth (prestige + ", .sym("sqrt_count"), ")"),
    prestige = "Prestige only",
    tot      = "TOT + 1"
  )
  if (relative) x_lab <- paste0(x_lab, ", relative to group mean (1 = mean)")
  sub <- switch(scales,
    free   = "Axes differ per panel",
    free_x = "x-axis differs per panel; y-axis shared",
    free_y = "x-axis shared; y-axis differs per panel",
    fixed  = "Axes shared across panels"
  )
  if (!is.null(orig)) sub <- paste0("Black line = observed distribution. ", sub)

  p <- ggplot2::ggplot(sims, ggplot2::aes(x = value, group = sim_id, colour = direction)) +
    ggplot2::stat_density(geom = "line", position = "identity",
                          alpha = alpha, linewidth = 0.4) +
    ggplot2::scale_colour_manual(values = c(Split = "steelblue", Lump = "coral")) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )) +
    ggplot2::facet_wrap(~ case, scales = scales, ncol = ncol) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(
      title = paste0("Wealth distribution across category bias simulations", graves_label),
      subtitle = sub, x = x_lab, y = "Density", colour = "Direction"
    )

  if (!is.null(orig)) {
    p <- p + ggplot2::stat_density(
      data = orig, ggplot2::aes(x = value), inherit.aes = FALSE,
      geom = "line", position = "identity", colour = "black", linewidth = 1
    )
  }
  p
}


#' Multiline Lorenz curve plot across category bias simulations
#'
#' Builds a Lorenz curve for each simulation stored in a
#' [simulate_category_bias()] run made with `store_wealth = TRUE`, and
#' overlays them as faint lines coloured by whether categories were split or
#' lumped in that simulation. This shows how category-recording decisions
#' affect the whole shape of inequality (not just the summary Gini plotted
#' by [plot_bias()]) -- where lines bow apart, the inequality picture itself
#' is sensitive to recording choices, not just its single Gini summary.
#'
#' Requires running [simulate_category_bias()] with `store_wealth = TRUE`,
#' which keeps per-grave wealth scores for every simulation. This is a large
#' object, so it is best used with modest `n_sims` and dataset sizes.
#'
#' The grave subset (`"adults"`, `"all"`, or `"subadults"`) is fixed when you
#' call [simulate_category_bias()] via its `graves` argument, not here -- this
#' function just labels which subset was used in the title. To compare
#' adults vs subadults, run [simulate_category_bias()] twice with different
#' `graves` values and plot each result separately.
#'
#' If `bias_result$wealth_scores$orig` is present (stored automatically by
#' [simulate_category_bias()] when `store_wealth = TRUE`), the observed
#' (non-simulated) distribution is overlaid as a bold black line, for
#' comparison against the simulated split/lump envelope. The grey diagonal
#' is the line of perfect equality, as usual for Lorenz curves.
#'
#' @param bias_result Output from [simulate_category_bias()] run with
#'   `store_wealth = TRUE`, or one group from `count_pipeline()$by_group`
#'   (its `$bias` is used).
#' @param measure Which wealth measure(s) to show: `"all"` (default, facets
#'   across all four measures used in [plot_bias()]), `"combined"`
#'   (prestige + sqrt-count bonus -- the main wealth measure), `"prestige"`
#'   (prestige only), or `"tot"` (TOT + 1).
#' @param case Character. Case label added to the title. When
#'   `bias_result` is a pipeline group and `case` is NULL, the group's label
#'   is used.
#' @param dates Character. Date range string added to the title.
#' @param alpha Numeric. Transparency of each simulation's Lorenz line
#'   (default 0.12). Lower values make the overall envelope easier to read
#'   when `n_sims` is large.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' set.seed(1)
#' bias <- simulate_category_bias(df, pres, n_sims = 30)
#' plot_bias_wealth_lorenz(bias, measure = "combined")
#' @export
plot_bias_wealth_lorenz <- function(bias_result,
                                     measure = c("all", "combined", "prestige", "tot"),
                                     case = NULL, dates = NULL, alpha = 0.12) {
  measure <- match.arg(measure)
  if (is.null(case) && inherits(bias_result, "qw_group")) case <- bias_result$case
  bias_result <- .as_bias_result(bias_result)
  if (is.null(bias_result$wealth_scores)) {
    stop("bias_result has no stored wealth scores. Re-run simulate_category_bias(",
         "..., store_wealth = TRUE) to enable this plot.")
  }

  label_map <- c(TOT = "TOT+1", TOT_sqrt = paste0("TOT+1+", .sym("sqrt_count")),
                 prestige = "Mean Prestige+1", prestige_sqrt = paste0("Mean Prestige+1+", .sym("sqrt_count")))

  .lorenz_coords <- function(x) {
    x  <- sort(x[!is.na(x)])
    n  <- length(x)
    cs <- cumsum(x) / sum(x)
    data.frame(cum_pop = seq(0, 1, length.out = n + 1),
               cum_wealth = c(0, cs))
  }

  build_lorenz <- function(lst, direction, cols) {
    do.call(rbind, lapply(seq_along(lst), function(i) {
      d <- lst[[i]]
      do.call(rbind, lapply(cols, function(col) {
        lc           <- .lorenz_coords(d[[col]])
        lc$Method    <- col
        lc$sim_id    <- paste(direction, i)
        lc$direction <- direction
        lc
      }))
    }))
  }

  has_orig <- !is.null(bias_result$wealth_scores$orig)
  if (!has_orig) {
    warning("bias_result has no stored original distribution (wealth_scores$orig). ",
            "Re-run simulate_category_bias(..., store_wealth = TRUE) to show it. ",
            "Plotting the simulations without it.")
  }

  cols <- if (measure == "all") c("TOT", "TOT_sqrt", "prestige", "prestige_sqrt")
          else switch(measure, combined = "prestige_sqrt", prestige = "prestige", tot = "TOT")

  long <- rbind(
    build_lorenz(bias_result$wealth_scores$split, "Split", cols),
    build_lorenz(bias_result$wealth_scores$lump,  "Lump",  cols)
  )

  if (has_orig) {
    orig_long <- build_lorenz(list(bias_result$wealth_scores$orig), "Original", cols)
  }

  if (measure == "all") {
    long$Method <- factor(label_map[long$Method],
                           levels = c("TOT+1", paste0("TOT+1+", .sym("sqrt_count")),
                                      "Mean Prestige+1", paste0("Mean Prestige+1+", .sym("sqrt_count"))))
    facet_layer <- ggplot2::facet_wrap(~ Method, ncol = 2)
    if (has_orig) orig_long$Method <- factor(label_map[orig_long$Method], levels = levels(long$Method))
  } else {
    facet_layer <- NULL
  }

  long$direction <- factor(long$direction, levels = c("Lump", "Split"))

  graves_label <- .age_label(bias_result$graves_used %||% "adults")

  ttl <- paste0("Lorenz curves across category bias simulations", graves_label)
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, .sym("dash"), dates)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = cum_pop, y = cum_wealth,
                                           group = sim_id, colour = direction)) +
    ggplot2::geom_line(alpha = alpha, linewidth = 0.4) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = c(Split = "steelblue", Lump = "coral")) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
    ) +
    ggplot2::labs(title = ttl, x = "Cumulative population share",
                  y = "Cumulative wealth share", colour = "Direction")

  if (has_orig) {
    p <- p + ggplot2::geom_line(
      data = orig_long, ggplot2::aes(x = cum_pop, y = cum_wealth),
      inherit.aes = FALSE, colour = "black", linewidth = 1
    ) +
      ggplot2::labs(subtitle = "Black line = observed (original) distribution")
  }

  if (!is.null(facet_layer)) p <- p + facet_layer
  p
}


#' Plot Lorenz curve for physical measure data
#' @param sizes Numeric vector of physical measure values.
#' @param ineq Optional output from [physical_measure_gini()].
#' @param data_type Character label for the measure type.
#' @param case Character. Case/site/period label.
#' @param dates Character. Date range string.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' areas <- c(40, 55, 60, 75, 80, 90, 120, 150, 200, 350)
#' plot_physical_lorenz(areas, data_type = "House floor area", case = "Example village")
#' @export
plot_physical_lorenz <- function(sizes, ineq = NULL, data_type = "Physical measure",
                                  case = NULL, dates = NULL) {
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  g_val <- if (!is.null(ineq)) ineq else physical_measure_gini(sizes, conf_level = NA)
  gini  <- round(g_val["gini"], 2)
  mean_s <- mean(sizes)
  abs_g  <- round(gini * mean_s, 2)
  ci_str <- if (!is.na(g_val["lower"] %||% NA))
    paste0(" \u00B1", round((g_val["upper"] - g_val["lower"]) / 2, 2)) else ""

  ttl <- paste(data_type, "Lorenz Curve")
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, .sym("dash"), dates)

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
#' @examples
#' areas <- c(40, 55, 60, 75, 80, 90, 120, 150, 200, 350)
#' plot_physical_density(areas, data_type = "House floor area")
#' @export
plot_physical_density <- function(sizes, data_type = "Physical measure",
                                   case = NULL, dates = NULL) {
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  ttl <- paste(data_type, "Distribution")
  if (!is.null(case)  && nchar(case)  > 0) ttl <- paste(ttl, .sym("dash"), case)
  if (!is.null(dates) && nchar(dates) > 0) ttl <- paste(ttl, .sym("dash"), dates)
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
#' @examples
#' areas <- c(40, 55, 60, 75, 80, 90, 120, 150, 200, 350)
#' plot_physical_decile(areas, data_type = "House floor area", unit = "m\u00b2")
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


#' Decile wealth distribution for every group of a pipeline result
#'
#' Group-wise version of [plot_decile()]: one stacked bar per group of a
#' [count_pipeline()] result, side by side, so the share of total wealth
#' held by each decile can be compared between groups (e.g. phases) and
#' datasets. The share scale (0-1) is unit-free, so bars from different
#' datasets are directly comparable. Numbers in the bars are the number of
#' graves in each decile.
#'
#' With fewer than about 30 graves a decile holds only 1-3 graves, and
#' graves with equal wealth are assigned to neighbouring deciles
#' arbitrarily ([dplyr::ntile()]), so the bars of small groups are
#' unstable; the n under each bar is there to keep that in view.
#'
#' @param pipeline_result Output of [count_pipeline()].
#' @param graves Which graves to use: `"adults"` (default), `"all"` or
#'   `"subadults"` (see [decile_data()]).
#' @param groups Optional character vector of group names (keys of
#'   `pipeline_result$by_group`) to include. Default NULL: all groups.
#' @param case_order Optional character vector of group labels (e.g. phase
#'   names) giving the left-to-right order; as in [plot_gini_comparison()].
#' @param show_counts Logical. Print the number of graves in each decile
#'   (default TRUE).
#' @param show_max Logical. Add the richest grave's wealth (combined) to the
#'   x label (default TRUE), so the shares can be read against how much
#'   wealth there is. Wealth values are only comparable within a dataset.
#' @param show_mean Logical. Also add the mean wealth to the x label
#'   (default FALSE). Mean x n = total wealth, so it rests on all graves,
#'   not just one.
#' @param title Optional plot title.
#' @return A [ggplot2::ggplot] object, one panel per dataset.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' plot_decile_groups(res)
#' plot_decile_groups(res, show_counts = FALSE, show_mean = TRUE)
#' @export
plot_decile_groups <- function(pipeline_result,
                               graves      = c("adults", "all", "subadults"),
                               groups      = NULL,
                               case_order  = NULL,
                               show_counts = TRUE,
                               show_max    = TRUE,
                               show_mean   = FALSE,
                               title       = NULL) {
  graves <- match.arg(graves)
  if (!inherits(pipeline_result, "qw_pipeline"))
    stop("pipeline_result must be the output of count_pipeline(). ",
         "For a single prestige table, use plot_decile().")

  by_group <- pipeline_result$by_group
  if (!is.null(groups)) {
    unknown <- setdiff(groups, names(by_group))
    if (length(unknown) > 0)
      stop("Unknown group(s): ", paste(unknown, collapse = ", "),
           ". Available: ", paste(names(by_group), collapse = ", "))
    by_group <- by_group[groups]
  }

  rows <- lapply(names(by_group), function(cs) {
    g  <- by_group[[cs]]
    pd <- g$prestige_df
    if (is.null(pd) || nrow(pd) == 0) return(NULL)
    dd <- tryCatch(decile_data(pd, graves = graves), error = function(e) NULL)
    if (is.null(dd) || nrow(dd) == 0) return(NULL)
    n  <- sum(dd$n_in_decile)
    has_age <- "is_adult_grave" %in% names(pd)
    w <- if (!has_age || graves == "all") pd$combined_total else
         if (graves == "adults") pd$combined_total[pd$is_adult_grave %in% TRUE] else
         pd$combined_total[pd$is_subadult_only %in% TRUE]
    dd$max_w  <- max(w, na.rm = TRUE)
    dd$mean_w <- mean(w, na.rm = TRUE)
    if (n < 10)
      message("Group '", cs, "' has only ", n, " graves: fewer than 10 deciles.")
    dd$dataset <- g$dataset %||% "all"
    dd$case    <- cs
    dd$group   <- as.character(g$group %||% cs)
    dd$n       <- n
    dd
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || nrow(df) == 0)
    stop("No graves available for graves = '", graves, "'.")

  # x labels "<group>\n(n=..)", ordered by group (case_order), then dataset
  lv <- .order_cases(df$group, case_order)
  df <- df[order(match(df$group, lv), df$dataset), , drop = FALSE]
  df$case_label <- sprintf("%s\n(n=%d)", df$group, as.integer(df$n))
  if (show_max)  df$case_label <- paste0(df$case_label, "\nmax: ",  signif(df$max_w, 3))
  if (show_mean) df$case_label <- paste0(df$case_label, "\nmean: ", signif(df$mean_w, 3))
  df$case_label <- factor(df$case_label, levels = unique(df$case_label))

  graves_label <- .age_label(graves)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = case_label, y = total_wealth,
                                        fill = wealth_rank)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_brewer(palette = "Spectral") +
    ggplot2::scale_y_continuous(limits = c(0, 1.0001), expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::labs(title = paste0(title %||% "Decile distribution by group", graves_label),
                  x = NULL, y = "Proportion of total wealth", fill = "Decile",
                  caption = paste(c(
                    if (show_counts) "Numbers: graves per decile",
                    if (show_max || show_mean)
                      "max/mean: combined wealth; comparable within a dataset only"),
                    collapse = "\n"))
  if (show_counts)
    p <- p + ggplot2::geom_text(ggplot2::aes(y = mid_point, label = n_in_decile),
                                size = 3, colour = "black")
  if (length(unique(df$dataset)) > 1)
    p <- p + ggplot2::facet_grid(~ dataset, scales = "free_x", space = "free_x")
  p
}


#' Heatmap of how often each grave-good type occurs, per group
#'
#' For every group of a [count_pipeline()] result, the share of graves that
#' contain each type (count > 0). Shows *which* types make up the typical
#' grave, e.g. to check whether a higher number of types per grave in one
#' dataset comes from the same kinds of finds as in another, or from
#' categories the other dataset does not record.
#'
#' Types are the count columns the pipeline used (after exclusions). Only
#' the `top_n` most frequent types per dataset are shown (ranked by their
#' highest share in any group of that dataset). One panel per dataset,
#' since datasets have different columns.
#'
#' @param pipeline_result Output of [count_pipeline()].
#' @param graves `"adults"` (default), `"all"` or `"subadults"`.
#' @param top_n Number of types shown per dataset (default 15).
#' @param groups Optional group names (keys of `pipeline_result$by_group`).
#' @param case_order Optional group labels giving the column order.
#' @param show_values Logical. Print the percentage in each cell (default TRUE).
#' @param x_angle Angle of the group labels on the x-axis (default 45, so
#'   labels of narrow panels do not overlap; 0 = horizontal).
#' @param title Optional plot title.
#' @return A [ggplot2::ggplot] object. The underlying table is attached as
#'   attribute `"prevalence"` (`attr(p, "prevalence")`).
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' plot_type_prevalence(res)
#' plot_type_prevalence(res, top_n = 10)
#' attr(plot_type_prevalence(res), "prevalence")[1:5, ]   # the numbers behind it
#' @export
plot_type_prevalence <- function(pipeline_result,
                                 graves      = c("adults", "all", "subadults"),
                                 top_n       = 15,
                                 groups      = NULL,
                                 case_order  = NULL,
                                 show_values = TRUE,
                                 x_angle     = 45,
                                 title       = NULL) {
  graves <- match.arg(graves)
  if (!inherits(pipeline_result, "qw_pipeline"))
    stop("pipeline_result must be the output of count_pipeline().")
  by_group <- pipeline_result$by_group
  if (!is.null(groups)) {
    unknown <- setdiff(groups, names(by_group))
    if (length(unknown) > 0)
      stop("Unknown group(s): ", paste(unknown, collapse = ", "))
    by_group <- by_group[groups]
  }

  rows <- lapply(names(by_group), function(cs) {
    g  <- by_group[[cs]]
    pd <- g$prestige_df
    cc <- intersect(g$count_cols, names(pd))
    if (is.null(pd) || length(cc) == 0) return(NULL)
    keep <- switch(graves,
      adults    = if ("is_adult_grave" %in% names(pd)) pd$is_adult_grave else rep(TRUE, nrow(pd)),
      subadults = if ("is_subadult_only" %in% names(pd)) pd$is_subadult_only else rep(FALSE, nrow(pd)),
      all       = rep(TRUE, nrow(pd)))
    pd <- pd[keep %in% TRUE, cc, drop = FALSE]
    if (nrow(pd) == 0) return(NULL)
    m <- as.matrix(pd); m[is.na(m)] <- 0
    data.frame(dataset = g$dataset %||% "all", case = cs,
               group = as.character(g$group %||% cs), n = nrow(pd),
               type = cc, share = colMeans(m > 0), stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || nrow(df) == 0)
    stop("No count columns found in prestige_df. Re-run count_pipeline() ",
         "with the current package version.")

  # top_n types per dataset, ranked by their highest share in any group
  df <- do.call(rbind, lapply(split(df, df$dataset), function(d) {
    rank_v <- tapply(d$share, d$type, max)
    top    <- names(sort(rank_v, decreasing = TRUE))[seq_len(min(top_n, length(rank_v)))]
    d <- d[d$type %in% top, ]
    d$type_key <- paste(d$dataset, d$type, sep = "___")
    d$type_ord <- match(d$type, rev(top))       # most frequent at the top
    d
  }))
  df$type_key <- factor(df$type_key, levels = unique(df$type_key[order(df$dataset, df$type_ord)]))

  lv <- .order_cases(df$group, case_order)
  df <- df[order(match(df$group, lv), df$dataset), , drop = FALSE]
  df$case_label <- factor(sprintf("%s\n(n=%d)", df$group, as.integer(df$n)),
                          levels = unique(sprintf("%s\n(n=%d)", df$group, as.integer(df$n))))

  graves_label <- .age_label(graves)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = case_label, y = type_key, fill = share)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08519c", limits = c(0, 1),
                                 labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = if (x_angle == 0) ggplot2::element_text() else
                     ggplot2::element_text(angle = x_angle, hjust = 1, vjust = 1)) +
    ggplot2::labs(title = paste0(title %||% "Share of graves containing each type", graves_label),
                  x = NULL, y = NULL, fill = "Graves\nwith type")
  if (show_values)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(100 * share),
                                             colour = share > 0.5),
                                size = 3, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"))
  if (length(unique(df$dataset)) > 1)
    p <- p + ggplot2::facet_wrap(~ dataset, scales = "free")
  attr(p, "prevalence") <- df[, c("dataset", "case", "group", "n", "type", "share")]
  p
}
