# ============================================================================
# Gini coefficients, Lorenz data, and decile distributions
# ============================================================================

#' Calculate Gini coefficient(s) for a wealth vector
#'
#' A thin wrapper around [DescTools::Gini()] that selects the confidence
#' interval method automatically: bootstrap CIs for n <= 500, point estimate
#' only for larger samples.
#'
#' @param x Numeric vector of wealth values (must be > 0 for meaningful
#'   results; the function adds 1 internally to handle zeros).
#' @param add_one Logical. If TRUE (default), adds 1 to all values before
#'   computing the Gini, so that zero-wealth observations are included
#'   without distorting the lower tail.
#' @param conf_level Numeric confidence level for the bootstrap CI
#'   (default 0.80, matching the app). Set to NA to skip CI.
#' @param n_boot Number of bootstrap replicates when n <= 500 (default 500).
#' @return Named numeric vector: `gini`, and (if CI computed) `lower`, `upper`.
#' @export
#' @examples
#' wealth <- c(1, 1, 2, 3, 5, 8, 13, 21)
#' calculate_gini(wealth)
calculate_gini <- function(x, add_one = TRUE, conf_level = 0.80, n_boot = 200) {
  x <- x[!is.na(x)]
  if (add_one) x <- x + 1
  if (length(x) < 2) {
    warning("Need at least 2 observations for Gini.")
    return(c(gini = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  if (!is.na(conf_level) && length(x) <= 500) {
    g <- DescTools::Gini(x, conf.level = conf_level,
                          unbiased = TRUE, type = "basic", R = n_boot)
    out <- c(gini = unname(g[1]), lower = unname(g[2]), upper = unname(g[3]))
  } else {
    g   <- DescTools::Gini(x, unbiased = TRUE)
    out <- c(gini = unname(g[1]), lower = NA_real_, upper = NA_real_)
  }
  out
}


#' Compute inequality summary for a prestige/wealth data frame
#'
#' Given the output of [calculate_prestige()], computes Gini coefficients for
#' three wealth measures and returns a tidy summary list.
#'
#' The three measures are:
#' * **combined** (`combined_total`): prestige + sqrt-count bonus -- the
#'   recommended main measure.
#' * **prestige** (`prestige_total`): prestige weights only (no quantity
#'   bonus).
#' * **TOT** (`raw_TOT + 1`): simple count of types present.
#'
#' When age-separation columns exist (`is_adult_grave`) the Gini is computed
#' on adult graves only; a separate Gini is computed for subadult-only graves
#' if at least 2 exist.
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param conf_level Numeric CI level (default 0.80). Set NA to skip CI.
#' @param n_boot Bootstrap replicates (default 500, used when n <= 500).
#' @return A list with elements:
#'   - `gini_combined`, `gini_prestige`, `gini_tot`: named numeric vectors
#'     from [calculate_gini()].
#'   - `abs_gini_combined`: absolute Gini = Gini x mean wealth.
#'   - `mean_wealth`, `median_wealth`, `min_wealth`, `max_wealth`: summary
#'     statistics of `combined_total`.
#'   - `n`: number of observations used.
#'   - `n_adult`, `n_subadult`: counts when age data present (else NA).
#'   - `gini_subadult`, `abs_gini_subadult`, `mean_wealth_subadult`:
#'     subadult results when available (else NA).
#' @export
#' @examples
#' df <- data.frame(
#'   id = paste0("G", 1:8),
#'   sword  = c(1, NA, 2, NA, 1, NA, 1, NA),
#'   shield = c(NA, 1, 1, NA, NA, 1, NA, 1),
#'   bronze = c(1, NA, NA, 1, 1, NA, NA, NA)
#' )
#' pres <- calculate_prestige(df)
#' inequality_summary(pres)
inequality_summary <- function(prestige_df, conf_level = 0.80, n_boot = 200) {

  has_age <- "is_adult_grave" %in% names(prestige_df)

  if (has_age) {
    adult_df   <- prestige_df[prestige_df$is_adult_grave, ]
    sub_df     <- prestige_df[prestige_df$is_subadult_only, ]
    main_df    <- adult_df
    n_adult    <- nrow(adult_df)
    n_subadult <- nrow(sub_df)
  } else {
    main_df    <- prestige_df
    n_adult    <- NA_integer_
    n_subadult <- NA_integer_
  }

  g_combined <- calculate_gini(main_df$combined_total, add_one = FALSE,
                                conf_level = conf_level, n_boot = n_boot)
  g_prestige <- calculate_gini(main_df$prestige_total, add_one = FALSE,
                                conf_level = conf_level, n_boot = n_boot)
  g_tot      <- calculate_gini(main_df$raw_TOT, add_one = TRUE,
                                conf_level = conf_level, n_boot = n_boot)

  mean_w   <- mean(main_df$combined_total, na.rm = TRUE)
  abs_gini <- round(g_combined["gini"] * mean_w, 2)

  # Subadult Gini
  if (has_age && nrow(sub_df) >= 2) {
    g_sub <- calculate_gini(sub_df$combined_total, add_one = FALSE,
                             conf_level = conf_level, n_boot = n_boot)
    mean_w_sub    <- mean(sub_df$combined_total, na.rm = TRUE)
    abs_gini_sub  <- round(g_sub["gini"] * mean_w_sub, 2)
  } else {
    g_sub         <- c(gini = NA_real_, lower = NA_real_, upper = NA_real_)
    mean_w_sub    <- NA_real_
    abs_gini_sub  <- NA_real_
  }

  list(
    gini_combined        = g_combined,
    gini_prestige        = g_prestige,
    gini_tot             = g_tot,
    abs_gini_combined    = unname(abs_gini),
    mean_wealth          = round(mean_w, 3),
    median_wealth        = round(median(main_df$combined_total, na.rm = TRUE), 3),
    min_wealth           = round(min(main_df$combined_total, na.rm = TRUE), 3),
    max_wealth           = round(max(main_df$combined_total, na.rm = TRUE), 3),
    n                    = nrow(main_df),
    n_adult              = n_adult,
    n_subadult           = n_subadult,
    gini_subadult        = g_sub,
    abs_gini_subadult    = unname(abs_gini_sub),
    mean_wealth_subadult = mean_w_sub
  )
}


#' Prepare Lorenz curve data for plotting
#'
#' Computes Lorenz curve coordinates (cumulative population share vs
#' cumulative wealth share) for each of the three wealth measures.
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param graves Which graves to use: `"adults"` (default, matching prior
#'   behaviour), `"all"`, or `"subadults"`. Ignored if `prestige_df` has no
#'   `is_adult_grave` column (age separation was not used).
#' @return A data frame with columns `cum_pop` (0-1), `cum_wealth` (0-1),
#'   and `measure` (factor: "combined", "prestige", "TOT+1").
#' @export
lorenz_data <- function(prestige_df, graves = c("adults", "all", "subadults")) {
  graves  <- match.arg(graves)
  has_age <- "is_adult_grave" %in% names(prestige_df)

  df <- if (!has_age || graves == "all") {
    prestige_df
  } else if (graves == "adults") {
    prestige_df[prestige_df$is_adult_grave, ]
  } else {
    prestige_df[prestige_df$is_subadult_only, ]
  }

  .lorenz_coords <- function(x) {
    x  <- sort(x[!is.na(x)])
    n  <- length(x)
    cs <- cumsum(x) / sum(x)
    data.frame(cum_pop = seq(0, 1, length.out = n + 1),
               cum_wealth = c(0, cs))
  }

  combined <- .lorenz_coords(df$combined_total)
  combined$measure <- "combined (prestige + \u221Acounts)"

  prestige <- .lorenz_coords(df$prestige_total)
  prestige$measure <- "prestige only"

  tot <- .lorenz_coords(df$raw_TOT + 1)
  tot$measure <- "TOT + 1"

  out        <- rbind(combined, prestige, tot)
  out$measure <- factor(out$measure,
                        levels = c("combined (prestige + \u221Acounts)",
                                   "prestige only", "TOT + 1"))
  out
}


#' Prepare decile distribution data for plotting
#'
#' Assigns each unit to a wealth decile and returns the share of total wealth
#' held by each decile.
#'
#' @param prestige_df Output from [calculate_prestige()].
#' @param graves Which graves to use: `"adults"` (default, matching prior
#'   behaviour), `"all"`, or `"subadults"`. Ignored if `prestige_df` has no
#'   `is_adult_grave` column (age separation was not used).
#' @return A data frame with columns `wealth_rank` (1-10, factor),
#'   `total_wealth` (share), `n_in_decile`, `cumulative_wealth`, `mid_point`.
#' @export
decile_data <- function(prestige_df, graves = c("adults", "all", "subadults")) {
  graves  <- match.arg(graves)
  has_age <- "is_adult_grave" %in% names(prestige_df)

  df <- if (!has_age || graves == "all") {
    prestige_df
  } else if (graves == "adults") {
    prestige_df[prestige_df$is_adult_grave, ]
  } else {
    prestige_df[prestige_df$is_subadult_only, ]
  }

  df |>
    dplyr::arrange(combined_total) |>
    dplyr::mutate(
      wealth_rank  = dplyr::ntile(combined_total, 10),
      wealth_share = combined_total / sum(combined_total, na.rm = TRUE)
    ) |>
    dplyr::group_by(wealth_rank) |>
    dplyr::summarise(
      total_wealth = sum(wealth_share),
      n_in_decile  = dplyr::n(),
      .groups      = "drop"
    ) |>
    dplyr::arrange(wealth_rank) |>
    dplyr::mutate(
      cumulative_wealth = cumsum(total_wealth),
      mid_point         = cumulative_wealth - total_wealth / 2,
      wealth_rank       = factor(wealth_rank, levels = rev(1:10))
    )
}


# Internal: drop NA / zero / negative measure values, emitting a message when
# any are removed, so the user is told that observations were excluded from the
# analysis. Shared by physical_measure_gini() and physical_measure_summary().
.drop_invalid_sizes <- function(sizes, caller = "physical_measure") {
  n_in  <- length(sizes)
  sizes <- sizes[!is.na(sizes) & sizes > 0]
  n_dropped <- n_in - length(sizes)
  if (n_dropped > 0) {
    message(sprintf(
      "%s(): %d of %d values dropped (zero, negative, or NA) before analysis.",
      caller, n_dropped, n_in))
  }
  sizes
}


#' Gini coefficient for physical measure data (e.g. house floor area)
#'
#' @param sizes Numeric vector of physical measure values (e.g. house areas
#'   in m^2).  All values must be positive.
#' @param conf_level Numeric CI level (default 0.80). Set NA to skip CI.
#' @param n_boot Bootstrap replicates (default 2000).
#' @return Named numeric vector: `gini`, `lower`, `upper`.
#' @export
#' @examples
#' areas <- c(40, 55, 60, 75, 80, 90, 120, 150, 200, 350)
#' physical_measure_gini(areas)
physical_measure_gini <- function(sizes, conf_level = 0.80, n_boot = 2000) {
  sizes <- .drop_invalid_sizes(sizes, "physical_measure_gini")
  if (length(sizes) < 2) stop("Need at least 2 valid (positive) observations.")

  if (!is.na(conf_level)) {
    g   <- DescTools::Gini(sizes, conf.level = conf_level,
                            unbiased = TRUE, type = "bca", R = n_boot)
    out <- c(gini = unname(g[1]), lower = unname(g[2]), upper = unname(g[3]))
  } else {
    g   <- DescTools::Gini(sizes, unbiased = TRUE)
    out <- c(gini = unname(g[1]), lower = NA_real_, upper = NA_real_)
  }
  out
}


#' Full inequality summary for physical measure data
#'
#' @param sizes Numeric vector (positive values, e.g. house areas in m2).
#' @param conf_level CI level (default 0.80). Set NA to skip CI.
#' @param n_boot Bootstrap replicates (default 2000).
#' @return A list with gini, abs_gini, mean, median, min, max, n.
#' @export
#' @examples
#' areas <- c(40, 55, 80, 120, 200, 350)
#' physical_measure_summary(areas)
physical_measure_summary <- function(sizes, conf_level = 0.80, n_boot = 2000) {
  sizes <- .drop_invalid_sizes(sizes, "physical_measure_summary")
  if (length(sizes) < 2) stop("Need at least 2 valid (positive) observations.")
  g <- physical_measure_gini(sizes, conf_level = conf_level, n_boot = n_boot)
  list(
    gini     = g,
    abs_gini = round(g["gini"] * mean(sizes), 2),
    mean     = round(mean(sizes), 3),
    median   = round(median(sizes), 3),
    min      = round(min(sizes), 3),
    max      = round(max(sizes), 3),
    n        = length(sizes)
  )
}
