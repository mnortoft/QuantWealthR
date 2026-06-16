# ============================================================================
# Prestige value calculation
# ============================================================================

#' Compute per-type TOT (Total Object Types) range table
#'
#' For each grave good type, records the TOT score (number of different types
#' present) of every grave that contains that type.  This long-format table is
#' the basis for all prestige calculations.
#'
#' @param df A data frame. First column = unit ID; remaining columns = numeric
#'   count data (use [identify_count_columns()] to check column selection).
#' @return A data frame with columns `Grave_ID`, `TOT`, and `Type`.
#' @export
#' @examples
#' df <- data.frame(
#'   id     = 1:6,
#'   sword  = c(1, NA, 2, NA, 1, NA),
#'   shield = c(NA, 1, 1, NA, NA, 1),
#'   bronze = c(1, NA, NA, 1, 1, NA)
#' )
#' get_TOT_range(df)
get_TOT_range <- function(df) {
  if (ncol(df) == 2) {
    df[, 2] <- as.numeric(df[, 2])
    df_bin  <- data.frame(
      Grave_ID = as.character(df[, 1]),
      type1    = ifelse(is.na(df[, 2]) | df[, 2] == 0, 0, 1)
    )
    names(df_bin)[2] <- names(df)[2]
    df_bin$TOT <- df_bin[, 2]
  } else {
    df[, -1]  <- lapply(df[, -1], as.numeric)
    df_bin    <- data.frame(
      Grave_ID = as.character(df[, 1]),
      ifelse(!is.na(df[, -1]) & df[, -1] > 0, 1, 0)
    )
    df_bin$TOT <- rowSums(df_bin[, -1, drop = FALSE])
  }

  type_cols <- setdiff(names(df_bin), c("Grave_ID", "TOT"))
  df_list <- lapply(type_cols, function(col) {
    rows <- df_bin[df_bin[[col]] == 1, c("Grave_ID", "TOT"), drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows$Type <- col
    rows
  })
  do.call(rbind, Filter(Negate(is.null), df_list))
}


#' Compute mean TOT per grave good type
#' @param tot_range Output from [get_TOT_range()].
#' @return Data frame with columns `Type` and `mean`.
#' @export
get_TOT_means <- function(tot_range) {
  tot_range |>
    dplyr::group_by(Type) |>
    dplyr::summarise(mean = mean(TOT), .groups = "drop")
}


#' Compute Bayesian (empirical prior) prestige estimates per type
#'
#' For types with n >= 2 occurrences, fits a Gamma prior using method of
#' moments from the type's own TOT distribution (local empirical prior).
#' For unique types (n = 1) a weak non-informative prior is used, and the
#' credible interval is narrowed proportionally to the total sample size.
#'
#' @param tot_range Output from [get_TOT_range()].
#' @return A data frame with columns `Type`, `bayes_est`, `lowerCI`,
#'   `upperCI`, `prior_type`, `item_n`, `observed_mean`.
#' @export
#' @examples
#' df <- data.frame(
#'   id = 1:10,
#'   sword  = c(1, NA, 1, 1, NA, NA, 1, NA, 1, NA),
#'   shield = c(1, 1, NA, 1, 1, NA, NA, NA, NA, 1),
#'   bronze = c(NA, NA, 1, NA, NA, 1, NA, NA, NA, NA)
#' )
#' tot <- get_TOT_range(df)
#' bayesian_prestige(tot)
bayesian_prestige <- function(tot_range) {
  types    <- unique(tot_range$Type)
  types    <- types[types != "TOT"]

  all_graves <- tot_range |>
    dplyr::group_by(Grave_ID) |>
    dplyr::summarise(TOT = dplyr::first(TOT), .groups = "drop")
  all_TOT <- all_graves$TOT
  N       <- length(all_TOT)

  rows <- lapply(types, function(t) {
    y <- tot_range$TOT[tot_range$Type == t]
    y <- y[!is.na(y)]
    n <- length(y)

    if (n == 0) {
      return(data.frame(Type = t, bayes_est = NA, lowerCI = NA, upperCI = NA,
                        prior_type = "no_data", item_n = 0, observed_mean = NA))
    }

    observed_mean <- mean(y)
    sum_y         <- sum(y)

    if (n >= 2) {
      M  <- observed_mean
      V  <- var(y)
      if (V == 0) V <- 0.1
      alpha0 <- M^2 / V
      beta0  <- M / V
      prior_type <- "local_empirical"
    } else {
      alpha0 <- 0.01
      beta0  <- 0.001
      prior_type <- "weak_prior"
    }

    alpha_post <- alpha0 + sum_y
    beta_post  <- beta0  + n
    post_mean  <- alpha_post / beta_post

    bayes_lower <- qgamma(0.025, shape = alpha_post, rate = beta_post)
    bayes_upper <- qgamma(0.975, shape = alpha_post, rate = beta_post)
    bayes_width <- bayes_upper - bayes_lower

    if (n == 1) {
      ratio      <- N * 0.001
      adjustment <- 1 / (1 + ratio)
      adj_lower  <- post_mean - bayes_width * adjustment / 2
      adj_upper  <- post_mean + bayes_width * adjustment / 2
    } else {
      adj_lower <- bayes_lower
      adj_upper <- bayes_upper
    }

    data.frame(
      Type          = t,
      bayes_est     = round(post_mean, 2),
      lowerCI       = round(adj_lower, 2),
      upperCI       = round(adj_upper, 2),
      prior_type    = prior_type,
      item_n        = n,
      observed_mean = round(observed_mean, 2)
    )
  })

  result <- do.call(rbind, rows)
  attr(result, "N")            <- N
  attr(result, "overall_mean") <- mean(all_TOT)
  result
}


#' Apply prestige weights to a grave goods data frame (mean method)
#'
#' Replaces each count > 0 with the mean TOT score of graves that contain
#' that type, then sums across types to produce `prestige_total`.
#'
#' @param means_df Output from [get_TOT_means()].
#' @param df The original data frame (first column = ID, rest = counts).
#' @return The data frame with grave good columns replaced by prestige weights
#'   and a `prestige_total` column appended.
#' @export
apply_prestige_mean <- function(means_df, df) {
  values <- stats::setNames(means_df$mean, means_df$Type)
  values <- round(values, 2)

  out <- df
  for (nm in names(values)) {
    if (!nm %in% names(out)) next
    out[[nm]] <- ifelse(!is.na(df[[nm]]) & df[[nm]] > 0, values[[nm]], 0)
  }
  out[is.na(out)] <- 0
  out$prestige_total <- round(rowSums(out[, -1, drop = FALSE], na.rm = TRUE), 2) + 1
  out
}


#' Apply prestige weights to a grave goods data frame (Bayesian method)
#'
#' Replaces each count > 0 with the Bayesian posterior mean for that type.
#'
#' @param bayes_df Output from [bayesian_prestige()].
#' @param df The original data frame (first column = ID, rest = counts).
#' @return Data frame with grave good columns as Bayesian prestige weights
#'   and a `prestige_total` column appended.
#' @export
apply_prestige_bayesian <- function(bayes_df, df) {
  out <- df
  for (nm in bayes_df$Type) {
    if (!nm %in% names(out)) next
    val <- round(bayes_df$bayes_est[bayes_df$Type == nm], 2)
    out[[nm]] <- ifelse(!is.na(df[[nm]]) & df[[nm]] > 0, val, 0)
  }
  out[is.na(out)] <- 0
  out$prestige_total <- round(rowSums(out[, -1, drop = FALSE], na.rm = TRUE), 2) + 1
  out
}


#' Compute sqrt-count bonus for each unit
#'
#' For each unit, sums sqrt(count) - 1 across all types where count >= 1.
#' This bonus accounts for quantity (e.g. 5 swords > 1 sword) without
#' letting counts dominate.
#'
#' @param df Data frame. First column = ID, remaining columns = counts.
#' @param count_cols Character vector of column names to include (defaults to
#'   all numeric columns identified by [identify_count_columns()]).
#' @return Numeric vector of length `nrow(df)`.
#' @export
sqrt_count_bonus <- function(df, count_cols = NULL) {
  if (is.null(count_cols)) count_cols <- identify_count_columns(df)
  mat <- as.matrix(df[, count_cols, drop = FALSE])
  mat[is.na(mat)] <- 0
  rowSums(ifelse(mat >= 1, sqrt(mat) - 1, 0))
}


#' Full prestige calculation pipeline for count data
#'
#' Combines TOT calculation, prestige estimation (mean or Bayesian), and
#' sqrt-count bonus into a single function.  Returns the original data frame
#' with added columns `raw_TOT`, `prestige_total`, and `combined_total`
#' (= prestige_total + sqrt bonus).
#'
#' If `adult_count` / `subadult_count` columns are present and `use_age_sep`
#' is TRUE, prestige weights are estimated from adult graves only, and
#' `combined_total` is divided by the number of adults (or subadults for
#' subadult-only graves) to give per-capita wealth.
#'
#' @param df Data frame. First column = unit ID; subsequent columns = numeric
#'   counts.  May include optional metadata columns: `adult_count`,
#'   `subadult_count`, `lat`, `lng`.
#' @param method `"bayesian"` (default) or `"mean"`.
#' @param use_age_sep Logical.  If TRUE and the data contains `adult_count` /
#'   `subadult_count` columns, enables age-separated prestige estimation and
#'   per-capita wealth. Default TRUE.
#' @return The original data frame plus columns `raw_TOT`, `prestige_total`,
#'   `combined_total`, and (if age separation is active) `is_adult_grave`,
#'   `is_subadult_only`.
#' @export
#' @examples
#' df <- data.frame(
#'   id     = paste0("G", 1:8),
#'   sword  = c(1, NA, 2, NA, 1, NA, 1, NA),
#'   shield = c(NA, 1, 1, NA, NA, 1, NA, 1),
#'   bronze = c(1, NA, NA, 1, 1, NA, NA, NA)
#' )
#' result <- calculate_prestige(df)
#' result[, c("id", "raw_TOT", "prestige_total", "combined_total")]
calculate_prestige <- function(df, method = "bayesian", use_age_sep = TRUE) {

  method <- match.arg(method, c("bayesian", "mean"))

  col_names_lower <- tolower(names(df))
  has_adult   <- "adult_count"   %in% col_names_lower
  has_sub     <- "subadult_count" %in% col_names_lower
  use_age_sep <- use_age_sep && (has_adult || has_sub)

  # Identify count columns
  count_cols <- identify_count_columns(df)
  if (length(count_cols) == 0) stop("No numeric count columns found in `df`.")
  count_cols_with_data <- count_cols[sapply(count_cols, function(col)
    any(df[[col]] > 0, na.rm = TRUE))]
  if (length(count_cols_with_data) == 0)
    stop("All count columns contain only zeros or NAs.")
  count_cols <- count_cols_with_data

  df_counts <- df[, c(names(df)[1], count_cols), drop = FALSE]

  # Age separation
  if (use_age_sep) {
    adult_col <- names(df)[col_names_lower == "adult_count"][1]
    sub_col   <- names(df)[col_names_lower == "subadult_count"]
    sub_col   <- if (length(sub_col) > 0) sub_col[1] else NULL

    adult_counts   <- as.numeric(df[[adult_col]])
    adult_counts[is.na(adult_counts)] <- 0
    subadult_counts <- if (!is.null(sub_col)) {
      sc <- as.numeric(df[[sub_col]])
      sc[is.na(sc)] <- 0
      sc
    } else rep(0, nrow(df))

    is_adult_grave   <- adult_counts >= 1 | (adult_counts == 0 & subadult_counts == 0)
    is_subadult_only <- adult_counts == 0 & subadult_counts >= 1

    # TOT range for adults only
    df_adults  <- df_counts[is_adult_grave, , drop = FALSE]
    tot_adults <- get_TOT_range(df_adults)

    # TOT range for subadult-exclusive types
    df_subs <- df_counts[is_subadult_only, , drop = FALSE]
    adult_types <- if (!is.null(tot_adults) && nrow(tot_adults) > 0)
      unique(tot_adults$Type) else character(0)

    tot_subs <- if (nrow(df_subs) > 0) {
      ts <- get_TOT_range(df_subs)
      if (!is.null(ts) && nrow(ts) > 0) {
        ts[!ts$Type %in% adult_types, , drop = FALSE]
      } else NULL
    } else NULL

    # Prestige weights
    if (method == "bayesian") {
      bayes_df <- bayesian_prestige(tot_adults)
      if (!is.null(tot_subs) && nrow(tot_subs) > 0)
        bayes_df <- rbind(bayes_df, bayesian_prestige(tot_subs))
      out <- apply_prestige_bayesian(bayes_df, df_counts)
    } else {
      means_df <- get_TOT_means(tot_adults)
      if (!is.null(tot_subs) && nrow(tot_subs) > 0)
        means_df <- rbind(means_df, get_TOT_means(tot_subs))
      out <- apply_prestige_mean(means_df, df_counts)
    }

    # sqrt bonus
    bonus <- sqrt_count_bonus(df_counts, count_cols)
    out$combined_total <- out$prestige_total + bonus

    # Per-capita division
    ac_safe <- pmax(adult_counts, 1)
    sc_safe <- pmax(subadult_counts, 1)
    out$combined_total[is_adult_grave] <-
      out$combined_total[is_adult_grave] / ac_safe[is_adult_grave]
    out$combined_total[is_subadult_only] <-
      out$combined_total[is_subadult_only] / sc_safe[is_subadult_only]
    out$prestige_total[is_adult_grave] <-
      out$prestige_total[is_adult_grave] / ac_safe[is_adult_grave]
    out$prestige_total[is_subadult_only] <-
      out$prestige_total[is_subadult_only] / sc_safe[is_subadult_only]

    out$is_adult_grave   <- is_adult_grave
    out$is_subadult_only <- is_subadult_only
    out$adult_count      <- adult_counts
    out$subadult_count   <- subadult_counts

    # Store plot data as attributes (mirrors app reactives)
    sub_type_names <- if (!is.null(tot_subs) && nrow(tot_subs) > 0)
      unique(tot_subs$Type) else character(0)

    # Plot TOT range: adult types from adult graves + subadult-exclusive from sub graves
    plot_tot_range <- if (!is.null(tot_subs) && nrow(tot_subs) > 0)
      rbind(tot_adults, tot_subs) else tot_adults

    plot_bayes <- if (method == "bayesian") bayes_df else NULL
    plot_means <- if (method == "mean")    {
      m <- get_TOT_means(tot_adults)
      if (!is.null(tot_subs) && nrow(tot_subs) > 0)
        m <- rbind(m, get_TOT_means(tot_subs))
      m
    } else NULL

  } else {
    # No age separation
    tot_range <- get_TOT_range(df_counts)
    if (method == "bayesian") {
      bayes_df <- bayesian_prestige(tot_range)
      out      <- apply_prestige_bayesian(bayes_df, df_counts)
    } else {
    }
    bonus <- sqrt_count_bonus(df_counts, count_cols)
    out$combined_total <- out$prestige_total + bonus

    sub_type_names <- character(0)
    plot_tot_range <- tot_range
    plot_bayes     <- if (method == "bayesian") bayes_df else NULL
    plot_means     <- if (method == "mean")     means_df  else NULL
  }

  raw_tot <- rowSums(!is.na(df[, count_cols, drop = FALSE]) &
                       df[, count_cols, drop = FALSE] > 0)

  final <- df
  for (col in count_cols) final[[col]] <- out[[col]]
  final$raw_TOT        <- raw_tot
  final$prestige_total <- out$prestige_total
  final$combined_total <- out$combined_total
  if ("is_adult_grave" %in% names(out)) {
    final$is_adult_grave   <- out$is_adult_grave
    final$is_subadult_only <- out$is_subadult_only
  }

  attr(final, "plot_tot_range")  <- plot_tot_range
  attr(final, "plot_bayes")      <- plot_bayes
  attr(final, "plot_means")      <- plot_means
  attr(final, "subadult_types")  <- sub_type_names
  attr(final, "method")          <- method

  final
}
