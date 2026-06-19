# ============================================================================
# Batch pipeline: run all analyses per group (phase / period)
# ============================================================================

#' Run the full QuantWealth analysis pipeline on a grouped dataset
#'
#' Given a data frame with a grouping column (e.g. period, phase, site), this
#' function runs the complete analysis pipeline for each group and collects
#' results in a convenient list.  Supports both count data (grave goods, hoard
#' types, house objects) and physical measure data (house area, mound volume,
#' etc.).
#'
#' **What is computed per group:**
#' 1. Prestige values (`calculate_prestige()`) -- count data only.
#' 2. Inequality summary (`inequality_summary()`) -- count data only.
#' 3. Category bias simulation (`simulate_category_bias()`) -- count data only,
#'    only when `run_bias_sim = TRUE`.
#' 4. Physical measure Gini (`physical_measure_gini()`) -- physical data only.
#'
#' @param df A data frame.  Must contain:
#'   * A grouping column named by the `group_col` argument.
#'   * Either count columns (first non-group column = ID, rest = numeric
#'     counts) **or** a single numeric column for physical measures (set
#'     `data_type = "physical"`).
#' @param group_col Character. Name of the column to group by (e.g.
#'   `"phase"`, `"period"`, `"site"`).  Make this column an ordered/explicit
#'   factor to control the order of groups in the output and plots (e.g.
#'   chronological phases); otherwise groups follow their order of first
#'   appearance in the data.
#' @param id_col Character. Name of the unit ID column (e.g. `"grave_id"`).
#'   If NULL (default), the first non-group column is used as the ID.
#' @param data_type `"count"` (default) for grave goods / hoard / house
#'   object data, or `"physical"` for a single numeric measure column.
#' @param measure_col For `data_type = "physical"`: name of the column
#'   containing the measure values (e.g. `"area_m2"`). Required when
#'   `data_type = "physical"`.
#' @param method Prestige method: `"bayesian"` (default) or `"mean"`. Ignored
#'   for physical data.
#' @param use_age_sep Logical. Enable age separation if `adult_count` /
#'   `subadult_count` columns are present. Default TRUE.
#' @param run_bias_sim Logical. Run category bias simulation per group.
#'   Can be slow for many groups or large datasets. Default FALSE.
#' @param n_bias_sims Integer. Number of bias simulations per group
#'   (default 200). Ignored when `run_bias_sim = FALSE`.
#' @param conf_level Numeric CI level for Gini coefficients (default 0.80).
#' @param min_group_size Integer. Groups with fewer observations than this are
#'   skipped with a warning (default 3).
#' @return A list with two elements:
#'   * `$summary`: a data frame with one row per group and columns for all
#'     key inequality statistics.
#'   * `$by_group`: a named list (one element per group) containing the full
#'     output of each analysis step.
#' @export
#' @examples
#' \dontrun{
#' # Count data example -- multiple Bronze Age phases
#' df <- data.frame(
#'   phase  = rep(c("Early", "Middle", "Late"), each = 15),
#'   grave  = paste0("G", 1:45),
#'   sword  = sample(c(NA, 1:3), 45, replace = TRUE),
#'   shield = sample(c(NA, 1:2), 45, replace = TRUE),
#'   bronze = sample(c(NA, 1:4), 45, replace = TRUE)
#' )
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave")
#' result$summary
#'
#' # Physical measure example -- house areas across phases
#' houses <- data.frame(
#'   phase   = rep(c("Phase 1", "Phase 2"), each = 20),
#'   house   = paste0("H", 1:40),
#'   area_m2 = c(rlnorm(20, 4, 0.5), rlnorm(20, 4.5, 0.7))
#' )
#' result_phys <- qw_pipeline(houses, group_col = "phase", id_col = "house",
#'                             data_type = "physical", measure_col = "area_m2")
#' result_phys$summary
#' }
qw_pipeline <- function(df,
                          group_col,
                          id_col          = NULL,
                          data_type       = c("count", "physical"),
                          measure_col     = NULL,
                          method          = c("bayesian", "mean"),
                          use_age_sep     = TRUE,
                          run_bias_sim    = FALSE,
                          n_bias_sims     = 200,
                          conf_level      = 0.80,
                          min_group_size  = 3) {

  data_type <- match.arg(data_type)
  method    <- match.arg(method)

  if (!group_col %in% names(df))
    stop(sprintf("Column '%s' not found in data frame.", group_col))

  if (data_type == "physical") {
    if (is.null(measure_col))
      stop("'measure_col' must be specified when data_type = 'physical'.")
    if (!measure_col %in% names(df))
      stop(sprintf("Column '%s' not found in data frame.", measure_col))
  }

  # Preserve a meaningful group order: factor-level order when the grouping
  # column is a factor (so chronological order can be set explicitly), else
  # the order the groups first appear in the data.
  groups    <- if (is.factor(df[[group_col]])) {
    levels(droplevels(df[[group_col]]))
  } else {
    unique(df[[group_col]])
  }
  by_group  <- list()
  rows_list <- list()

  for (grp in groups) {
    grp_df <- df[df[[group_col]] == grp, , drop = FALSE]
    n_obs  <- nrow(grp_df)

    if (n_obs < min_group_size) {
      warning(sprintf("Group '%s' has only %d observations (< %d). Skipping.",
                      grp, n_obs, min_group_size))
      next
    }

    grp_result <- list(group = grp, n = n_obs)

    tryCatch({
      if (data_type == "count") {

        # Determine ID column
        non_group_cols <- setdiff(names(grp_df), group_col)
        id_c <- if (!is.null(id_col) && id_col %in% names(grp_df))
          id_col else non_group_cols[1]

        # Build sub-frame: ID col + count cols
        count_cols <- identify_count_columns(grp_df[, non_group_cols, drop = FALSE])
        count_cols <- setdiff(count_cols, group_col)

        if (length(count_cols) == 0) {
          warning(sprintf("Group '%s': no count columns found. Skipping.", grp))
          next
        }

        extra_cols <- intersect(names(grp_df),
                                 c("adult_count", "subadult_count", "lat", "lng"))
        sub_df <- grp_df[, unique(c(id_c, count_cols, extra_cols)), drop = FALSE]

        # 1. Prestige
        pres_df <- calculate_prestige(sub_df, method = method,
                                       use_age_sep = use_age_sep)
        grp_result$prestige_df <- pres_df

        # 2. Inequality
        ineq <- inequality_summary(pres_df, conf_level = conf_level)
        grp_result$inequality <- ineq

        # 3. Bias simulation (optional)
        if (run_bias_sim) {
          bias <- simulate_category_bias(sub_df, n_sims = n_bias_sims)
          grp_result$bias <- bias
        }

        # Build summary row
        g  <- ineq$gini_combined
        ci <- if (!is.na(g["lower"])) {
          paste0("[", round(g["lower"], 3), "\u2013", round(g["upper"], 3), "]")
        } else NA_character_

        row <- data.frame(
          group               = as.character(grp),
          n                   = ineq$n,
          n_adult             = ineq$n_adult %||% NA_integer_,
          n_subadult          = ineq$n_subadult %||% NA_integer_,
          gini_combined       = round(g["gini"], 3),
          gini_ci             = ci,
          abs_gini            = ineq$abs_gini_combined,
          mean_wealth         = ineq$mean_wealth,
          median_wealth       = ineq$median_wealth,
          gini_prestige       = round(ineq$gini_prestige["gini"], 3),
          gini_tot            = round(ineq$gini_tot["gini"], 3),
          gini_subadult       = round(ineq$gini_subadult["gini"], 3),
          abs_gini_subadult   = ineq$abs_gini_subadult %||% NA_real_,
          stringsAsFactors    = FALSE,
          row.names           = NULL,
          check.names         = FALSE
        )

      } else {
        # Physical measure
        sizes <- as.numeric(grp_df[[measure_col]])
        sizes <- sizes[!is.na(sizes) & sizes > 0]

        if (length(sizes) < 2) {
          warning(sprintf("Group '%s': fewer than 2 valid measure values. Skipping.", grp))
          next
        }

        g <- physical_measure_gini(sizes, conf_level = conf_level)
        ci <- if (!is.na(g["lower"])) {
          paste0("[", round(g["lower"], 3), "\u2013", round(g["upper"], 3), "]")
        } else NA_character_

        grp_result$sizes <- sizes
        grp_result$gini  <- g

        row <- data.frame(
          group         = as.character(grp),
          n             = length(sizes),
          gini          = round(g["gini"], 3),
          gini_ci       = ci,
          abs_gini      = round(unname(g["gini"]) * mean(sizes), 2),
          mean_measure  = round(mean(sizes), 3),
          median_measure = round(median(sizes), 3),
          min_measure   = round(min(sizes), 3),
          max_measure   = round(max(sizes), 3),
          stringsAsFactors = FALSE,
          row.names        = NULL,
          check.names      = FALSE
        )
      }

      by_group[[as.character(grp)]] <- grp_result
      rows_list[[length(rows_list) + 1]] <- row

    }, error = function(e) {
      warning(sprintf("Group '%s' failed: %s", grp, e$message))
    })
  }

  summary_df <- if (length(rows_list) > 0)
    do.call(rbind, rows_list) else data.frame()

  list(summary = summary_df, by_gr