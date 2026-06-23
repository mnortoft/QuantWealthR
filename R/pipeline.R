# ============================================================================
# Batch pipelines: run all analyses per group (phase / period), for one or
# more datasets at once
# ============================================================================

#' Normalise a `count_pipeline()`/`phys_pipeline()` first argument
#'
#' Accepts either a single data frame or a named list of data frames (one
#' per dataset/site being compared) and returns a consistently-named list
#' plus a flag for whether dataset tagging should be added to the output.
#' Unnamed list elements get auto-generated names ("Dataset1", "Dataset2",
#' ...) so every dataset is always addressable.
#'
#' Each dataset keeps its own columns -- nothing is column-unioned or
#' rbound here. Datasets are analysed completely independently downstream,
#' which is what makes differing grave-good columns across datasets a
#' non-issue: there's never a combined matrix where an absent column would
#' need to be filled with NA (or wrongly with 0).
#'
#' @param data A data frame, or a (possibly unnamed/partially named) list
#'   of data frames.
#' @return A list with `datasets` (named list of data frames) and `multi`
#'   (logical: TRUE if `data` was a list, even of length 1 -- a list input
#'   signals the caller wants dataset tracking even when there's only one).
#' @noRd
.normalize_dataset_list <- function(data) {
  if (is.data.frame(data)) {
    return(list(datasets = list(data1 = data), multi = FALSE))
  }
  if (!is.list(data) || length(data) == 0)
    stop("'data' must be a data frame or a named list of data frames.")
  if (!all(vapply(data, is.data.frame, logical(1))))
    stop("Every element of 'data' must be a data frame.")

  nms <- names(data)
  if (is.null(nms)) nms <- rep("", length(data))
  missing <- nms == "" | is.na(nms)
  if (any(missing))
    nms[missing] <- paste0("Dataset", seq_along(nms))[missing]
  if (any(duplicated(nms)))
    stop("Dataset names must be unique: ", paste(nms[duplicated(nms)], collapse = ", "))

  names(data) <- nms
  list(datasets = data, multi = TRUE)
}


#' Run the count-data QuantWealth analysis pipeline on one or more datasets
#'
#' Given a data frame (or a named list of data frames, when comparing
#' datasets that may have different grave-good columns, different group
#' names, or different numbers of groups) with a grouping column in each
#' (e.g. period, phase, site), this runs the complete count-data analysis
#' for every group in every dataset and collects results in one combined
#' object.
#'
#' **What is computed per group:**
#' 1. Prestige values ([calculate_prestige()]).
#' 2. Inequality summary ([inequality_summary()]), including the full
#'    measure x age-group `grid` (see that function's docs).
#' 3. Category bias simulation ([simulate_category_bias()]), only when
#'    `run_bias_sim = TRUE`.
#'
#' For physical measure data (house area, mound volume, etc.), use
#' [phys_pipeline()] instead -- it has its own dedicated pipeline rather
#' than a shared `data_type` switch, since the two kinds of data need
#' different columns and produce different summary statistics.
#'
#' @param data A data frame, **or** a named list of data frames (e.g.
#'   `list(SiteA = df1, SiteB = df2)`) to analyse and compare side by side.
#'   Each dataset is analysed entirely on its own terms -- with its own
#'   grave-good columns, its own number of groups, even reused group names
#'   (e.g. two sites that each have an "Early" phase) -- since nothing is
#'   merged across datasets before analysis. Unnamed list elements are
#'   auto-labelled "Dataset1", "Dataset2", etc.
#' @param group_col Character. Name of the column to group by (e.g.
#'   `"phase"`, `"period"`, `"site"`). Make this column an ordered/explicit
#'   factor to control the order of groups in the output and plots (e.g.
#'   chronological phases); otherwise groups follow their order of first
#'   appearance in the data. The same `group_col` name is used for every
#'   dataset when `data` is a list.
#' @param id_col Character. Name of the unit ID column (e.g. `"grave_id"`).
#'   If NULL (default), the first non-group column is used as the ID, in
#'   each dataset.
#' @param date_col Character, optional. Name of a numeric column giving an
#'   absolute calendar date for each row (negative = BCE, positive = CE),
#'   e.g. a calibrated radiocarbon midpoint. If supplied and present in a
#'   dataset, the mean (and min/max) of this column within each group is
#'   computed and stored as `date`/`date_min`/`date_max` in `$summary` and
#'   `$by_group`, on top of everything else this function already computes.
#'   This does not change any wealth/Gini calculation -- it only carries
#'   chronological information through so it's available later, e.g. for
#'   [plot_gini_timeline()]. Default `NULL` (no date column; the date
#'   fields are simply absent from the output, exactly as if this argument
#'   didn't exist). Rows with a missing/non-numeric value in `date_col` are
#'   dropped only for this date computation, not from the rest of the
#'   analysis.
#' @param method Prestige method: `"bayesian"` (default) or `"mean"`.
#' @param use_age_sep Logical. Enable age separation if `adult_count` /
#'   `subadult_count` columns are present. Default TRUE.
#' @param run_bias_sim Logical. Run category bias simulation per group.
#'   Can be slow for many groups or large datasets. Default FALSE. When
#'   TRUE, the summary's `abs_gini_ci` column is also populated: a CI for
#'   the Absolute Gini (G x mean wealth) taken from quantiles of
#'   `gini x mean` across the simulation's split- and lump-scenario draws
#'   (pooled), at the same `conf_level`. This is a sensitivity range across
#'   plausible category-recording decisions, not a sampling-uncertainty
#'   interval like `gini_ci` -- see [simulate_category_bias()].
#' @param n_bias_sims Integer. Number of bias simulations per group
#'   (default 200). Ignored when `run_bias_sim = FALSE`.
#' @param conf_level Numeric CI level for Gini coefficients (default 0.80).
#'   Also used for `abs_gini_ci` when `run_bias_sim = TRUE`.
#' @param min_group_size Integer. Groups with fewer observations than this are
#'   skipped with a warning (default 3).
#' @return A list with two elements:
#'   * `$summary`: a data frame with one row per group (per dataset, when
#'     multiple). When `data` is a single data frame, this matches the
#'     previous `qw_pipeline()` count-data output exactly. When `data` is a
#'     list, two extra columns are added: `dataset` (the list name) and
#'     `case` (`"<dataset>: <group>"`), the latter used as the unique row
#'     and `$by_group` key so identical group names across datasets never
#'     collide.
#'   * `$by_group`: a named list (keyed by `case`, or by `group` when `data`
#'     was a single data frame) containing the full output of each
#'     analysis step, including `inequality$grid`.
#' @export
#' @examples
#' \dontrun{
#' # Single dataset -- multiple Bronze Age phases
#' df <- data.frame(
#'   phase  = rep(c("Early", "Middle", "Late"), each = 15),
#'   grave  = paste0("G", 1:45),
#'   sword  = sample(c(NA, 1:3), 45, replace = TRUE),
#'   shield = sample(c(NA, 1:2), 45, replace = TRUE),
#'   bronze = sample(c(NA, 1:4), 45, replace = TRUE)
#' )
#' result <- count_pipeline(df, group_col = "phase", id_col = "grave")
#' result$summary
#'
#' # Multiple datasets with different grave-good columns
#' result2 <- count_pipeline(list(SiteA = df_a, SiteB = df_b),
#'                            group_col = "phase", id_col = "grave")
#' result2$summary
#' }
count_pipeline <- function(data,
                            group_col,
                            id_col          = NULL,
                            date_col        = NULL,
                            method          = c("bayesian", "mean"),
                            use_age_sep     = TRUE,
                            run_bias_sim    = FALSE,
                            n_bias_sims     = 200,
                            conf_level      = 0.80,
                            min_group_size  = 3) {

  method <- match.arg(method)
  norm     <- .normalize_dataset_list(data)
  datasets <- norm$datasets
  multi    <- norm$multi

  by_group  <- list()
  rows_list <- list()

  for (dsname in names(datasets)) {
    df <- datasets[[dsname]]

    if (!group_col %in% names(df))
      stop(sprintf("Column '%s' not found in dataset '%s'.", group_col, dsname))

    groups <- if (is.factor(df[[group_col]])) {
      levels(droplevels(df[[group_col]]))
    } else {
      unique(df[[group_col]])
    }

    for (grp in groups) {
      grp_df <- df[df[[group_col]] == grp, , drop = FALSE]
      n_obs  <- nrow(grp_df)
      case   <- if (multi) paste0(dsname, ": ", grp) else as.character(grp)

      if (n_obs < min_group_size) {
        warning(sprintf("Group '%s' (dataset '%s') has only %d observations (< %d). Skipping.",
                        grp, dsname, n_obs, min_group_size))
        next
      }

      grp_result <- list(dataset = dsname, group = grp, case = case, n = n_obs)

      if (!is.null(date_col) && date_col %in% names(grp_df)) {
        dts <- suppressWarnings(as.numeric(grp_df[[date_col]]))
        dts <- dts[!is.na(dts)]
        if (length(dts) > 0) {
          grp_result$date     <- mean(dts)
          grp_result$date_min <- min(dts)
          grp_result$date_max <- max(dts)
        }
      }

      tryCatch({
        # Determine ID column (never default to date_col, even if it
        # happens to be the first non-group column)
        non_group_cols <- setdiff(names(grp_df), group_col)
        id_c <- if (!is.null(id_col) && id_col %in% names(grp_df)) {
          id_col
        } else {
          setdiff(non_group_cols, date_col)[1]
        }

        # Build sub-frame: ID col + count cols, using only this dataset's
        # own columns -- no unioning across datasets. date_col is excluded
        # here too, so it's never mistaken for a grave-good count column.
        count_cols <- identify_count_columns(grp_df[, non_group_cols, drop = FALSE])
        count_cols <- setdiff(count_cols, c(group_col, date_col))

        if (length(count_cols) == 0) {
          warning(sprintf("Group '%s' (dataset '%s'): no count columns found. Skipping.",
                          grp, dsname))
          next
        }

        extra_cols <- intersect(names(grp_df),
                                 c("adult_count", "subadult_count", "lat", "lng"))
        sub_df <- grp_df[, unique(c(id_c, count_cols, extra_cols)), drop = FALSE]

        # 1. Prestige
        pres_df <- calculate_prestige(sub_df, method = method,
                                       use_age_sep = use_age_sep)
        grp_result$prestige_df <- pres_df

        # 2. Inequality (includes the full measure x graves $grid)
        ineq <- inequality_summary(pres_df, conf_level = conf_level)
        grp_result$inequality <- ineq

        # 3. Bias simulation (optional)
        abs_gini_ci <- NA_character_
        if (run_bias_sim) {
          # Pass prestige_result so the simulation's "observed" prestige
          # score matches calculate_prestige() exactly, instead of falling
          # back to its internal approximation (with a warning).
          bias <- simulate_category_bias(sub_df, prestige_result = pres_df,
                                          n_sims = n_bias_sims)
          grp_result$bias <- bias

          # Absolute Gini (= Gini x mean wealth) CI: pool the split- and
          # lump-scenario draws for the "prestige_sqrt" measure (matches
          # gini_combined = prestige + sqrt-count bonus) and take the same
          # conf_level quantiles used for gini_ci above. This is a
          # sensitivity range across category-recording decisions, not a
          # sampling-uncertainty interval.
          abs_gini_sim <- c(
            bias$ginis_split_prestige_sqrt * bias$means_split_prestige_sqrt,
            bias$ginis_lump_prestige_sqrt  * bias$means_lump_prestige_sqrt
          )
          alpha  <- (1 - conf_level) / 2
          bounds <- stats::quantile(abs_gini_sim, probs = c(alpha, 1 - alpha),
                                     na.rm = TRUE)
          abs_gini_ci <- paste0("[", round(bounds[1], 2), "\u2013",
                                 round(bounds[2], 2), "]")
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
          abs_gini_ci         = abs_gini_ci,
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
        if (!is.null(date_col)) {
          row$date     <- grp_result$date     %||% NA_real_
          row$date_min <- grp_result$date_min %||% NA_real_
          row$date_max <- grp_result$date_max %||% NA_real_
        }
        if (multi) {
          row <- cbind(
            data.frame(dataset = dsname, case = case, stringsAsFactors = FALSE),
            row
          )
        }

        by_group[[case]] <- grp_result
        rows_list[[length(rows_list) + 1]] <- row

      }, error = function(e) {
        warning(sprintf("Group '%s' (dataset '%s') failed: %s", grp, dsname, e$message))
      })
    }
  }

  summary_df <- if (length(rows_list) > 0)
    do.call(rbind, rows_list) else data.frame()

  list(summary = summary_df, by_group = by_group)
}


#' Run the physical-measure QuantWealth analysis pipeline on one or more datasets
#'
#' Companion to [count_pipeline()] for physical measure data (house floor
#' area, grave pit volume, mound size, etc. -- a single numeric column per
#' unit, rather than grave-good counts). Given a data frame (or a named
#' list of data frames) with a grouping column and a measure column in
#' each, this computes the physical-measure Gini ([physical_measure_gini()])
#' for every group in every dataset and collects results in one combined
#' object, in the same shape [count_pipeline()] returns.
#'
#' @param data A data frame, **or** a named list of data frames (e.g.
#'   `list(SiteA = df1, SiteB = df2)`) to analyse and compare side by side.
#'   Each dataset is analysed independently. Unnamed list elements are
#'   auto-labelled "Dataset1", "Dataset2", etc.
#' @param group_col Character. Name of the column to group by (e.g.
#'   `"phase"`, `"period"`, `"site"`). Same name is used for every dataset
#'   when `data` is a list.
#' @param measure_col Character. Name of the column containing the measure
#'   values (e.g. `"area_m2"`). Same name is used for every dataset when
#'   `data` is a list.
#' @param date_col Character, optional. Name of a numeric column giving an
#'   absolute calendar date for each row (negative = BCE, positive = CE).
#'   Same behaviour as in [count_pipeline()]: mean/min/max per group are
#'   stored as `date`/`date_min`/`date_max` in `$summary` and `$by_group`
#'   when supplied, for later use by [plot_gini_timeline()]. Default `NULL`.
#' @param conf_level Numeric CI level (default 0.80).
#' @param min_group_size Integer. Groups with fewer than 2 valid measure
#'   values are always skipped (a Gini needs at least 2 points); this
#'   argument additionally skips groups with fewer than `min_group_size`
#'   rows before that check, for consistency with [count_pipeline()]
#'   (default 3).
#' @return A list with two elements, matching [count_pipeline()]'s shape:
#'   * `$summary`: one row per group (per dataset, when multiple), with
#'     `gini`, `gini_ci`, `abs_gini`, `mean_measure`, `median_measure`,
#'     `min_measure`, `max_measure`. When `data` is a single data frame,
#'     this matches the previous `qw_pipeline(data_type = "physical")`
#'     output exactly; when `data` is a list, `dataset` and `case` columns
#'     are added as in [count_pipeline()].
#'   * `$by_group`: a named list (keyed by `case`, or `group` for a single
#'     dataset) with `sizes` (the cleaned measure vector) and `gini`.
#' @export
#' @examples
#' \dontrun{
#' houses <- data.frame(
#'   phase   = rep(c("Phase 1", "Phase 2"), each = 20),
#'   house   = paste0("H", 1:40),
#'   area_m2 = c(rlnorm(20, 4, 0.5), rlnorm(20, 4.5, 0.7))
#' )
#' result <- phys_pipeline(houses, group_col = "phase",
#'                          measure_col = "area_m2")
#' result$summary
#' }
phys_pipeline <- function(data,
                           group_col,
                           measure_col,
                           date_col        = NULL,
                           conf_level      = 0.80,
                           min_group_size  = 3) {

  norm     <- .normalize_dataset_list(data)
  datasets <- norm$datasets
  multi    <- norm$multi

  by_group  <- list()
  rows_list <- list()

  for (dsname in names(datasets)) {
    df <- datasets[[dsname]]

    if (!group_col %in% names(df))
      stop(sprintf("Column '%s' not found in dataset '%s'.", group_col, dsname))
    if (!measure_col %in% names(df))
      stop(sprintf("Column '%s' not found in dataset '%s'.", measure_col, dsname))

    groups <- if (is.factor(df[[group_col]])) {
      levels(droplevels(df[[group_col]]))
    } else {
      unique(df[[group_col]])
    }

    for (grp in groups) {
      grp_df <- df[df[[group_col]] == grp, , drop = FALSE]
      n_obs  <- nrow(grp_df)
      case   <- if (multi) paste0(dsname, ": ", grp) else as.character(grp)

      if (n_obs < min_group_size) {
        warning(sprintf("Group '%s' (dataset '%s') has only %d observations (< %d). Skipping.",
                        grp, dsname, n_obs, min_group_size))
        next
      }

      tryCatch({
        sizes <- as.numeric(grp_df[[measure_col]])
        sizes <- sizes[!is.na(sizes) & sizes > 0]

        if (length(sizes) < 2) {
          warning(sprintf("Group '%s' (dataset '%s'): fewer than 2 valid measure values. Skipping.",
                          grp, dsname))
          next
        }

        g <- physical_measure_gini(sizes, conf_level = conf_level)
        ci <- if (!is.na(g["lower"])) {
          paste0("[", round(g["lower"], 3), "\u2013", round(g["upper"], 3), "]")
        } else NA_character_

        grp_result <- list(dataset = dsname, group = grp, case = case,
                            n = n_obs, sizes = sizes, gini = g)

        if (!is.null(date_col) && date_col %in% names(grp_df)) {
          dts <- suppressWarnings(as.numeric(grp_df[[date_col]]))
          dts <- dts[!is.na(dts)]
          if (length(dts) > 0) {
            grp_result$date     <- mean(dts)
            grp_result$date_min <- min(dts)
            grp_result$date_max <- max(dts)
          }
        }

        row <- data.frame(
          group          = as.character(grp),
          n              = length(sizes),
          gini           = round(g["gini"], 3),
          gini_ci        = ci,
          abs_gini       = round(unname(g["gini"]) * mean(sizes), 2),
          mean_measure   = round(mean(sizes), 3),
          median_measure = round(median(sizes), 3),
          min_measure    = round(min(sizes), 3),
          max_measure    = round(max(sizes), 3),
          stringsAsFactors = FALSE,
          row.names        = NULL,
          check.names      = FALSE
        )
        if (!is.null(date_col)) {
          row$date     <- grp_result$date     %||% NA_real_
          row$date_min <- grp_result$date_min %||% NA_real_
          row$date_max <- grp_result$date_max %||% NA_real_
        }
        if (multi) {
          row <- cbind(
            data.frame(dataset = dsname, case = case, stringsAsFactors = FALSE),
            row
          )
        }

        by_group[[case]] <- grp_result
        rows_list[[length(rows_list) + 1]] <- row

      }, error = function(e) {
        warning(sprintf("Group '%s' (dataset '%s') failed: %s", grp, dsname, e$message))
      })
    }
  }

  summary_df <- if (length(rows_list) > 0)
    do.call(rbind, rows_list) else data.frame()

  list(summary = summary_df, by_group = by_group)
}


#' Run the QuantWealth analysis pipeline (deprecated)
#'
#' Deprecated -- replaced by [count_pipeline()] (for count data) and
#' [phys_pipeline()] (for physical measure data), which give each data type
#' its own clearer interface and both support multiple datasets natively.
#' This wrapper still works exactly as before and dispatches to one of the
#' two new functions (with a warning), but will be removed in a future
#' version.
#'
#' @param df A data frame. See [count_pipeline()] or [phys_pipeline()].
#' @param group_col Character. Name of the column to group by.
#' @param id_col Character. Name of the unit ID column. Ignored when
#'   `data_type = "physical"`.
#' @param data_type `"count"` (default) or `"physical"`.
#' @param measure_col Required when `data_type = "physical"`.
#' @param method Prestige method, count data only.
#' @param use_age_sep Logical, count data only.
#' @param run_bias_sim Logical, count data only.
#' @param n_bias_sims Integer, count data only.
#' @param conf_level Numeric CI level.
#' @param min_group_size Integer.
#' @return See [count_pipeline()] / [phys_pipeline()].
#' @export
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

  warning(
    "qw_pipeline() is deprecated. Use count_pipeline() for count data or ",
    "phys_pipeline() for physical measure data instead.",
    call. = FALSE
  )

  if (data_type == "physical") {
    if (is.null(measure_col))
      stop("'measure_col' must be specified when data_type = 'physical'.")
    phys_pipeline(df, group_col = group_col, measure_col = measure_col,
                  conf_level = conf_level, min_group_size = min_group_size)
  } else {
    count_pipeline(df, group_col = group_col, id_col = id_col, method = method,
                    use_age_sep = use_age_sep, run_bias_sim = run_bias_sim,
                    n_bias_sims = n_bias_sims, conf_level = conf_level,
                    min_group_size = min_group_size)
  }
}


#' Print a formatted summary of count_pipeline()/phys_pipeline() results
#'
#' @param pipeline_result Output from [count_pipeline()] or [phys_pipeline()]
#'   (or the deprecated [qw_pipeline()]).
#' @param digits Number of decimal places (default 3).
#' @export
print_pipeline_summary <- function(pipeline_result, digits = 3) {
  df <- pipeline_result$summary
  if (nrow(df) == 0) {
    message("No groups were successfully processed.")
    return(invisible(NULL))
  }
  # Round numeric columns
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  print(df, row.names = FALSE)
  invisible(df)
}
