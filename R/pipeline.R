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


#' Combine several grouping columns into one group label
#'
#' Pastes the values of `group_cols` row-wise ("SK | 2300-2200"). Rows with
#' a missing value in any grouping column get NA. If every grouping column
#' is a factor, the result is a factor whose levels follow the columns'
#' level orders (first column varies slowest), keeping only observed
#' combinations; otherwise a character vector.
#' @noRd
.combine_group_cols <- function(df, group_cols, sep = " | ") {
  vals <- lapply(group_cols, function(gc) as.character(df[[gc]]))
  key  <- do.call(paste, c(vals, sep = sep))
  any_na <- Reduce(`|`, lapply(group_cols, function(gc) is.na(df[[gc]])))
  key[any_na] <- NA_character_

  if (all(vapply(group_cols, function(gc) is.factor(df[[gc]]), logical(1)))) {
    lev <- levels(interaction(df[group_cols], sep = sep,
                              lex.order = TRUE, drop = TRUE))
    key <- factor(key, levels = lev)
  }
  key
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
#'
#'   To cross-group by several columns at once, give a character vector,
#'   e.g. `group_col = c("culture", "phase")`. Every observed combination
#'   then becomes its own group (and gets its own bias simulation when
#'   `run_bias_sim = TRUE`), labelled `"<culture> | <phase>"`. Combinations
#'   that don't occur in the data are not created. If all grouping columns
#'   are factors, groups are ordered by the first column's levels, then the
#'   second's, etc.; otherwise by order of first appearance. Rows with a
#'   missing value in any grouping column are left out (with a warning).
#' @param id_col Character. Name of the unit ID column (e.g. `"grave_id"`).
#'   If NULL (default), the first non-group column is used as the ID, in
#'   each dataset.
#'   IDs must be unique within each dataset; the pipeline stops and lists
#'   the repeated IDs otherwise.
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
#' @param exclude_cols Character vector of column names to leave out of the
#'   grave-good columns (e.g. numeric metadata such as `"Cremation"`, or
#'   features you don't want valued). Applied to every dataset; names absent
#'   from a given dataset are ignored there, but a warning is given for
#'   names found in none of the datasets (likely typos). Default NULL.
#' @param exclude_pattern A single regular expression; grave-good columns
#'   whose names match it (case-insensitive) are left out, e.g.
#'   `"^(CON|ritual)_"` to drop all construction and ritual columns. A
#'   warning is given if it matches no column in any dataset. Default NULL.
#'
#'   Excluded columns are removed before prestige values, inequality
#'   measures and the bias simulation are calculated, so they affect none
#'   of them. The columns actually used for each group are stored in
#'   `$by_group[[case]]$count_cols`, and the excluded ones in
#'   `$by_group[[case]]$excluded_cols`.
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
#' @param bias_ages Which graves the bias simulation covers. `"adults"`
#'   (default, as before and as in the app): adult graves only. `"both"`:
#'   also a second simulation for the subadult-only graves, in which every
#'   (simulated) type is valued from the adult graves (types found only in
#'   subadult graves from those), as in [calculate_prestige()]. Stored in
#'   `$by_group[[case]]$bias_subadults` and used for bias bands and
#'   [compare_cases()] with `graves = "subadults"`. Doubles the simulation
#'   time. Adult results are unaffected.
#' @param min_subadult_bias Minimum number of subadult-only graves in a group
#'   for the subadult simulation to be run (default 5); smaller groups get
#'   no subadult band (a message says so).
#' @param n_bias_sims Integer. Number of bias simulations per group
#'   (default 200). Ignored when `run_bias_sim = FALSE`.
#' @param min_match_len Integer. Passed to [simulate_category_bias()]:
#'   minimum shared letter-sequence length for grouping columns without a
#'   common prefix into lump groups (default 4). Ignored when
#'   `run_bias_sim = FALSE`.
#' @param progress Logical. Show a text progress bar in the console
#'   (default: `interactive()`, i.e. on in RStudio/console, off in scripts
#'   run with Rscript or knitr). With `run_bias_sim = TRUE` the bar advances
#'   per simulation, since those take most of the time.
#' @param split_stat `"median"` (default) or `"mean"`. Passed to
#'   [simulate_category_bias()]: which typical count per type limits how
#'   many subtypes it can be split into. Ignored when `run_bias_sim = FALSE`.
#' @param store_wealth Logical. Passed to [simulate_category_bias()]. If
#'   TRUE (default), the per-grave wealth scores of every simulation are kept
#'   in `$by_group[[case]]$bias$wealth_scores`, so they can be plotted later
#'   with [plot_bias_wealth_density()] or [plot_bias_wealth_lorenz()]. Set
#'   FALSE to save memory with many groups, large datasets or many
#'   simulations. Ignored when `run_bias_sim = FALSE`.
#' @param conf_level Numeric CI level for Gini coefficients (default 0.80).
#'   Also used for `abs_gini_ci` when `run_bias_sim = TRUE`.
#' @param min_group_size Integer. Groups with fewer observations than this are
#'   skipped with a warning (default 3).
#' @return A list of class `"qw_pipeline"` with two elements:
#'   * `$summary`: a data frame with one row per group (per dataset and
#'     group, when `data` is a list). Columns: `group`; with several
#'     datasets also `dataset` and `case` (`"<dataset>: <group>"`, the name
#'     used everywhere else, so identical group names in different datasets
#'     never collide); with several `group_col`s one column per grouping
#'     variable; `n` (graves in the main subset: adult graves when age
#'     separation is used, otherwise all), `n_adult`, `n_subadult`;
#'     `gini_combined` with `gini_ci`; `abs_gini` (Gini x mean wealth) with
#'     `abs_gini_ci` (from the bias simulations, if run); `mean_wealth`,
#'     `median_wealth`; `gini_prestige` and `gini_tot` (the same without the
#'     count bonus / with type counts only); `gini_subadult`,
#'     `abs_gini_subadult`; and `date`, `date_min`, `date_max` when
#'     `date_col` is given.
#'   * `$by_group`: a named list with one entry per case (class
#'     `"qw_group"`), holding `dataset`, `group`, `case`, `n`, the dates,
#'     `count_cols` (the grave-good columns used) and `excluded_cols`,
#'     `prestige_df` (the [calculate_prestige()] result: one row per grave
#'     with its wealth), `inequality` (the [inequality_summary()] result,
#'     including `$grid` with every measure x grave subset), and with
#'     `run_bias_sim = TRUE` `bias` (the [simulate_category_bias()] result)
#'     and, with `bias_ages = "both"`, `bias_subadults`. An entry can be
#'     passed directly to [plot_bias()], [plot_bias_wealth_density()] and
#'     [plot_bias_wealth_lorenz()].
#' @examples
#' ph <- read_count_data(example_data_path("phases"))
#'
#' # One dataset per culture; each phase is analysed separately
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' res$summary
#' names(res$by_group)              # the cases, "dataset: phase"
#' res$by_group[["A: A2"]]$prestige_df[1:3, ]
#'
#' # With category-bias simulations, also for the subadult graves
#' set.seed(1)
#' res_b <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                         id_col = "Grave_ID", date_col = "date_BCE",
#'                         run_bias_sim = TRUE, n_bias_sims = 20,
#'                         bias_ages = "both", progress = FALSE)
#'
#' # Leave out construction features from the valuation
#' res_nc <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                          id_col = "Grave_ID", date_col = "date_BCE",
#'                          exclude_pattern = "^CON_",
#'                          progress = FALSE)
#' @export
count_pipeline <- function(data,
                            group_col,
                            id_col          = NULL,
                            date_col        = NULL,
                            exclude_cols    = NULL,
                            exclude_pattern = NULL,
                            method          = c("bayesian", "mean"),
                            use_age_sep     = TRUE,
                            run_bias_sim    = FALSE,
                            n_bias_sims     = 200,
                            min_match_len   = 4,
                            split_stat      = c("median", "mean"),
                            store_wealth    = TRUE,
                            progress        = interactive(),
                            conf_level      = 0.80,
                            min_group_size  = 3,
                            bias_ages       = c("adults", "both"),
                            min_subadult_bias = 5) {

  method <- match.arg(method)
  split_stat <- match.arg(split_stat)
  bias_ages  <- match.arg(bias_ages)

  # Several grouping columns -> analyse every observed combination as its
  # own group, via a temporary combined column. `group_cols` keeps the
  # original names so they're never mistaken for grave-good columns.
  group_cols <- group_col
  if (length(group_cols) > 1) group_col <- ".qw_combined_group"

  norm     <- .normalize_dataset_list(data)
  datasets <- norm$datasets
  multi    <- norm$multi

  # Check exclusions against all datasets up front, so a typo is flagged
  # once instead of silently leaving a column in.
  if (!is.null(exclude_pattern) &&
      (!is.character(exclude_pattern) || length(exclude_pattern) != 1))
    stop("'exclude_pattern' must be a single character string (a regular expression).")
  all_names <- unique(unlist(lapply(datasets, names)))
  if (!is.null(exclude_cols)) {
    not_found <- setdiff(exclude_cols, all_names)
    if (length(not_found) > 0)
      warning("exclude_cols not found in any dataset (check spelling): ",
              paste(not_found, collapse = ", "))
  }
  if (!is.null(exclude_pattern) &&
      !any(grepl(exclude_pattern, all_names, ignore.case = TRUE)))
    warning("exclude_pattern '", exclude_pattern, "' matches no column in any dataset.")

  by_group  <- list()
  rows_list <- list()

  # ---- Progress bar: one unit per group, plus one per bias simulation ----
  pb <- NULL
  pb_env <- new.env()
  pb_env$done <- 0; pb_env$target <- 0
  units_per_group <- 1 + if (isTRUE(run_bias_sim))
    n_bias_sims * (if (bias_ages == "both") 2 else 1) else 0
  if (isTRUE(progress)) {
    n_grp_total <- sum(vapply(datasets, function(d) {
      gc <- intersect(group_cols, names(d))
      if (length(gc) == 0) 0L else nrow(unique(stats::na.omit(d[, gc, drop = FALSE])))
    }, integer(1)))
    pb <- utils::txtProgressBar(min = 0, max = max(1, n_grp_total * units_per_group),
                                style = 3)
    on.exit(close(pb), add = TRUE)
  }
  pb_tick <- function(k = 1) {
    if (is.null(pb)) return(invisible())
    pb_env$done <- min(pb_env$done + k, pb_env$target)
    utils::setTxtProgressBar(pb, pb_env$done)
  }

  for (dsname in names(datasets)) {
    df <- datasets[[dsname]]

    missing_gc <- setdiff(group_cols, names(df))
    if (length(missing_gc) > 0)
      stop(sprintf("Column(s) %s not found in dataset '%s'.",
                   paste0("'", missing_gc, "'", collapse = ", "), dsname))

    if (length(group_cols) > 1) {
      df[[group_col]] <- .combine_group_cols(df, group_cols)
      n_na <- sum(is.na(df[[group_col]]))
      if (n_na > 0)
        warning(sprintf("%d row(s) in dataset '%s' have a missing value in %s and are left out.",
                        n_na, dsname, paste(group_cols, collapse = " / ")))
    }

    # Unique IDs are required: later steps look rows up by ID. Checked here,
    # per dataset, so the error isn't swallowed as a per-group warning.
    if (!is.null(id_col) && !id_col %in% names(df))
      stop(sprintf("id_col '%s' not found in dataset '%s'. Available columns start with: %s",
                   id_col, dsname, paste(utils::head(names(df), 6), collapse = ", ")),
           call. = FALSE)
    id_ds <- if (!is.null(id_col)) id_col else
      setdiff(names(df), c(group_col, group_cols, date_col))[1]
    .check_unique_ids(df[[id_ds]],
                      sprintf("Dataset '%s', ID column '%s'", dsname, id_ds))

    # Grave-good counts can't be negative: a negative column is almost always
    # a date or other numeric metadata that would be valued as a grave good.
    cc_all <- setdiff(identify_count_columns(df[, c(id_ds, setdiff(names(df), id_ds)), drop = FALSE]),
                      c(group_col, group_cols, date_col, exclude_cols))
    if (!is.null(exclude_pattern))
      cc_all <- cc_all[!grepl(exclude_pattern, cc_all, ignore.case = TRUE)]
    neg <- cc_all[vapply(cc_all, function(cl) any(suppressWarnings(as.numeric(df[[cl]])) < 0,
                                                  na.rm = TRUE), logical(1))]
    if (length(neg) > 0)
      warning(sprintf(paste0("Dataset '%s': column(s) %s contain negative values but would be ",
                             "counted as grave goods. If this is a date, pass it as `date_col`; ",
                             "otherwise add it to `exclude_cols`."),
                      dsname, paste0("'", neg, "'", collapse = ", ")), call. = FALSE)

    groups <- if (is.factor(df[[group_col]])) {
      levels(droplevels(df[[group_col]]))
    } else {
      unique(df[[group_col]][!is.na(df[[group_col]])])
    }
    # Blank labels (e.g. empty cells read as "") are missing values, not a group
    blank <- !is.na(df[[group_col]]) & !nzchar(trimws(as.character(df[[group_col]])))
    if (any(blank))
      message(sprintf("Dataset '%s': %d row(s) with an empty %s are left out (no group).",
                      dsname, sum(blank), group_col))
    groups <- groups[nzchar(trimws(as.character(groups)))]

    for (grp in groups) {
      if (!is.null(pb)) {                 # finish the previous group's share
        pb_env$done <- pb_env$target
        utils::setTxtProgressBar(pb, pb_env$done)
        pb_env$target <- pb_env$target + units_per_group
      }
      grp_df <- df[which(df[[group_col]] == grp), , drop = FALSE]
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
        non_group_cols <- setdiff(names(grp_df), c(group_col, group_cols))
        id_c <- if (!is.null(id_col) && id_col %in% names(grp_df)) {
          id_col
        } else {
          setdiff(non_group_cols, date_col)[1]
        }

        # Build sub-frame: ID col + count cols, using only this dataset's
        # own columns -- no unioning across datasets. date_col is excluded
        # here too, so it's never mistaken for a grave-good count column.
        count_cols <- identify_count_columns(grp_df[, non_group_cols, drop = FALSE])
        count_cols <- setdiff(count_cols, c(group_col, group_cols, date_col))

        # User-specified exclusions (numeric metadata, features not to value)
        excluded <- intersect(count_cols, exclude_cols)
        if (!is.null(exclude_pattern))
          excluded <- union(excluded,
                            count_cols[grepl(exclude_pattern, count_cols, ignore.case = TRUE)])
        count_cols <- setdiff(count_cols, excluded)
        grp_result$count_cols    <- count_cols
        grp_result$excluded_cols <- excluded

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
        pb_tick()

        # 3. Bias simulation (optional)
        abs_gini_ci <- NA_character_
        if (run_bias_sim) {
          # Pass prestige_result so the simulation's "observed" prestige
          # score matches calculate_prestige() exactly, instead of falling
          # back to its internal approximation (with a warning).
          bias <- simulate_category_bias(sub_df, prestige_result = pres_df,
                                          n_sims = n_bias_sims,
                                          min_match_len = min_match_len,
                                          split_stat = split_stat,
                                          store_wealth = store_wealth,
                                          .tick = if (!is.null(pb)) pb_tick)
          grp_result$bias <- bias

          # Optional second simulation for the subadult graves, valued like
          # the main analysis (types valued from adult graves).
          if (bias_ages == "both") {
            n_sub <- sum(pres_df$is_subadult_only %||% FALSE, na.rm = TRUE)
            if (n_sub >= min_subadult_bias) {
              grp_result$bias_subadults <- simulate_category_bias(
                sub_df, prestige_result = pres_df, graves = "subadults",
                values_from = "adults", n_sims = n_bias_sims,
                min_match_len = min_match_len, split_stat = split_stat,
                store_wealth = store_wealth, .tick = if (!is.null(pb)) pb_tick)
            } else {
              message(sprintf("%s: %d subadult grave(s) (< min_subadult_bias = %d); no subadult bias simulation.",
                              case, n_sub, min_subadult_bias))
              if (!is.null(pb)) pb_tick(n_bias_sims)
            }
          }

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
        if (length(group_cols) > 1) {
          gc_vals <- lapply(group_cols, function(gc) as.character(grp_df[[gc]][1]))
          names(gc_vals) <- group_cols
          grp_result$group_values <- gc_vals
          row <- cbind(row[, "group", drop = FALSE],
                       as.data.frame(gc_vals, stringsAsFactors = FALSE,
                                     check.names = FALSE),
                       row[, setdiff(names(row), "group"), drop = FALSE])
        }
        if (multi) {
          row <- cbind(
            data.frame(dataset = dsname, case = case, stringsAsFactors = FALSE),
            row
          )
        }

        class(grp_result) <- c("qw_group", "list")
        by_group[[case]] <- grp_result
        rows_list[[length(rows_list) + 1]] <- row

      }, error = function(e) {
        warning(sprintf("Group '%s' (dataset '%s') failed: %s", grp, dsname, e$message))
      })
    }
  }

  summary_df <- if (length(rows_list) > 0)
    do.call(rbind, rows_list) else data.frame()

  structure(list(summary = summary_df, by_group = by_group),
            class = c("qw_pipeline", "list"))
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
#'     `min_measure`, `max_measure`. When `data` is a list, `dataset` and `case` columns
#'     are added as in [count_pipeline()].
#'   * `$by_group`: a named list (keyed by `case`, or `group` for a single
#'     dataset) with `sizes` (the cleaned measure vector) and `gini`.
#' @examples
#' set.seed(1)
#' houses <- data.frame(
#'   phase   = rep(c("Phase 1", "Phase 2"), each = 20),
#'   house   = paste0("H", 1:40),
#'   area_m2 = c(rlnorm(20, 4, 0.5), rlnorm(20, 4.5, 0.7))
#' )
#' res <- phys_pipeline(houses, group_col = "phase", measure_col = "area_m2")
#' res$summary
#' @export
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

  structure(list(summary = summary_df, by_group = by_group),
            class = c("qw_phys_pipeline", "list"))
}


#' Print a formatted summary of count_pipeline()/phys_pipeline() results
#'
#' @param pipeline_result Output from [count_pipeline()] or [phys_pipeline()].
#' @param digits Number of decimal places (default 3).
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#' print_pipeline_summary(res)
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
