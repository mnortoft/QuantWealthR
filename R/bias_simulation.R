# ============================================================================
# Category bias simulation -- matches app.R simulate_category_bias() exactly
# ============================================================================

#' Simulate the effect of category splitting and lumping on Gini coefficients
#'
#' Runs a Monte Carlo simulation that randomly splits and lumps artefact
#' categories to show how sensitive the Gini coefficient is to those recording
#' decisions.  Matches the behaviour of the QuantWealth online app exactly.
#'
#' **Splitting**: each artefact type may be split into up to `base_max_split`
#' sub-types.  Types sharing name substrings get a lower split limit
#' (adaptive reduction).  High-count types (beads, shells, etc.) are capped
#' using asinh of their median count.
#'
#' **Lumping**: types sharing a name substring are candidates for random
#' lumping into a single column.
#'
#' Four Gini measures are tracked: TOT+1, TOT+1+sqrt-count, Prestige+1, and
#' Prestige+1+sqrt-count -- for both split and lump scenarios. The +1 is a
#' base value for the grave itself; with `graves = "adults"` the whole value,
#' base included, is divided by the number of adults, as in
#' [calculate_prestige()].
#'
#' @param df Data frame. First column = unit ID; remaining columns = numeric
#'   counts (after [read_count_data()] or equivalent).
#' @param prestige_result Optional. The data frame returned by
#'   [calculate_prestige()] for this same dataset (run before any grave
#'   filtering). When supplied, the observed (non-simulated) prestige score
#'   used for `orig_gini_prestige`, `orig_gini_prestige_sqrt`,
#'   `orig_mean_prestige`, `orig_mean_prestige_sqrt`, and
#'   `wealth_scores$orig` is taken directly from `calculate_prestige()`'s
#'   `prestige_total` / `combined_total` columns (matched by ID). If
#'   omitted, these values fall back to a simplified internal approximation
#'   and a warning is issued, since that approximation will not match
#'   [calculate_prestige()]'s output.
#' @param graves Which graves to use: `"adults"` (default, matching the app),
#'   `"subadults"`, or `"all"`. `graves = "adults"` without an `adult_count`
#'   column falls back to `"all"` with a warning (rather than silently
#'   matching every row). `graves = "subadults"` without a `subadult_count`
#'   column raises an error, since there is no sensible "subadults" subset
#'   to fall back to.
#' @param n_sims Integer. Number of Monte Carlo iterations (default 100).
#' @param values_from Only used with `graves = "subadults"`. `"subset"`
#'   (default, previous behaviour): the simulation runs on the subadult graves
#'   alone, so each simulated type's value is taken from children's graves
#'   only. `"adults"`: the simulation runs on all graves and values each
#'   (simulated) type from the adult graves -- types found only in subadult
#'   graves from those -- as [calculate_prestige()] does; the results are
#'   then reported for the subadult graves. Use `"adults"` when the plotted
#'   subadult wealth comes from [calculate_prestige()] with age separation,
#'   so that the band describes the same value system.
#' @param min_match_len Integer. Minimum length of a shared letter sequence
#'   for two columns to be put in the same lump group when they don't share
#'   a name prefix (default 4, e.g. catches `"bead"`). Sequences containing
#'   `_` or `.` never count, so accidental matches across the prefix
#'   separator (e.g. `amb**er_**Schmuck` / `c**er_**KA`) are ignored. Columns
#'   sharing the prefix before the first `_` are always grouped. The groups
#'   used are returned in `$lump_groups`.
#' @param base_max_split Integer. Maximum split count before adaptive
#'   reduction (default 5).
#' @param progress Logical. Show a text progress bar over the simulations
#'   (default FALSE; [count_pipeline()] shows its own bar instead).
#' @param .tick Internal: a function called after each simulation, used by
#'   [count_pipeline()] to drive its progress bar.
#' @param split_stat How the typical count of a type is measured when
#'   limiting how many subtypes it can be split into: `"median"` (default,
#'   the previous behaviour) or `"mean"` of the non-zero counts, rounded up.
#'   A type can only be split into as many subtypes as this typical count
#'   (and at most `base_max_split`), so with `"median"` a type that usually
#'   occurs once per grave is never split, even if some graves hold several.
#'   `"mean"` lets graves with high counts raise the limit; for bead/shell-type
#'   columns the asinh damping still applies. Stored in `$split_stat`.
#' @param store_wealth Logical. If TRUE (default), stores per-grave wealth
#'   scores for every simulation in `$wealth_scores`, needed by
#'   [plot_bias_wealth_density()] and [plot_bias_wealth_lorenz()]. The object
#'   grows with `n_sims` x number of graves; set FALSE for very large
#'   datasets or many simulations if memory is a concern.
#' @return A list of class `"qw_bias"` with the following elements,
#'   matching the app's output:
#'   - `ginis_split_TOT`, `ginis_lump_TOT`, `ginis_split_TOT_sqrt`,
#'     `ginis_lump_TOT_sqrt`, `ginis_split_prestige`, `ginis_lump_prestige`,
#'     `ginis_split_prestige_sqrt`, `ginis_lump_prestige_sqrt`: numeric vectors
#'     of length `n_sims`.
#'   - `orig_gini_TOT`, `orig_gini_TOT_sqrt`, `orig_gini_prestige`,
#'     `orig_gini_prestige_sqrt`: observed Gini values (the two `prestige`
#'     values use `calculate_prestige()` output when `prestige_result` is
#'     supplied).
#'   - `means_split_*`, `means_lump_*`, `orig_mean_*`: mean wealth per
#'     simulation (for absolute Gini = G x mean).
#'   - `total_splits`, `total_lumped_cols`: integer vectors (length `n_sims`)
#'     recording how many columns existed in each simulation.
#'   - `n_sims`, `n_lump_groups`, `n_original_cols`.
#'   - `lump_groups`: list of character vectors, the column names in each
#'     lump group (columns lumping may merge within a simulation).
#'   - `adaptive_splits`, `max_counts`, `col_names`, `matched_strings`.
#'   - `graves_used`: which graves were included (`"adults"`, `"subadults"`,
#'     or `"all"`).
#'   - `wealth_scores` (only when `store_wealth = TRUE`): a list with `split`
#'     and `lump` (each a list of `n_sims` data frames with columns
#'     `grave_id`, `TOT`, `TOT_sqrt`, `prestige`, `prestige_sqrt`), plus
#'     `orig`, a single data frame with the same columns for the observed
#'     (non-simulated) distribution. `orig$prestige` / `orig$prestige_sqrt`
#'     come from `calculate_prestige()` when `prestige_result` is supplied.
#' @examples
#' df   <- read_count_data(example_data_path())
#' pres <- calculate_prestige(df)
#' set.seed(1)
#' bias <- simulate_category_bias(df, pres, n_sims = 50)
#' bias$orig_gini_prestige_sqrt               # observed Gini
#' summary(bias$ginis_split_prestige_sqrt)    # Ginis when types are split ...
#' summary(bias$ginis_lump_prestige_sqrt)     # ... and when they are lumped
#' plot_bias(bias, case = "Example site")
#'
#' # Subadult graves, with types valued from the adult graves
#' # (as calculate_prestige() does with age separation)
#' bias_sub <- simulate_category_bias(df, pres, graves = "subadults",
#'                                    values_from = "adults", n_sims = 50)
#' @export
simulate_category_bias <- function(df,
                                    prestige_result = NULL,
                                    graves        = c("adults", "all", "subadults"),
                                    n_sims        = 100,
                                    base_max_split = 5,
                                    min_match_len = 4,
                                    store_wealth  = TRUE,
                                    split_stat    = c("median", "mean"),
                                    values_from   = c("subset", "adults"),
                                    progress      = FALSE,
                                    .tick         = NULL) {

  graves <- match.arg(graves)
  split_stat <- match.arg(split_stat)
  values_from <- match.arg(values_from)
  if (values_from == "adults" && graves != "subadults") {
    warning("values_from = \"adults\" only applies to graves = \"subadults\"; ignored.")
    values_from <- "subset"
  }
  sub_mode <- FALSE   # TRUE: simulate on all graves, value types from adults, report subadults
  typical <- if (split_stat == "median") stats::median else mean

  # ---- Filter graves -------------------------------------------------------
  col_names_lower <- tolower(names(df))
  has_adult   <- "adult_count"    %in% col_names_lower
  has_sub     <- "subadult_count" %in% col_names_lower

  if (graves == "subadults" && !has_sub) {
    stop("graves = \"subadults\" was requested, but no `subadult_count` ",
         "column was found in `df`. Add a `subadult_count` column, or use ",
         "graves = \"all\" / \"adults\" instead.")
  }

  if (graves == "adults" && !has_adult) {
    warning("graves = \"adults\" was requested, but no `adult_count` column ",
            "was found in `df`. Falling back to graves = \"all\" (no age ",
            "filtering or per-capita division will be applied).")
    graves <- "all"
  }

  if ((has_adult || has_sub) && graves != "all") {
    ac <- if (has_adult) {
      v <- as.numeric(df[[names(df)[col_names_lower == "adult_count"][1]]])
      v[is.na(v)] <- 0; v
    } else rep(0L, nrow(df))   # graves == "subadults" here; no adult_count present

    sc <- if (has_sub) {
      v <- as.numeric(df[[names(df)[col_names_lower == "subadult_count"][1]]])
      v[is.na(v)] <- 0; v
    } else rep(0L, nrow(df))

    is_adult_grave   <- ac >= 1 | (ac == 0 & sc == 0)
    is_subadult_only <- ac == 0 & sc >= 1

    n_unknown_age <- sum(ac == 0 & sc == 0)
    if (n_unknown_age > 0) {
      warning(sprintf(
        "%d grave(s) have no recorded adult_count or subadult_count and are being treated as adult graves by default.",
        n_unknown_age
      ))
    }

    if (graves == "subadults" && values_from == "adults") {
      # Keep every grave: type values come from the adult graves (types found
      # only in subadult graves from those), as in calculate_prestige(); the
      # results are then reported for the subadult graves only.
      sub_mode  <- TRUE
      out_rows  <- is_subadult_only
      val_rows  <- is_adult_grave
      fb_rows   <- is_subadult_only
      if (!any(out_rows)) stop("No subadult-only graves found.")
    } else {
      df <- switch(graves,
        adults    = df[is_adult_grave,   , drop = FALSE],
        subadults = df[is_subadult_only, , drop = FALSE]
      )
    }

    if (nrow(df) == 0) {
      stop(sprintf(
        "No graves remain after filtering to graves = \"%s\". Check that your data actually contains graves matching that category.",
        graves
      ))
    }

    # Pass adult_counts vector for per-capita if using adults
    adult_counts_vec <- if (sub_mode) {
      ifelse(is_subadult_only, pmax(sc, 1), pmax(ac, 1))   # per-capita divisor
    } else if (graves == "adults") ac[is_adult_grave] else NULL
  } else {
    adult_counts_vec <- NULL
  }

  graves_used <- graves

  # ---- Identify count columns ----------------------------------------------
  gg_cols <- identify_count_columns(df)
  gg_cols <- gg_cols[!is.na(gg_cols) & nchar(gg_cols) > 0]
  gg_cols <- gg_cols[sapply(gg_cols, function(col) {
    vals <- df[[col]][!is.na(df[[col]])]
    length(vals) > 0 && !all(vals == 0)
  })]
  if (length(gg_cols) == 0) stop("No usable count columns found.")

  n_rows <- nrow(df)
  n_cols <- length(gg_cols)

  grave_ids <- as.character(df[[1]])
  if (!sub_mode) {
    val_rows <- rep(TRUE, nrow(df)); fb_rows <- rep(FALSE, nrow(df))
  }
  # Value of each type = mean TOT of the graves containing it, taken from the
  # value-defining graves (val_rows); types absent there use fb_rows.
  .type_vals <- function(m, tot) {
    vapply(seq_len(ncol(m)), function(j) {
      p  <- m[, j] > 0
      pa <- p & val_rows
      if (any(pa)) return(mean(tot[pa]))
      pf <- p & fb_rows
      if (any(pf)) mean(tot[pf]) else 0
    }, numeric(1))
  }

  # ---- Build grave-goods matrix --------------------------------------------
  gg_matrix <- as.matrix(df[, gg_cols, drop = FALSE])
  gg_matrix <- apply(gg_matrix, 2, as.numeric)
  if (n_cols == 1) gg_matrix <- matrix(gg_matrix, ncol = 1,
                                        dimnames = list(NULL, gg_cols))
  gg_matrix[is.na(gg_matrix)] <- 0

  # ---- Adaptive split limits (also provides matched_strings for annotation) -
  adaptive_info   <- .calc_adaptive_splits(gg_cols, base_max_split)
  adaptive_splits <- adaptive_info$splits
  matched_strings <- adaptive_info$matched_strings   # same source as app

  lower_cols <- tolower(gg_cols)

  # ---- High-count cap ------------------------------------------------------
  high_count_pats <- c("bead","ornament","pearl","perle","anh\u00E4nger","pendant",
                        "pebble","sherd","shell","arrowhead","tooth","teeth")
  is_high <- sapply(lower_cols, function(col)
    any(sapply(high_count_pats, function(p) grepl(p, col, fixed = TRUE))))

  max_counts <- sapply(seq_len(n_cols), function(j) {
    pv <- gg_matrix[, j][gg_matrix[, j] > 0]
    if (length(pv) == 0) return(1L)
    lim <- min(ceiling(typical(pv)), max(pv))
    if (is_high[j]) min(ceiling(asinh(lim)), 10L) else lim
  })

  for (j in which(is_high)) {
    pv <- gg_matrix[, j][gg_matrix[, j] > 0]
    if (length(pv) > 0)
      adaptive_splits[j] <- min(adaptive_splits[j],
                                 max(1L, ceiling(asinh(typical(pv)))))
  }

  # ---- Lumpable groups -----------------------------------------------------
  lump_groups   <- .find_lump_groups(gg_cols, min_match_len = min_match_len)
  n_lump_groups <- length(lump_groups)

  # ---- Original metrics ----------------------------------------------------
  orig_TOT        <- rowSums(gg_matrix > 0)
  orig_sqrt_bonus <- rowSums(ifelse(gg_matrix >= 1, sqrt(gg_matrix) - 1, 0))
  type_means_orig <- .type_vals(gg_matrix, orig_TOT)
  type_means_orig <- round(type_means_orig, 2)   # match app: rounds to 2 dp
  orig_prestige <- if (n_cols == 1)
    (gg_matrix > 0) * type_means_orig[1]
  else
    rowSums(sweep(gg_matrix > 0, 2, type_means_orig, `*`))

  # Base value of 1 per GRAVE: the burial itself counts as one unit of
  # wealth, shared between the adults buried in it (same as calculate_prestige():
  # (prestige + 1 + bonus) / adults). Without age data it is simply 1.
  base <- if (!is.null(adult_counts_vec)) 1 / pmax(adult_counts_vec, 1) else 1

  if (!is.null(adult_counts_vec)) {
    ac_safe          <- pmax(adult_counts_vec, 1)
    orig_TOT         <- orig_TOT        / ac_safe
    orig_sqrt_bonus  <- orig_sqrt_bonus / ac_safe
    orig_prestige    <- orig_prestige   / ac_safe
  }
  if (sub_mode) {
    orig_TOT        <- orig_TOT[out_rows]
    orig_sqrt_bonus <- orig_sqrt_bonus[out_rows]
    orig_prestige   <- orig_prestige[out_rows]
    base            <- base[out_rows]
    grave_ids       <- grave_ids[out_rows]
  }

  # ---- Optional: use calculate_prestige() output for the observed prestige score --
  if (!is.null(prestige_result)) {
    pr_ids    <- as.character(prestige_result[[1]])
    .check_unique_ids(grave_ids, "simulate_category_bias(): ID column of `df`")
    .check_unique_ids(pr_ids,    "simulate_category_bias(): ID column of `prestige_result`")
    match_idx <- match(grave_ids, pr_ids)
    if (anyNA(match_idx)) {
      warning(sum(is.na(match_idx)), " grave(s) in `df` have no matching ID in ",
              "`prestige_result`; their original prestige score will be NA.")
    }
    orig_prestige_final      <- prestige_result$prestige_total[match_idx]
    orig_prestige_sqrt_final <- prestige_result$combined_total[match_idx]
  } else {
    warning("`prestige_result` not supplied: the observed/original prestige score is ",
            "approximated internally rather than taken from calculate_prestige(). ",
            "Pass the result of calculate_prestige(df) as the second argument, e.g. ",
            "simulate_category_bias(df, calculate_prestige(df), ...), to use the ",
            "package's actual prestige estimate for the original distribution.")
    orig_prestige_final      <- orig_prestige + base
    orig_prestige_sqrt_final <- orig_prestige + base + orig_sqrt_bonus
  }

  orig_gini_TOT           <- DescTools::Gini(orig_TOT + base,                   unbiased = TRUE)
  orig_gini_TOT_sqrt      <- DescTools::Gini(orig_TOT + base + orig_sqrt_bonus, unbiased = TRUE)
  orig_gini_prestige      <- DescTools::Gini(orig_prestige_final,            unbiased = TRUE)
  orig_gini_prestige_sqrt <- DescTools::Gini(orig_prestige_sqrt_final,       unbiased = TRUE)

  orig_mean_TOT           <- mean(orig_TOT + base)
  orig_mean_TOT_sqrt      <- mean(orig_TOT + base + orig_sqrt_bonus)
  orig_mean_prestige      <- mean(orig_prestige_final)
  orig_mean_prestige_sqrt <- mean(orig_prestige_sqrt_final)

  # ---- Pre-allocate results ------------------------------------------------
  mk <- function() numeric(n_sims)
  ginis_split_TOT           <- mk(); ginis_lump_TOT           <- mk()
  ginis_split_TOT_sqrt      <- mk(); ginis_lump_TOT_sqrt      <- mk()
  ginis_split_prestige      <- mk(); ginis_lump_prestige      <- mk()
  ginis_split_prestige_sqrt <- mk(); ginis_lump_prestige_sqrt <- mk()

  means_split_TOT           <- mk(); means_lump_TOT           <- mk()
  means_split_TOT_sqrt      <- mk(); means_lump_TOT_sqrt      <- mk()
  means_split_prestige      <- mk(); means_lump_prestige      <- mk()
  means_split_prestige_sqrt <- mk(); means_lump_prestige_sqrt <- mk()

  total_splits      <- integer(n_sims)
  total_lumped_cols <- integer(n_sims)

  wealth_split_list <- if (store_wealth) vector("list", n_sims) else NULL
  wealth_lump_list  <- if (store_wealth) vector("list", n_sims) else NULL

  # ---- Simulation loop -----------------------------------------------------
  own_pb <- if (isTRUE(progress) && is.null(.tick))
    utils::txtProgressBar(min = 0, max = n_sims, style = 3) else NULL
  if (!is.null(own_pb)) on.exit(close(own_pb), add = TRUE)
  for (sim in seq_len(n_sims)) {
    if (!is.null(.tick)) .tick()
    if (!is.null(own_pb)) utils::setTxtProgressBar(own_pb, sim)

    # === SPLIT ===
    k_splits <- sapply(seq_len(n_cols), function(j) {
      max_k <- min(adaptive_splits[j], max_counts[j])
      if (max_k <= 1) return(1L)
      if (stats::runif(1) < 0.5) return(1L)
      sample(2:max_k, 1)
    })
    total_splits[sim] <- sum(k_splits)

    vlist <- lapply(seq_len(n_cols), function(ci) {
      k     <- k_splits[ci]
      col_v <- gg_matrix[, ci]
      sub_m <- matrix(0L, nrow = n_rows, ncol = k)
      for (ri in seq_len(n_rows)) {
        v <- col_v[ri]
        if (v == 0) next
        if (k == 1) sub_m[ri, 1] <- v
        else sub_m[ri, ] <- tabulate(sample(1:k, v, replace = TRUE), nbins = k)
      }
      sub_m
    })
    vm <- do.call(cbind, vlist)

    s_TOT        <- rowSums(vm > 0)
    s_sqrt_bonus <- rowSums(ifelse(vm >= 1, sqrt(vm) - 1, 0))
    nv           <- ncol(vm)
    vm_means     <- .type_vals(vm, s_TOT)
    s_prestige <- rowSums(sweep(vm > 0, 2, vm_means, `*`))

    if (!is.null(adult_counts_vec)) {
      s_TOT        <- s_TOT        / ac_safe
      s_sqrt_bonus <- s_sqrt_bonus / ac_safe
      s_prestige   <- s_prestige   / ac_safe
    }
    if (sub_mode) {
      s_TOT <- s_TOT[out_rows]; s_sqrt_bonus <- s_sqrt_bonus[out_rows]
      s_prestige <- s_prestige[out_rows]
    }

    ginis_split_TOT[sim]           <- DescTools::Gini(s_TOT + base,                        unbiased = TRUE)
    ginis_split_TOT_sqrt[sim]      <- DescTools::Gini(s_TOT + base + s_sqrt_bonus,         unbiased = TRUE)
    ginis_split_prestige[sim]      <- DescTools::Gini(s_prestige + base,                   unbiased = TRUE)
    ginis_split_prestige_sqrt[sim] <- DescTools::Gini(s_prestige + base + s_sqrt_bonus,    unbiased = TRUE)

    means_split_TOT[sim]           <- mean(s_TOT + base)
    means_split_TOT_sqrt[sim]      <- mean(s_TOT + base + s_sqrt_bonus)
    means_split_prestige[sim]      <- mean(s_prestige + base)
    means_split_prestige_sqrt[sim] <- mean(s_prestige + base + s_sqrt_bonus)

    if (store_wealth) {
      wealth_split_list[[sim]] <- data.frame(
        grave_id       = grave_ids,
        TOT            = s_TOT + base,
        TOT_sqrt       = s_TOT + base + s_sqrt_bonus,
        prestige       = s_prestige + base,
        prestige_sqrt  = s_prestige + base + s_sqrt_bonus
      )
    }

    # === LUMP ===
    if (n_lump_groups > 0) {
      lm      <- gg_matrix
      cols_rm <- c()
      for (grp in lump_groups) {
        n2lump  <- sample(2:length(grp), 1)
        c2lump  <- sample(grp, n2lump)
        lm[, c2lump[1]] <- rowSums(lm[, c2lump, drop = FALSE])
        cols_rm <- c(cols_rm, c2lump[-1])
      }
      if (length(cols_rm) > 0) lm <- lm[, -cols_rm, drop = FALSE]
      total_lumped_cols[sim] <- ncol(lm)

      l_TOT   <- rowSums(lm > 0)
      l_sqrt  <- rowSums(ifelse(lm >= 1, sqrt(lm) - 1, 0))
      nl      <- ncol(lm)
      l_means <- .type_vals(lm, l_TOT)
      l_prestige <- rowSums(sweep(lm > 0, 2, l_means, `*`))

      if (!is.null(adult_counts_vec)) {
        l_TOT      <- l_TOT      / ac_safe
        l_sqrt     <- l_sqrt     / ac_safe
        l_prestige <- l_prestige / ac_safe
      }
      if (sub_mode) {
        l_TOT <- l_TOT[out_rows]; l_sqrt <- l_sqrt[out_rows]
        l_prestige <- l_prestige[out_rows]
      }

      ginis_lump_TOT[sim]           <- DescTools::Gini(l_TOT + base,                     unbiased = TRUE)
      ginis_lump_TOT_sqrt[sim]      <- DescTools::Gini(l_TOT + base + l_sqrt,            unbiased = TRUE)

      ginis_lump_prestige[sim]      <- DescTools::Gini(l_prestige + base,                unbiased = TRUE)
      ginis_lump_prestige_sqrt[sim] <- DescTools::Gini(l_prestige + base + l_sqrt,       unbiased = TRUE)
      means_lump_TOT[sim]           <- mean(l_TOT + base)
      means_lump_TOT_sqrt[sim]      <- mean(l_TOT + base + l_sqrt)
      means_lump_prestige[sim]      <- mean(l_prestige + base)
      means_lump_prestige_sqrt[sim] <- mean(l_prestige + base + l_sqrt)
      if (store_wealth) {
        wealth_lump_list[[sim]] <- data.frame(
          grave_id      = grave_ids,
          TOT           = l_TOT + base,
          TOT_sqrt      = l_TOT + base + l_sqrt,
          prestige      = l_prestige + base,
          prestige_sqrt = l_prestige + base + l_sqrt
        )
      }
    } else {
      ginis_lump_TOT[sim]           <- ginis_split_TOT[sim]
      ginis_lump_TOT_sqrt[sim]      <- ginis_split_TOT_sqrt[sim]
      ginis_lump_prestige[sim]      <- ginis_split_prestige[sim]
      ginis_lump_prestige_sqrt[sim] <- ginis_split_prestige_sqrt[sim]
      means_lump_TOT[sim]           <- means_split_TOT[sim]
      means_lump_TOT_sqrt[sim]      <- means_split_TOT_sqrt[sim]
      means_lump_prestige[sim]      <- means_split_prestige[sim]
      means_lump_prestige_sqrt[sim] <- means_split_prestige_sqrt[sim]
      total_lumped_cols[sim]        <- n_cols
      if (store_wealth) wealth_lump_list[[sim]] <- wealth_split_list[[sim]]
    }
  }

  result <- list(
    ginis_split_TOT           = ginis_split_TOT,
    ginis_lump_TOT            = ginis_lump_TOT,
    ginis_split_TOT_sqrt      = ginis_split_TOT_sqrt,
    ginis_lump_TOT_sqrt       = ginis_lump_TOT_sqrt,
    ginis_split_prestige      = ginis_split_prestige,
    ginis_lump_prestige       = ginis_lump_prestige,
    ginis_split_prestige_sqrt = ginis_split_prestige_sqrt,
    ginis_lump_prestige_sqrt  = ginis_lump_prestige_sqrt,
    orig_gini_TOT           = round(orig_gini_TOT, 2),
    orig_gini_TOT_sqrt      = round(orig_gini_TOT_sqrt, 2),
    orig_gini_prestige      = round(orig_gini_prestige, 2),
    orig_gini_prestige_sqrt = round(orig_gini_prestige_sqrt, 2),
    means_split_TOT           = means_split_TOT,
    means_lump_TOT            = means_lump_TOT,
    means_split_TOT_sqrt      = means_split_TOT_sqrt,
    means_lump_TOT_sqrt       = means_lump_TOT_sqrt,
    means_split_prestige      = means_split_prestige,
    means_lump_prestige       = means_lump_prestige,
    means_split_prestige_sqrt = means_split_prestige_sqrt,
    means_lump_prestige_sqrt  = means_lump_prestige_sqrt,
    orig_mean_TOT           = orig_mean_TOT,
    orig_mean_TOT_sqrt      = orig_mean_TOT_sqrt,
    orig_mean_prestige      = orig_mean_prestige,
    orig_mean_prestige_sqrt = orig_mean_prestige_sqrt,
    total_splits      = total_splits,
    total_lumped_cols = total_lumped_cols,
    n_sims          = n_sims,
    n_lump_groups   = n_lump_groups,
    lump_groups     = lapply(lump_groups, function(i) gg_cols[i]),
    n_original_cols = n_cols,
    adaptive_splits = adaptive_splits,
    max_counts      = max_counts,
    col_names       = gg_cols,
    matched_strings = matched_strings,
    graves_used     = graves_used,
    values_from     = values_from,
    split_stat      = split_stat
  )
  if (store_wealth) {
    wealth_orig <- data.frame(
      grave_id      = grave_ids,
      TOT           = orig_TOT + base,
      TOT_sqrt      = orig_TOT + base + orig_sqrt_bonus,
      prestige      = orig_prestige_final,
      prestige_sqrt = orig_prestige_sqrt_final
    )
    result$wealth_scores <- list(split = wealth_split_list, lump = wealth_lump_list,
                                  orig = wealth_orig)
  }
  class(result) <- c("qw_bias", "list")
  result
}


.calc_adaptive_splits <- function(col_names, base_max_split = 5,
                                   min_split = 1, min_str_len = 5) {
  n <- length(col_names)
  if (n == 0) return(list(splits = integer(0), matched_strings = character(0)))
  if (n == 1) return(list(
    splits          = stats::setNames(as.integer(base_max_split), col_names),
    matched_strings = NA_character_
  ))
  lower   <- tolower(col_names)
  adj     <- integer(n)
  matched <- rep(NA_character_, n)
  special_patterns <- c("bead","ornament","pearl","perle","anh\u00E4nger",
                         "pendant","pebble","sherd","shell","arrowhead")
  for (i in seq_len(n)) {
    ci <- lower[i]; best_count <- 0L; best_string <- NA_character_
    if (grepl("_", ci)) {
      pref <- sub("_.*", "", ci)
      if (nchar(pref) >= 1) {
        cnt <- sum(sapply(lower[-i], function(cj)
          grepl(paste0("^", pref, "_"), cj) | grepl(paste0("_", pref, "_"), cj)))
        if (cnt > 0) { best_count <- cnt; best_string <- paste0(pref, "_") }
      }
    }
    if (best_count == 0) {
      for (pat in special_patterns) {
        if (grepl(pat, ci, fixed = TRUE)) {
          cnt <- sum(sapply(lower[-i], function(cj) grepl(pat, cj, fixed = TRUE)))
          if (cnt > best_count) { best_count <- cnt; best_string <- pat }
        }
      }
    }
    if (best_count == 0 && nchar(ci) >= min_str_len) {
      for (start in seq_len(nchar(ci) - min_str_len + 1)) {
        for (len in min_str_len:min(10, nchar(ci) - start + 1)) {
          sub_i <- substr(ci, start, start + len - 1)
          cnt   <- sum(sapply(lower[-i], function(cj) grepl(sub_i, cj, fixed = TRUE)))
          if (cnt > best_count) { best_count <- cnt; best_string <- sub_i }
        }
      }
    }
    adj[i] <- if (best_count > 0) best_count + 1L else 0L
    matched[i] <- best_string
  }
  splits <- pmax(as.integer(min_split), as.integer(base_max_split) - adj)
  names(splits) <- col_names
  list(splits = splits, matched_strings = matched)
}


.find_lump_groups <- function(col_names, min_match_len = 4) {
  n <- length(col_names); lower <- tolower(col_names)
  groups <- list(); assigned <- logical(n); excl <- c("presence","present")
  if (n < 2) return(groups)
  for (i in seq_len(n - 1)) {
    if (assigned[i]) next
    grp <- i; ci <- lower[i]
    pref <- if (grepl("_", ci)) sub("_.*", "", ci) else NULL
    for (j in seq(i + 1, n)) {
      if (assigned[j]) next
      cj <- lower[j]; matched <- FALSE
      if (!is.null(pref) && nchar(pref) >= 1 && !pref %in% excl &&
          grepl(paste0("^", pref, "_"), cj)) matched <- TRUE
      max_len <- min(max(6, min_match_len), nchar(ci))
      if (!matched && max_len >= min_match_len) {
        for (len in min_match_len:max_len) {
          for (start in seq_len(nchar(ci) - len + 1)) {
            sub_i <- substr(ci, start, start + len - 1)
            # Sequences spanning a separator (e.g. "er_") are spelling
            # accidents, not shared meaning -- never count them.
            if (grepl("[_.]", sub_i)) next
            if (!sub_i %in% excl && grepl(sub_i, cj, fixed = TRUE)) {
              matched <- TRUE; break
            }
          }
          if (matched) break
        }
      }
      if (matched) { grp <- c(grp, j); assigned[j] <- TRUE }
    }
    if (length(grp) > 1) { groups[[length(groups) + 1]] <- grp; assigned[i] <- TRUE }
  }
  groups
}
