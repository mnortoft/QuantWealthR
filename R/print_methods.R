# ============================================================================
# Compact printing of pipeline results
# ============================================================================
# Typing the name of a pipeline result used to print every list inside it
# (all grave tables, all simulations), which can be millions of lines. These
# methods print a short overview instead. The full content is unchanged and
# still reachable with $, e.g. res$summary or res$by_group[["A: A1"]]$prestige_df.

#' Print a pipeline result as a short overview
#'
#' Typing the name of a [count_pipeline()] or [phys_pipeline()] result (or of
#' one of its cases, or a bias simulation) prints a short overview rather
#' than every table and simulation inside it. Nothing is removed: all parts
#' are still in the object and can be reached with `$`, as the overview
#' shows.
#'
#' * A pipeline result shows the number of cases and the summary table (one
#'   row per case, the first `n` rows), and lists its parts.
#' * A case (`res$by_group[["A: A1"]]`) shows its size, date and main
#'   statistics, and lists its parts with their sizes.
#' * A bias simulation (`res$by_group[["A: A1"]]$bias`) shows the original
#'   Gini and the range of the simulated Ginis.
#'
#' To see everything, use `str(x, max.level = 2)` (the structure, compact) or
#' `print(unclass(x))` (all contents, can be very long).
#'
#' @param x A pipeline result, a case from its `by_group` list, or a bias
#'   simulation.
#' @param n Maximum number of summary rows to print (default 20).
#' @param digits Number of decimals (default 3).
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       progress = FALSE)
#' res                          # overview
#' res$by_group[["A: A2"]]      # one case
#' res$summary                  # the full summary table
#' @name print_pipeline
NULL

.round_df <- function(df, digits) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  df
}

# One line describing an object's size, e.g. "data frame, 26 rows x 25 columns"
.describe <- function(v) {
  if (is.data.frame(v)) sprintf("data frame, %d rows x %d columns", nrow(v), ncol(v))
  else if (is.list(v)) sprintf("list of %d", length(v))
  else if (length(v) == 1) paste0(class(v)[1], ": ", format(v, digits = 4))
  else sprintf("%s vector, length %d", class(v)[1], length(v))
}

.list_parts <- function(x, prefix) {
  cat("\nParts (use ", prefix, "$<name>):\n", sep = "")
  w <- max(nchar(names(x)))
  for (nm in names(x))
    cat(sprintf("  %-*s  %s\n", w, nm, .describe(x[[nm]])))
}

#' @rdname print_pipeline
#' @export
print.qw_pipeline <- function(x, n = 20, digits = 3, ...) {
  s  <- x$summary
  bg <- x$by_group
  kind <- if (inherits(x, "qw_phys_pipeline")) "Physical-measure pipeline result"
          else "QuantWealthR pipeline result"
  cat(kind, ": ", length(bg), " case(s)", sep = "")
  if (!is.null(s) && "dataset" %in% names(s))
    cat(" from ", length(unique(s$dataset)), " dataset(s)", sep = "")
  has_bias <- length(bg) && any(vapply(bg, function(g) !is.null(g$bias), logical(1)))
  cat(if (has_bias) "; bias simulations included" else "", "\n\n", sep = "")
  if (is.null(s) || !nrow(s)) {
    cat("(empty summary)\n")
  } else {
    show <- .round_df(s[seq_len(min(n, nrow(s))), , drop = FALSE], digits)
    # list-columns (e.g. confidence intervals) are shortened to "lo-hi"
    for (cl in names(show)) if (is.list(show[[cl]]))
      show[[cl]] <- vapply(show[[cl]], function(v)
        paste(format(round(as.numeric(v), digits)), collapse = "-"), character(1))
    print(show, row.names = FALSE)
    if (nrow(s) > n) cat("... ", nrow(s) - n, " more row(s): see x$summary\n", sep = "")
  }
  cat("\nParts: $summary (table above), $by_group (one entry per case:\n  ",
      paste0('"', utils::head(names(bg), 3), '"', collapse = ", "),
      if (length(bg) > 3) ", ..." else "", ")\n", sep = "")
  cat("Look at one case with x$by_group[[\"<case>\"]]; full structure: str(x, max.level = 2)\n")
  invisible(x)
}

#' @rdname print_pipeline
#' @export
print.qw_phys_pipeline <- function(x, n = 20, digits = 3, ...) print.qw_pipeline(x, n, digits, ...)

#' @rdname print_pipeline
#' @export
print.qw_group <- function(x, digits = 3, ...) {
  ineq <- x$inequality
  cat("Case \"", x$case %||% x$group, "\": ", x$n, " graves", sep = "")
  if (is.list(ineq) && !is.null(ineq$n_adult))
    cat(" (", ineq$n_adult, " adult, ", ineq$n_subadult %||% 0, " subadult)", sep = "")
  if (!is.null(x$date) && !is.na(x$date)) {
    rng <- if (!is.null(x$date_min) && !isTRUE(x$date_min == x$date_max))
      sprintf(" (range %s to %s)", x$date_min, x$date_max) else ""
    cat(", date ", x$date, rng, sep = "")
  }
  cat("\n")
  f <- function(v) format(round(as.numeric(v), digits))
  gci <- function(v) if (length(v) >= 3) sprintf("%s [%s-%s]", f(v[1]), f(v[2]), f(v[3])) else f(v[1])
  if (is.list(ineq)) {
    if (!is.null(ineq$median_wealth))
      cat("Adults: median wealth ", f(ineq$median_wealth), ", mean ", f(ineq$mean_wealth),
          ", max ", f(ineq$max_wealth), "\n", sep = "")
    g <- c(combined = "gini_combined", prestige = "gini_prestige", TOT = "gini_tot",
           subadults = "gini_subadult")
    g <- g[vapply(g, function(k) !is.null(ineq[[k]]), logical(1))]
    if (length(g))
      cat("Gini [95% CI]: ", paste(sprintf("%s %s", names(g),
          vapply(g, function(k) gci(ineq[[k]]), character(1))), collapse = "; "), "\n", sep = "")
  }
  if (!is.null(x$bias))
    cat("Bias simulation: ", x$bias$n_sims, " runs (x$bias)\n", sep = "")
  .list_parts(unclass(x), "x")
  invisible(x)
}

#' @rdname print_pipeline
#' @export
print.qw_bias <- function(x, digits = 3, ...) {
  cat("Category-bias simulation: ", x$n_sims, " simulations, ",
      x$n_original_cols, " grave-good types", sep = "")
  if (!is.null(x$graves_used)) cat(", graves: ", x$graves_used, sep = "")
  cat("\n")
  for (m in c("prestige", "TOT")) {
    o  <- x[[paste0("orig_gini_", m)]]
    sp <- x[[paste0("ginis_split_", m)]]
    lp <- x[[paste0("ginis_lump_", m)]]
    if (is.null(o)) next
    rg <- function(v) if (length(v)) paste(format(round(range(v, na.rm = TRUE), digits)),
                                           collapse = " to ") else "-"
    cat(sprintf("  Gini (%s): original %s; split %s; lump %s\n",
                m, format(round(o, digits)), rg(sp), rg(lp)))
  }
  cat("\n", length(x), " parts, e.g. x$ginis_split_prestige (one Gini per simulation). ",
      "All names: names(x)\n", sep = "")
  invisible(x)
}
