# ============================================================================
# Multiple imputation of phases for undated burials
# ============================================================================

#' Make several imputed versions of a dataset (phase imputation)
#'
#' Rows whose phase was assigned at random (e.g. `dating_method ==
#' "random imputation"`) are given a new random phase in each of `n_imp`
#' copies of the data. One draw is made per burial event, so individuals
#' buried together always share a phase. The draw probabilities are the
#' phase shares among the dated burial events (each event counted once).
#' Rows that are dated, or inherit their date from a dated co-burial, are
#' never changed.
#'
#' **Restricted imputation.** Some undated graves can still be limited to a
#' few phases, e.g. aceramic Corded Ware graves that radiocarbon shows to be
#' early or middle, never late. Give them their own value in `method_col`
#' (e.g. `"restricted imputation"`) and list the allowed phases in
#' `restrict`. Their draws then use only the allowed phases. The
#' probabilities come from a reference group of dated graves (`ref`, e.g.
#' the radiocarbon-dated aceramic graves) if it has at least `min_ref`
#' dated burial events in the allowed phases; otherwise from all dated
#' graves in the allowed phases (rescaled to 100%).
#'
#' The result is a list of data frames, ready for [multiverse_compare()]
#' (give it as the dataset, or use its `impute` argument) or for running
#' [count_pipeline()] on each copy and passing the list of results to
#' [compare_cases()].
#'
#' @param df A data frame (one row per grave/burial unit).
#' @param phase_col Column with the phase label (overwritten for imputed rows).
#' @param method_col Column that marks how a row was dated.
#' @param impute_value Value of `method_col` that marks a randomly imputed
#'   row (default `"random imputation"`).
#' @param unit_col Column identifying the row's burial unit (default
#'   `"context_id"`).
#' @param event_pattern Optional regular expression removed from `unit_col`
#'   to get the burial event, so that units split from one burial event
#'   (e.g. `"GRAB_12_ind5"` and `"GRAB_12_ind6"`) get the same draw. Default
#'   `"_ind[0-9+]+$"`; `NULL` uses `unit_col` as it is.
#' @param date_col Optional numeric date column, set for imputed rows to the
#'   phase's date. `dates` gives the date per phase; by default the most
#'   common date of that phase among the non-imputed rows is used (so the
#'   sign convention of the data, e.g. negative = BCE, is kept).
#' @param dates Optional named numeric vector, phase label -> date.
#' @param restrict Optional named list for restricted imputation. Each name
#'   is a value of `method_col`; each element is either a character vector
#'   of allowed phases, or a list with `phases` (allowed phases) and `ref`
#'   (a logical vector, one value per row of `df`, marking the reference
#'   graves whose dated members give the probabilities). Example:
#'   `list("restricted imputation" = list(phases = c("early", "middle"),
#'   ref = tolower(df$Typochronology) == "aceramic"))`.
#' @param min_ref Minimum number of dated reference burial events needed to
#'   use `ref` for the probabilities (default 10); below that, all dated
#'   graves in the allowed phases are used, with a message.
#' @param n_imp Number of imputed copies (default 20).
#' @param seed Optional random seed, for reproducible imputations.
#' @param verbose Print the draw probabilities (default TRUE).
#' @return A list of `n_imp` data frames. Attribute `"probabilities"` holds
#'   the phase probabilities used (a list: one element per imputation
#'   type).
#' @examples
#' ph <- read_count_data(example_data_path("phases"))
#' A  <- ph[ph$culture == "A", ]
#' table(A$dating_method)   # 12 graves are "random imputation"
#'
#' imps <- impute_phases(A, phase_col = "phase", unit_col = "Grave_ID",
#'                       event_pattern = NULL, date_col = "date_BCE",
#'                       n_imp = 5, seed = 1)
#' length(imps)                          # 5 imputed copies of the data
#' table(imps[[1]]$phase)
#' attr(imps, "probabilities")
#'
#' # Restricted imputation: say some undated graves can only be A1 or A2,
#' # with probabilities from the dated graves of a reference group
#' A$dating_method[A$dating_method == "random imputation"][1:4] <- "restricted"
#' ref <- !is.na(A$f_blade)              # e.g. graves with a flint blade
#' imps_r <- impute_phases(A, phase_col = "phase", unit_col = "Grave_ID",
#'                         event_pattern = NULL, n_imp = 5, seed = 1,
#'                         restrict = list(restricted = list(phases = c("A1", "A2"),
#'                                                           ref = ref)))
#' attr(imps_r, "probabilities")
#' @export
impute_phases <- function(df, phase_col, method_col = "dating_method",
                          impute_value = "random imputation",
                          unit_col = "context_id", event_pattern = "_ind[0-9+]+$",
                          date_col = NULL, dates = NULL,
                          restrict = NULL, min_ref = 10,
                          n_imp = 20, seed = NULL, verbose = TRUE) {
  for (cl in c(phase_col, method_col, unit_col))
    if (!cl %in% names(df)) stop(sprintf("Column '%s' not found.", cl))
  if (!is.null(date_col) && !date_col %in% names(df))
    stop(sprintf("date_col '%s' not found.", date_col))
  if (!is.null(restrict) && (!is.list(restrict) || is.null(names(restrict)) ||
                             any(names(restrict) == "")))
    stop("restrict must be a named list (names = values of ", method_col, ").")
  if (impute_value %in% names(restrict))
    stop("'", impute_value, "' is the unrestricted imputation value; ",
         "give restricted rows their own value in ", method_col, ".")

  method <- as.character(df[[method_col]])
  phase  <- as.character(df[[phase_col]])
  event  <- as.character(df[[unit_col]])
  if (!is.null(event_pattern)) event <- sub(event_pattern, "", event)

  # which imputation type each row has ("" = not imputed)
  kinds <- c(impute_value, names(restrict))
  kind  <- ifelse(method %in% kinds, method, "")
  imp   <- kind != ""
  if (!any(imp)) {
    warning("No rows with ", method_col, " in (", paste0("'", kinds, "'", collapse = ", "),
            "); returning ", n_imp, " identical copies.")
    return(rep(list(df), n_imp))
  }
  # one imputation type per burial event (the first row's)
  ev_kind <- tapply(kind[imp], event[imp], function(k) k[1])
  if (any(tapply(kind[imp], event[imp], function(k) length(unique(k)) > 1)))
    warning("Some burial events mix imputation types; the first row's type is used.")
  und_events <- names(ev_kind)

  no_phase <- !imp & (is.na(phase) | phase == "")
  if (any(no_phase)) {
    tab <- table(method[no_phase], useNA = "ifany")
    warning(sum(no_phase), " row(s) have no phase and are not imputed, so they will drop ",
            "out of phase-based analyses. ", method_col, " of these rows: ",
            paste(sprintf("%s (%d)", names(tab), tab), collapse = ", "),
            ". Add them to `restrict` or set them to '", impute_value, "'.",
            call. = FALSE)
  }
  dated <- !imp & !event %in% und_events & !no_phase
  first <- dated & !duplicated(event)        # count each dated event once
  p_all <- prop.table(table(phase[first]))
  if (length(p_all) == 0) stop("No dated burial events to take phase probabilities from.")

  # probabilities per imputation type
  probs <- list()
  if (impute_value %in% ev_kind) probs[[impute_value]] <- p_all
  for (k in intersect(names(restrict), ev_kind)) {
    r <- restrict[[k]]
    ph  <- if (is.list(r)) r$phases else r
    ref <- if (is.list(r)) r$ref else NULL
    bad <- setdiff(ph, names(p_all))
    if (length(bad)) stop("restrict '", k, "': phase(s) with no dated graves: ",
                          paste(bad, collapse = ", "))
    use <- first & phase %in% ph
    src <- "all dated graves in these phases"
    if (!is.null(ref)) {
      if (length(ref) != nrow(df)) stop("restrict '", k, "': ref must have one value per row.")
      ref <- !is.na(ref) & as.logical(ref)
      ref_ev <- unique(event[ref])           # an event is a reference if any row is
      n_ref <- sum(use & event %in% ref_ev)
      if (n_ref >= min_ref) {
        use <- use & event %in% ref_ev
        src <- sprintf("%d dated reference graves", n_ref)
      } else if (verbose) {
        message("restrict '", k, "': only ", n_ref, " dated reference graves (< min_ref = ",
                min_ref, "); using all dated graves in the allowed phases.")
      }
    }
    pk <- prop.table(table(factor(phase[use], levels = ph)))
    attr(pk, "source") <- src
    probs[[k]] <- pk
  }

  if (!is.null(date_col) && is.null(dates)) {
    dv <- suppressWarnings(as.numeric(df[[date_col]]))
    dates <- vapply(names(p_all), function(ph) {
      x <- dv[!imp & phase == ph & !is.na(dv)]
      if (length(x) == 0) NA_real_ else as.numeric(names(which.max(table(x))))
    }, numeric(1))
  }
  if (verbose) for (k in names(probs)) {
    message(sprintf("'%s': %d burial events (%d rows) redrawn %d times. Probabilities%s: %s",
                    k, sum(ev_kind == k), sum(kind == k), n_imp,
                    if (is.null(attr(probs[[k]], "source"))) "" else
                      paste0(" (from ", attr(probs[[k]], "source"), ")"),
                    paste(sprintf("%s = %.1f%%", names(probs[[k]]), 100 * probs[[k]]),
                          collapse = ", ")))
  }

  if (!is.null(seed)) set.seed(seed)
  out <- lapply(seq_len(n_imp), function(i) {
    draw <- stats::setNames(character(length(und_events)), und_events)
    for (k in names(probs)) {
      e <- und_events[ev_kind == k]
      if (length(e)) draw[e] <- sample(names(probs[[k]]), length(e), replace = TRUE,
                                        prob = as.numeric(probs[[k]]))
    }
    d <- df
    newph <- unname(draw[event[imp]])
    if (is.factor(d[[phase_col]])) {
      d[[phase_col]] <- factor(as.character(d[[phase_col]]),
                               levels = union(levels(d[[phase_col]]), names(p_all)))
    }
    d[[phase_col]][imp] <- newph
    if (!is.null(date_col)) d[[date_col]][imp] <- unname(dates[newph])
    d
  })
  attr(out, "probabilities") <- probs
  out
}
