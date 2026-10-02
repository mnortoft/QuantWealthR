# ---- Internal utility functions ----

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Format BCE/CE year range as a readable string
#'
#' @param start_year Numeric. Start year (negative = BCE).
#' @param end_year Numeric. End year (negative = BCE).
#' @return Character string such as "2500 - 2200 BCE".
#' @examples
#' format_dates(-2500, -2200)   # "2500-2200 BCE"
#' format_dates(-100, 300)      # range crossing year 0
#' format_dates(1000, 1200)
#' @export
format_dates <- function(start_year, end_year) {
  fmt <- function(y) if (y < 0) paste0(abs(y), " BCE") else paste0(y, " CE")
  if (start_year < 0 && end_year < 0) {
    paste0(abs(start_year), " - ", abs(end_year), " BCE")
  } else if (start_year >= 0 && end_year >= 0) {
    paste0(start_year, " - ", end_year, " CE")
  } else {
    paste0(fmt(start_year), " - ", fmt(end_year))
  }
}


#' Identify count data columns in a data frame
#'
#' Returns names of columns that are purely numeric and not metadata columns
#' (lat, lng, adult_count, subadult_count, etc.). The first column (ID) is
#' always excluded.
#'
#' @param df A data frame. First column should be the unit ID.
#' @return Character vector of count-data column names.
#' @examples
#' df <- data.frame(id = 1:5, sword = c(1, 0, 2, 0, 1), shield = c(0, 1, 0, 1, 0),
#'                  adult_count = 1, site = "X")
#' identify_count_columns(df)   # "sword" "shield": IDs, text and age counts are left out
#' @export
identify_count_columns <- function(df) {
  col_names       <- names(df)
  col_names_lower <- tolower(col_names)
  excluded        <- c("lat", "lng", "adult_count", "subadult_count",
                       "prestige_total", "combined_total", "tot",
                       "is_adult_grave", "is_subadult_only")

  result <- character(0)
  for (i in seq(2, ncol(df))) {
    nm <- col_names[i]
    if (is.na(nm) || nchar(trimws(nm)) == 0) next
    if (tolower(nm) %in% excluded) next
    vals         <- df[[i]]
    non_na       <- vals[!is.na(vals)]
    if (length(non_na) == 0) next
    numeric_vals <- suppressWarnings(as.numeric(non_na))
    if (!any(is.na(numeric_vals))) result <- c(result, nm)
  }
  result
}


#' Read a CSV or Excel data file and clean zeros to NA
#'
#' Auto-detects the CSV separator. Zeros in numeric columns (except column 1)
#' are converted to NA to represent absence -- this is meant for grave-good /
#' artefact count columns, where 0 should be read as "not recorded" rather
#' than "recorded as none". `lat`, `lng`, `adult_count`, and `subadult_count`
#' are excluded from this conversion, because 0 is a genuine, informative
#' value for all four (a real coordinate near the equator/prime meridian, or
#' a grave with zero adults/subadults rather than missing age data).
#'
#' For single-column physical measure data (e.g. house area), use
#' [read_physical_data()] instead.
#'
#' @param filepath Path to a CSV (.csv) or Excel (.xlsx / .xls) file.
#' @return A data frame with zeros converted to NA in numeric columns (except
#'   `lat`, `lng`, `adult_count`, `subadult_count`).
#' @examples
#' # The bundled example data (15 graves)
#' df <- read_count_data(example_data_path())
#' str(df)
#'
#' # Your own file: CSV (separator detected automatically) or Excel
#' \dontrun{
#' df <- read_count_data("my_graves.csv")
#' df <- read_count_data("my_graves.xlsx")   # needs the readxl package
#' }
#' @export
read_count_data <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read Excel files.")
    df <- as.data.frame(readxl::read_excel(filepath))
  } else {
    sep <- .auto_detect_sep(filepath)
    df  <- read.csv(filepath, sep = sep, dec = if (sep == ";") "," else ".",
                    stringsAsFactors = FALSE)
  }
  zero_to_na_excluded <- c("lat", "lng", "adult_count", "subadult_count")
  if (ncol(df) > 1) {
    for (i in 2:ncol(df)) {
      if (tolower(names(df)[i]) %in% zero_to_na_excluded) next
      num <- suppressWarnings(as.numeric(df[[i]]))
      if (!all(is.na(num))) {
        num[num == 0] <- NA
        df[[i]] <- num
      }
    }
  }
  df
}


#' Read a CSV or Excel file of physical measure data
#'
#' Companion to [read_count_data()] for physical measures (e.g. house floor
#' area, grave pit depth, mound volume). Unlike the count-data reader, this
#' applies no zero-to-NA cleaning: the file is read as-is into a data frame.
#' You choose which column to analyse later, at the inequality stage -- e.g.
#' by passing `df$area_m2` to [physical_measure_summary()] or `measure_col`
#' to [phys_pipeline()].
#'
#' @param filepath Path to a CSV (.csv) or Excel (.xlsx / .xls) file.
#' @return A data frame containing the file's contents, unmodified (the CSV
#'   separator is auto-detected).
#' @examples
#' # Write a small house table to a temporary file and read it back
#' houses <- data.frame(house = paste0("H", 1:8),
#'                      area_m2 = c(40, 55, 60, 75, 80, 90, 120, 350))
#' f <- tempfile(fileext = ".csv")
#' write.csv(houses, f, row.names = FALSE)
#' h <- read_physical_data(f)
#' physical_measure_summary(h$area_m2)
#' @export
read_physical_data <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read Excel files.")
    df <- as.data.frame(readxl::read_excel(filepath))
  } else {
    sep <- .auto_detect_sep(filepath)
    df  <- read.csv(filepath, sep = sep, dec = if (sep == ";") "," else ".",
                    stringsAsFactors = FALSE)
  }
  df
}


.auto_detect_sep <- function(filepath) {
  lines <- readLines(filepath, n = 5, warn = FALSE)
  if (length(lines) == 0) return(";")
  seps   <- c(";", ",", "\t")
  counts <- sapply(seps, function(s) {
    median(sapply(lines, function(l)
      sum(gregexpr(s, l, fixed = TRUE)[[1]] > 0)), na.rm = TRUE)   # gregexpr gives -1 for no match
  })
  counts[is.na(counts)] <- 0
  if (max(counts) == 0) return(";")
  seps[which.max(counts)]
}


#' Path to the bundled example datasets
#'
#' Returns the file path to one of the two example CSV files included with
#' the package. Load them with [read_count_data()].
#'
#' * `"graves"` (default): 15 graves from one fictitious site, with grave-good
#'   counts and `adult_count` / `subadult_count`. Small enough to inspect by
#'   eye; used in the examples of the single-dataset functions.
#' * `"phases"`: 220 **simulated** graves from two fictitious cultures, `A`
#'   and `B`, from five fictitious sites each (`Site`, with `lat` / `lng`),
#'   with three phases each (`phase`, e.g. `"A1"`), a phase date
#'   (`date_BCE`, negative = BCE), how each grave was dated
#'   (`dating_method`), age counts, and 17 grave-good types (prefixes: `c_`
#'   pottery, `f_` flint, `s_` stone, `cu_` copper, `o_` ornaments, `b_`
#'   bone, `CON_` construction; culture A records no constructions). 12
#'   graves of culture A have no phase (`dating_method = "random
#'   imputation"`), for [impute_phases()]. Used for the multi-phase and
#'   cross-culture functions ([count_pipeline()], [plot_gini_timeline()],
#'   [compare_cases()], [multiverse_compare()], ...). The values are random,
#'   not real archaeological data; the script that made them is in the
#'   package source (`data-raw/make_phases_example.R`).
#'
#' @param which `"graves"` (default) or `"phases"`.
#' @return A character string file path.
#' @examples
#' # Single site
#' df <- read_count_data(example_data_path())
#' head(df)
#'
#' # Two simulated cultures with phases
#' ph <- read_count_data(example_data_path("phases"))
#' table(ph$culture, ph$phase, useNA = "ifany")
#'
#' # Split into one data frame per culture, as count_pipeline() expects
#' cultures <- split(ph, ph$culture)
#' @export
example_data_path <- function(which = c("graves", "phases")) {
  which <- match.arg(which)
  file <- switch(which, graves = "quantwealth_example.csv",
                 phases = "quantwealth_phases_example.csv")
  system.file("extdata", file, package = "QuantWealthR")
}

#' Stop if an ID vector contains duplicates
#'
#' Several steps look rows up by ID (e.g. the observed wealth scores in
#' [simulate_category_bias()]). With duplicated IDs, `match()` silently
#' returns the first row for every copy, giving rows the wrong values. This
#' stops with a message naming the repeated IDs instead.
#' @param ids Vector of IDs.
#' @param where Character. Where the IDs come from, for the message.
#' @noRd
.check_unique_ids <- function(ids, where) {
  ids <- as.character(ids)
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) == 0) return(invisible(TRUE))
  n_rows <- sum(ids %in% dup)
  shown  <- paste(utils::head(dup, 10), collapse = ", ")
  more   <- if (length(dup) > 10) sprintf(" (and %d more)", length(dup) - 10) else ""
  stop(sprintf(paste0(
    "%s: %d ID(s) occur more than once (%d rows in total): %s%s.\n",
    "Every row needs a unique ID, otherwise rows can receive another row's ",
    "wealth values. Give each row (e.g. each burial event) its own ID, or ",
    "pass a unique column as `id_col`."),
    where, length(dup), n_rows, shown, more), call. = FALSE)
}


# Symbols used in plot labels. The plain pdf() and postscript() devices
# cannot draw them (on Windows this is an error, elsewhere a dot), so with
# one of those devices open, or with options(QuantWealthR.unicode = FALSE),
# ASCII stand-ins are used instead.
.sym <- function(name) {
  ok <- getOption("QuantWealthR.unicode", NA)
  if (is.na(ok)) ok <- !names(grDevices::dev.cur()) %in% c("pdf", "postscript")
  if (isTRUE(ok)) {
    switch(name, epsilon = "\u03b5", sqrt_count = "\u221acount", arrow = "\u2192",
           dash = "\u2014")
  } else {
    switch(name, epsilon = "e", sqrt_count = "sqrt(count)", arrow = "->", dash = "-")
  }
}

# Title suffix for the age group analysed ("" if unknown, e.g. older objects)
.age_label <- function(graves) {
  if (is.null(graves) || length(graves) != 1) return("")
  switch(graves, adults = " (adults)", all = " (all ages)", subadults = " (subadults)", "")
}
