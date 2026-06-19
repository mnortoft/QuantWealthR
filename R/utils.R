# ---- Internal utility functions ----

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Format BCE/CE year range as a readable string
#'
#' @param start_year Numeric. Start year (negative = BCE).
#' @param end_year Numeric. End year (negative = BCE).
#' @return Character string such as "2500 - 2200 BCE".
#' @export
#' @examples
#' format_dates(-2500, -2200)
#' format_dates(-100, 300)
#' format_dates(1000, 1200)
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
#' @export
#' @examples
#' df <- data.frame(id = 1:5, sword = c(1,0,2,0,1), shield = c(0,1,0,1,0))
#' identify_count_columns(df)
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
#' @export
#' @examples
#' # Load the bundled example dataset
#' df <- read_count_data(example_data_path())
#'
#' \dontrun{
#' # Or load your own file
#' df <- read_count_data("my_graves.csv")
#' }
read_count_data <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read Excel files.")
    df <- as.data.frame(readxl::read_excel(filepath))
  } else {
    sep <- .auto_detect_sep(filepath)
    df  <- read.csv(filepath, sep = sep, stringsAsFactors = FALSE)
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
#' to [qw_pipeline()].
#'
#' @param filepath Path to a CSV (.csv) or Excel (.xlsx / .xls) file.
#' @return A data frame containing the file's contents, unmodified (the CSV
#'   separator is auto-detected).
#' @export
#' @examples
#' \dontrun{
#' houses