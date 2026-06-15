#---- Setup library path ----
tryCatch({
  log_path <- "/srv/shiny-server/quantwealth/startup_log_quantwealth.txt"
  writeLines("app.R started successfully", con = log_path)
}, error = function(e) {
  writeLines(paste("Error in startup logging:", e$message), con = log_path)
})


sink(log_path, append = TRUE)
cat("Library paths at app start:\n")
print(.libPaths())

# ---- Load required packages ----
required_packages <- c(
  "shiny", "shinyjs", "dplyr", "ggplot2", "DT",
  "DescTools", "gglorenz", "leaflet", "leaflet.extras",
  "DBI", "here", "scales", "plotly", "zoo", "gridExtra", "ggtext", "blsib", "readxl"
)

for (pkg in required_packages) {
  cat(sprintf("Trying to load: %s\n", pkg))
  tryCatch({
    library(pkg, character.only = TRUE)
    cat(sprintf("✓ Loaded: %s\n", pkg))
  }, error = function(e) {
    cat(sprintf("✗ Failed to load: %s — %s\n", pkg, conditionMessage(e)))
  })
}
sink()

log_path <- "/srv/shiny-server/quantwealth/startup_log_quantwealth.txt"

#function for easy checkpoint addition
safe_log <- function(msg) {
  try(write(msg, log_path, append = TRUE), silent = TRUE)
}

safe_log("Checkpoint: finished loading libraries")

#set the proper root path of the app:
app_root <- if (!is.null(sys.frame(1)$ofile)) {
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
db_path  <- file.path(app_root, "db", "quantwealth.sqlite")

#### DEPLOYMENT CONFIGURATION ####
# Change this when switching between test and production:
# - Test server:  "/quantwealth-test"
# - Production:   "" (empty string for root, or "/quantwealth" if in subfolder)
APP_BASE_URL <- ""

# Helper function to build URLs for saved files
build_file_url <- function(file_path) {
  # Strip "www/" prefix if present (Shiny serves www/ at root)
  clean_path <- gsub("^www/", "", file_path)
  # Combine base URL with file path
  paste0(APP_BASE_URL, "/", clean_path)
}

#getOption("timeout")  # Check current timeout (default 60 seconds)
#options(timeout = 300)  # Set to 5 minutes

## you can already check that the DB exists and fail early if you want
if (!file.exists(db_path))
  stop("Missing DB: ", db_path)

#### End of set up library and DB path ####

# Function to auto-detect column separator
# Function to auto-detect column separator
auto_detect_sep <- function(filepath) {
  # Read first few lines to detect separator
  lines <- readLines(filepath, n = 5, warn = FALSE)
  if (length(lines) == 0) return(";")  # default fallback
  
  separators <- c(";", ",", "\t")
  
  # Count occurrences of each separator in each line
  sep_counts <- sapply(separators, function(sep) {
    counts <- sapply(lines, function(line) {
      length(gregexpr(sep, line, fixed = TRUE)[[1]])
    })
    # Use median count to be robust against header differences
    median(counts[counts > 0], na.rm = TRUE)
  })
  
  # Replace NA with 0
  sep_counts[is.na(sep_counts)] <- 0
  
  # Return separator with highest consistent count
  if (max(sep_counts) == 0) return(";")  # fallback if no separators found
  
  best_sep <- separators[which.max(sep_counts)]
  return(best_sep)
}

# Function to read data file (CSV or Excel)
read_data_file <- function(file_input) {
  filepath <- file_input$datapath
  filename <- file_input$name
  
  # Determine file type from extension
  ext <- tolower(tools::file_ext(filename))
  
  if (ext %in% c("xlsx", "xls")) {
    # Read Excel file
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files. Please install it.")
    }
    df <- readxl::read_excel(filepath)
    df <- as.data.frame(df)
  } else {
    # Read CSV with auto-detected separator
    sep <- auto_detect_sep(filepath)
    df <- read.csv(filepath, sep = sep, stringsAsFactors = FALSE)
  }
  
  # Clean data: convert 0 to NA in numeric columns (except first column)
  if (ncol(df) > 1) {
    for (i in 2:ncol(df)) {
      if (is.numeric(df[[i]])) {
        df[[i]][df[[i]] == 0] <- NA
      } else {
        # Try to convert to numeric first
        numeric_col <- suppressWarnings(as.numeric(df[[i]]))
        if (!all(is.na(numeric_col))) {
          numeric_col[numeric_col == 0] <- NA
          df[[i]] <- numeric_col
        }
      }
    }
  }
  
  return(df)
}


# Helper function to extract coordinates from uploaded data
# Returns centroid if multiple different coordinates, or single coordinate if all same
extract_coordinates <- function(df) {
  if (!all(c("lat", "lng") %in% tolower(names(df)))) {
    return(NULL)
  }
  
  # Find the lat/lng columns (case-insensitive)
  lat_col <- names(df)[tolower(names(df)) == "lat"][1]
  lng_col <- names(df)[tolower(names(df)) == "lng"][1]
  
  lats <- as.numeric(df[[lat_col]])
  lngs <- as.numeric(df[[lng_col]])
  
  # Remove NAs
  valid <- !is.na(lats) & !is.na(lngs)
  lats <- lats[valid]
  lngs <- lngs[valid]
  
  if (length(lats) == 0) {
    return(NULL)
  }
  
  # Return centroid
  return(list(
    lat = mean(lats),
    lng = mean(lngs)
  ))
}

# Calculate number of unique sites and area/distance from coordinates
# Calculate number of unique sites and area/distance from coordinates
# Groups coordinates within ~1km (2 decimal places) as same site
calculate_sites_and_area <- function(df) {
  # Check if coordinates exist
  if (!all(c("lat", "lng") %in% tolower(names(df)))) {
    return(list(num_sites = 1, area_or_distance = NA, area_unit = NA,
                num_coords_raw = NA, coords_were_grouped = FALSE))
  }
  
  # Find the lat/lng columns (case-insensitive)
  lat_col <- names(df)[tolower(names(df)) == "lat"][1]
  lng_col <- names(df)[tolower(names(df)) == "lng"][1]
  lats <- as.numeric(df[[lat_col]])
  lngs <- as.numeric(df[[lng_col]])
  
  # Get unique coordinate pairs (raw)
  coords <- data.frame(lat = lats, lng = lngs)
  coords <- coords[!is.na(coords$lat) & !is.na(coords$lng), ]
  coords_unique_raw <- unique(coords)
  num_coords_raw <- nrow(coords_unique_raw)
  
  if (num_coords_raw == 0) {
    return(list(num_sites = 1, area_or_distance = NA, area_unit = NA,
                num_coords_raw = 0, coords_were_grouped = FALSE))
  }
  
  if (num_coords_raw == 1) {
    return(list(num_sites = 1, area_or_distance = NA, area_unit = NA,
                num_coords_raw = 1, coords_were_grouped = FALSE))
  }
  
  # Round to 2 decimals (~1km grid) for site grouping
  coords_unique_raw$lat_rounded <- round(coords_unique_raw$lat, 2)
  coords_unique_raw$lng_rounded <- round(coords_unique_raw$lng, 2)
  


  # Get unique rounded coordinates (these are the "sites")
  site_coords <- unique(coords_unique_raw[, c("lat_rounded", "lng_rounded")])
  names(site_coords) <- c("lat", "lng")
  num_sites <- nrow(site_coords)
  
  # Check if grouping occurred
  coords_were_grouped <- num_coords_raw > num_sites
  
  if (num_sites == 1) {
    return(list(num_sites = 1, area_or_distance = NA, area_unit = NA,
                num_coords_raw = num_coords_raw, coords_were_grouped = coords_were_grouped))
  }
  
  # Haversine distance function (returns km)
  haversine_km <- function(lat1, lng1, lat2, lng2) {
    R <- 6371
    dlat <- (lat2 - lat1) * pi / 180
    dlng <- (lng2 - lng1) * pi / 180
    a <- sin(dlat/2)^2 + cos(lat1 * pi/180) * cos(lat2 * pi/180) * sin(dlng/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    R * c
  }
  
  if (num_sites == 2) {
    dist_km <- haversine_km(site_coords$lat[1], site_coords$lng[1], 
                            site_coords$lat[2], site_coords$lng[2])
    return(list(num_sites = 2, area_or_distance = round(dist_km, 2), area_unit = "km",
                num_coords_raw = num_coords_raw, coords_were_grouped = coords_were_grouped))
  }
  
  # 3+ sites: calculate convex hull area
  hull_indices <- grDevices::chull(site_coords$lng, site_coords$lat)
  hull_coords <- site_coords[hull_indices, ]
  hull_coords <- rbind(hull_coords, hull_coords[1, ])
  
  lats_rad <- hull_coords$lat * pi / 180
  lngs_rad <- hull_coords$lng * pi / 180
  
  n <- nrow(hull_coords) - 1
  area_rad <- 0
  for (i in 1:n) {
    area_rad <- area_rad + (lngs_rad[i+1] - lngs_rad[i]) * (2 + sin(lats_rad[i]) + sin(lats_rad[i+1]))
  }
  area_rad <- abs(area_rad) / 2
  
  R <- 6371
  area_km2 <- area_rad * R^2 
  
  return(list(num_sites = num_sites, area_or_distance = round(area_km2, 2), area_unit = "km²",
              num_coords_raw = num_coords_raw, coords_were_grouped = coords_were_grouped))
}

# Identify which columns are grave good columns (purely numeric, not metadata)
identify_grave_good_columns <- function(df) {
  col_names <- names(df)
  col_names_lower <- tolower(col_names)
  # Always exclude these columns
  excluded_patterns <- c("lat", "lng", "adult_count", "subadult_count")
  grave_good_cols <- c()
  for (i in 2:ncol(df)) {  # Skip first column (ID)
    col_name <- col_names[i]
    # Skip if it matches excluded patterns
    if (tolower(col_name) %in% excluded_patterns) {
      next
    }
    # Check if column is purely numeric
    col_values <- df[[i]]
    non_na_values <- col_values[!is.na(col_values)]
    if (length(non_na_values) == 0) {
      next  # Skip empty columns
    }
    # If all non-NA values convert to numeric without new NAs, it's numeric
    numeric_values <- suppressWarnings(as.numeric(non_na_values)) 
    if (!any(is.na(numeric_values))) {
      grave_good_cols <- c(grave_good_cols, col_name)
    }
  } 
  return(grave_good_cols)
}

# Validate coordinates from uploaded data
# Returns list with: valid (boolean), error_message (string or NULL)
validate_coordinates <- function(df) {
  if (!all(c("lat", "lng") %in% tolower(names(df)))) {
    return(list(valid = TRUE, error_message = NULL))  # No coords to validate
  }
  
  # Find the lat/lng columns (case-insensitive)
  lat_col <- names(df)[tolower(names(df)) == "lat"][1]
  lng_col <- names(df)[tolower(names(df)) == "lng"][1]
  
  lats <- as.numeric(df[[lat_col]])
  lngs <- as.numeric(df[[lng_col]])
  
  # Remove NAs for validation
  valid_lats <- lats[!is.na(lats)]
  valid_lngs <- lngs[!is.na(lngs)]
  
  if (length(valid_lats) == 0 || length(valid_lngs) == 0) {
    return(list(valid = TRUE, error_message = NULL))  # No valid coords to check
  }
  
  # Check if lat values look like they might be longitude (outside -90 to 90)
  if (any(valid_lats > 90 | valid_lats < -90)) {
    return(list(
      valid = FALSE,
      error_message = paste0(
        "Your 'lat' column contains values outside the valid range (-90 to 90). ",
        "Values found: min=", round(min(valid_lats), 2), ", max=", round(max(valid_lats), 2), ". ",
        "These look like longitude values! Please ensure lat=latitude (Y-axis) and lng=longitude (X-axis), ",
        "or remove the coordinate columns if not needed."
      )
    ))
  }
  
  # Check if lng values are outside valid range
  if (any(valid_lngs > 180 | valid_lngs < -180)) {
    return(list(
      valid = FALSE,
      error_message = paste0(
        "Your 'lng' column contains values outside the valid range (-180 to 180). ",
        "Values found: min=", round(min(valid_lngs), 2), ", max=", round(max(valid_lngs), 2), ". ",
        "Please check your coordinate data or remove the coordinate columns if not needed."
      )
    ))
  }
  
  return(list(valid = TRUE, error_message = NULL))
}

# Check if data coordinates might be flipped (lat/lng swapped)
# Compares data centroid to where user clicked on map
check_coordinates_flipped <- function(data_coords, click_coords, threshold = 35) {
  if (is.null(data_coords) || is.null(click_coords$lat) || is.null(click_coords$lng)) {
    return(FALSE)
  }
  
  # Calculate simple degree distance (not geodesic, but sufficient for continent-scale check)
  lat_diff <- abs(data_coords$lat - click_coords$lat)
  lng_diff <- abs(data_coords$lng - click_coords$lng)
  distance <- sqrt(lat_diff^2 + lng_diff^2)
  
  if (distance > threshold) {
    # Check if swapping would bring them closer
    swapped_lat_diff <- abs(data_coords$lng - click_coords$lat)
    swapped_lng_diff <- abs(data_coords$lat - click_coords$lng)
    swapped_distance <- sqrt(swapped_lat_diff^2 + swapped_lng_diff^2)
    
    # If swapped coords are significantly closer to click, likely flipped
    return(swapped_distance < distance * 0.5)  # Swapped is at least 50% closer
  }
  
  return(FALSE)
}

format_dates <- function(start_year, end_year) {
      if (start_year < 0) {
        start_date <- paste0(abs(start_year), " BCE")
      } else {
        start_date <- paste0(start_year, " CE")
      }
      if (end_year < 0) {
        end_date <- paste0(abs(end_year), " BCE")
      } else {
        end_date <- paste0(end_year, " CE")
      }
      if (start_year < 0 && end_year < 0) {
        date_range <- paste0(abs(start_year), " - ", abs(end_year), " BCE")
      } else if (start_year >= 0 && end_year >= 0) {
        date_range <- paste0(start_year, " - ", end_year, " CE")
      } else {
        date_range <- paste0(start_date, " - ", end_date)
      }
      return(date_range)
    }


#### START OF MAX SPLITS FUNCTION ####
# Calculate adaptive max_splits based on shared strings in column names
# Returns a list with splits and the strings that matched
# Priority: 1) Prefix before underscore, 2) Special patterns, 3) 5-char substrings
calculate_adaptive_max_splits <- function(col_names, base_max_split = 5, min_split = 1, min_string_length = 5) {
  n_cols <- length(col_names)
  if (n_cols == 0) return(list(splits = integer(0), matched_strings = character(0)))
  # Single column case - no matching possible
  if (n_cols == 1) {
    result <- list(
      splits = setNames(base_max_split, col_names),
      matched_strings = NA_character_,
      col_names = col_names
    )
    return(result)
  }
  col_names_lower <- tolower(col_names)
  adjustments <- rep(0, n_cols)
  matched_strings <- rep(NA_character_, n_cols)
  
  # Special high-count patterns (Priority 2)
  special_patterns <- c("bead", "ornament", "pearl", "perle", "anhänger", "pendant", "pebble", "sherd", "shell", "arrowhead")
  
  for (i in seq_along(col_names)) {
    col_i <- col_names_lower[i]
    best_match_count <- 0
    best_match_string <- NA_character_
    
    # PRIORITY 1: Check prefix before underscore (even short ones like "c_")
    if (grepl("_", col_i)) {
      prefix <- sub("_.*", "", col_i)
      if (nchar(prefix) >= 1) {
        match_count <- sum(sapply(col_names_lower[-i], function(col_j) {
          grepl(paste0("^", prefix, "_"), col_j) || grepl(paste0("_", prefix, "_"), col_j)
        }))
        if (match_count > 0) {
          best_match_count <- match_count
          best_match_string <- paste0(prefix, "_")
        }
      }
    }
    
    # PRIORITY 2: Check special patterns (only if no prefix match)
    if (best_match_count == 0) {
      for (pat in special_patterns) {
        if (grepl(pat, col_i, fixed = TRUE)) {
          match_count <- sum(sapply(col_names_lower[-i], function(col_j) {
            grepl(pat, col_j, fixed = TRUE)
          }))
          if (match_count > best_match_count) {
            best_match_count <- match_count
            best_match_string <- pat
          }
        }
      }
    }
    
    # PRIORITY 3: Check for shared substrings of minimum length (only if no above matches)
    if (best_match_count == 0 && nchar(col_i) >= min_string_length) {
      for (start in 1:(nchar(col_i) - min_string_length + 1)) {
        for (len in min_string_length:min(10, nchar(col_i) - start + 1)) {
          substring_i <- substr(col_i, start, start + len - 1)
          match_count <- sum(sapply(col_names_lower[-i], function(col_j) {
            grepl(substring_i, col_j, fixed = TRUE)
          }))
          if (match_count > best_match_count) {
            best_match_count <- match_count
            best_match_string <- substring_i
          }
        }
      }
    }
    
    # Include self in count: 5 columns sharing "antler_" → adjustment = 5
    adjustments[i] <- if (best_match_count > 0) best_match_count + 1 else 0
    matched_strings[i] <- best_match_string
  }
  
  adjusted <- pmax(min_split, base_max_split - adjustments)
  names(adjusted) <- col_names
  
  list(
    splits = adjusted,
    matched_strings = matched_strings,
    col_names = col_names
  )
}
###END OF MAX SPLITS FUNCTION ####

### START OF CATEGORY BIAS FUNCTION ##############################
# Simulate category bias - fully vectorized version
simulate_category_bias <- function(df, n_sims = 100, base_max_split = 5, adult_counts = NULL) {
  
  tryCatch({
    # Identify grave good columns
    col_names <- names(df)
    excluded_patterns <- c("lat", "lng", "adult_count", "subadult_count", 
                           "prestige_total", "combined_total", "tot", 
                           "is_adult_grave", "is_subadult_only")
    
    gg_cols <- c()
    for (i in 2:ncol(df)) {
      col_name <- col_names[i]
      if (tolower(col_name) %in% excluded_patterns) next
      col_values <- df[[i]]
      non_na_values <- col_values[!is.na(col_values)]
      if (length(non_na_values) == 0) next
      numeric_values <- suppressWarnings(as.numeric(non_na_values))
      if (!any(is.na(numeric_values))) {
        # Skip columns where all values are 0 (no artifacts of this type)
        if (all(numeric_values == 0)) next
        gg_cols <- c(gg_cols, col_name)
      }
    }
    
    if (length(gg_cols) == 0) return(NULL)
    
    n_rows <- nrow(df)
    n_cols <- length(gg_cols)
    
    safe_log(paste("Bias sim: n_rows=", n_rows, "n_cols=", n_cols))
    
    # Calculate adaptive max_splits for each column
    adaptive_info <- calculate_adaptive_max_splits(gg_cols, base_max_split)
    adaptive_splits <- adaptive_info$splits
    matched_strings <- adaptive_info$matched_strings
    safe_log(paste("Adaptive splits range:", min(adaptive_splits), "-", max(adaptive_splits)))
    
    # Extract grave goods matrix
    gg_matrix <- as.matrix(df[, gg_cols, drop = FALSE])
    gg_matrix <- apply(gg_matrix, 2, as.numeric)
    # Ensure matrix structure is preserved for single column
    if (n_cols == 1) {
      gg_matrix <- matrix(gg_matrix, ncol = 1)
      colnames(gg_matrix) <- gg_cols
    }
    gg_matrix[is.na(gg_matrix)] <- 0

    safe_log(paste("gg_matrix dim:", nrow(gg_matrix), "x", ncol(gg_matrix)))
    safe_log(paste("gg_matrix class:", class(gg_matrix)))
    
    # Calculate ORIGINAL metrics
    orig_TOT <- rowSums(gg_matrix > 0)
    safe_log("orig_TOT calculated")
    orig_sqrt_bonus <- rowSums(ifelse(gg_matrix >= 1, sqrt(gg_matrix) - 1, 0))
    safe_log("orig_sqrt_bonus calculated")

    # Original mean prestige
    safe_log("Calculating type_means")
    type_means <- sapply(1:n_cols, function(j) {
      present_rows <- gg_matrix[, j, drop = FALSE] > 0
      if (sum(present_rows) == 0) return(0)
      mean(orig_TOT[present_rows])
    })
    type_means <- round(type_means, 2)
    safe_log(paste("type_means:", paste(type_means, collapse=", ")))
    # Handle single column case for sweep
    if (n_cols == 1) {
      orig_prestige <- (gg_matrix > 0) * type_means[1]
    } else {
      orig_prestige <- rowSums(sweep(gg_matrix > 0, 2, type_means, `*`))
    }
    safe_log("orig_prestige calculated")

    # Apply per-capita division if adult_counts provided
    if (!is.null(adult_counts) && length(adult_counts) == nrow(df)) {
      adult_counts_safe <- pmax(adult_counts, 1)
      orig_TOT        <- orig_TOT        / adult_counts_safe
      orig_sqrt_bonus <- orig_sqrt_bonus / adult_counts_safe
      orig_prestige   <- orig_prestige   / adult_counts_safe
    }
    
    # Original Ginis
    orig_gini_TOT <- DescTools::Gini(orig_TOT + 1, unbiased = TRUE)
    orig_gini_TOT_sqrt <- DescTools::Gini(orig_TOT + 1 + orig_sqrt_bonus, unbiased = TRUE)
    orig_gini_prestige <- DescTools::Gini(orig_prestige + 1, unbiased = TRUE)
    orig_gini_prestige_sqrt <- DescTools::Gini(orig_prestige + 1 + orig_sqrt_bonus, unbiased = TRUE)
    
    safe_log(paste("Original Ginis - TOT:", round(orig_gini_TOT, 3), 
                   "TOT+sqrt:", round(orig_gini_TOT_sqrt, 3),
                   "Prestige:", round(orig_gini_prestige, 3),
                   "Prestige+sqrt:", round(orig_gini_prestige_sqrt, 3)))
    
    # --- IDENTIFY LUMPABLE GROUPS ---
    # Find columns that share strings (candidates for lumping)
    col_names_lower <- tolower(gg_cols)
    lump_groups <- list()
    assigned <- rep(FALSE, n_cols)
    
    if (n_cols >= 2) {
    for (i in 1:(n_cols - 1)) {
      if (assigned[i]) next
      
      group <- i
      col_i <- col_names_lower[i]
      
      # Check prefix before underscore
      prefix_i <- if (grepl("_", col_i)) sub("_.*", "", col_i) else NULL
      
      for (j in (i + 1):n_cols) {
        if (assigned[j]) next
        col_j <- col_names_lower[j]
        
        matched <- FALSE
        
        # Check prefix match (exclude generic strings)
        excluded_prefixes <- c("presence", "present")
        if (!is.null(prefix_i) && nchar(prefix_i) >= 1 && !(prefix_i %in% excluded_prefixes)) {
          if (grepl(paste0("^", prefix_i, "_"), col_j)) {
            matched <- TRUE
          }
        }
        
        # Check 5+ char substring match
        # Exclude certain generic strings from triggering lumping
        excluded_strings <- c("presence", "present")
        
        if (!matched && nchar(col_i) >= 5) {
          for (len in 3:min(6, nchar(col_i))) {
            for (start in 1:(nchar(col_i) - len + 1)) {
              substr_i <- substr(col_i, start, start + len - 1)
              # Skip if this substring is in the exclusion list
              if (substr_i %in% excluded_strings) next
              if (grepl(substr_i, col_j, fixed = TRUE)) {
                matched <- TRUE
                break
              }
            }
            if (matched) break
          }
        }
        if (matched) {
          group <- c(group, j)
          assigned[j] <- TRUE
        }
      }
      if (length(group) > 1) {
        lump_groups[[length(lump_groups) + 1]] <- group
        assigned[i] <- TRUE
      }
    }
    } # end if (n_cols >= 2)
    
    n_lump_groups <- length(lump_groups)
    safe_log(paste("Found", n_lump_groups, "lumpable groups"))
    
    # Pre-allocate results
    ginis_split_TOT <- numeric(n_sims)
    ginis_split_TOT_sqrt <- numeric(n_sims)
    ginis_split_prestige <- numeric(n_sims)
    ginis_split_prestige_sqrt <- numeric(n_sims)

    # Mean wealth vectors for absolute Gini calculation
    means_split_TOT <- numeric(n_sims)
    means_split_TOT_sqrt <- numeric(n_sims)
    means_split_prestige <- numeric(n_sims)
    means_split_prestige_sqrt <- numeric(n_sims)
    means_lump_TOT <- numeric(n_sims)
    means_lump_TOT_sqrt <- numeric(n_sims)
    means_lump_prestige <- numeric(n_sims)
    means_lump_prestige_sqrt <- numeric(n_sims)
    
    ginis_lump_TOT <- numeric(n_sims)
    ginis_lump_TOT_sqrt <- numeric(n_sims)
    ginis_lump_prestige <- numeric(n_sims)
    ginis_lump_prestige_sqrt <- numeric(n_sims)

    # Track total columns created per simulation
    total_splits <- numeric(n_sims)
    total_lumped_cols <- numeric(n_sims)
    
# Calculate max count per column (can't split more than this)
# For high-count items (beads, ornaments, etc.), use sqrt to limit splits
high_count_patterns <- c("bead", "ornament", "pearl", "perle", "Anhänger", "pendant", "pebble", "sherd", "shell", "arrowhead", "tooth", "teeth")
is_high_count_col <- sapply(tolower(gg_cols), function(col) {
  any(sapply(high_count_patterns, function(pat) grepl(pat, col, fixed = TRUE)))
})

max_counts <- sapply(1:n_cols, function(j) {
  col_vals <- gg_matrix[, j]
  positive_vals <- col_vals[col_vals > 0]
  
  if (length(positive_vals) == 0) return(1)
  
  # Use median of positive values as base (more robust than max)
  median_val <- median(positive_vals)
  raw_max <- max(positive_vals)
  
  # Use median * 2 as reasonable split limit, but cap at actual max
  reasonable_limit <- min(ceiling(median_val), raw_max)
  
  if (is_high_count_col[j]) {
    min(ceiling(asinh(reasonable_limit)), 10)  # asinh-cap for beads etc.
  } else {
    reasonable_limit
  }
})

safe_log(paste("High-count columns (asinh-capped):", sum(is_high_count_col), 
               "of", n_cols, "-", paste(gg_cols[is_high_count_col], collapse=", ")))

# For high-count pattern columns, cap adaptive_splits at asinh(median)
# String matching can still reduce it, but this sets an upper bound
for (j in which(is_high_count_col)) {
  col_vals <- gg_matrix[, j]
  positive_vals <- col_vals[col_vals > 0]
  if (length(positive_vals) > 0) {
    median_val <- median(positive_vals)
    asinh_cap <- max(1, ceiling(asinh(median_val)))  # at least 1
    adaptive_splits[j] <- min(adaptive_splits[j], asinh_cap)
  }
}

# For high-count pattern columns, reset adaptive_splits to base_max_split
# These special patterns trump the general string matching rules
#adaptive_splits[is_high_count_col] <- base_max_split

safe_log(paste("High-count columns (sqrt-capped):", sum(is_high_count_col), 
               "of", n_cols, "-", paste(gg_cols[is_high_count_col], collapse=", ")))
    
    # Run simulations
    for (sim in 1:n_sims) {
      
      # ===== SPLITTING SIMULATION =====
      # Cap splits at both adaptive limit AND max count in column
      k_splits <- sapply(1:n_cols, function(j) {
        max_k <- min(adaptive_splits[j], max_counts[j])
        if (max_k <= 1) return(1)
        # 50% chance of no split, 50% chance of random split
        if (runif(1) < 0.5) return(1)
        sample(2:max_k, 1)
      })
      total_splits[sim] <- sum(k_splits) # splits stored for display in graphs

      virtual_list <- list()
      for (col_idx in 1:n_cols) {
        k <- k_splits[col_idx]
        col_vals <- gg_matrix[, col_idx]
        
        sub_matrix <- matrix(0, nrow = n_rows, ncol = k)
        for (row_idx in 1:n_rows) {
          v <- col_vals[row_idx]
          if (v == 0) next
          if (k == 1) {
            sub_matrix[row_idx, 1] <- v
          } else {
            bin_assignments <- sample(1:k, v, replace = TRUE)
            bins <- tabulate(bin_assignments, nbins = k)
            sub_matrix[row_idx, ] <- bins
          }
        }
        virtual_list[[col_idx]] <- sub_matrix
      }
      
      virtual_matrix <- do.call(cbind, virtual_list)
      
      sim_TOT <- rowSums(virtual_matrix > 0)
      sim_sqrt_bonus <- rowSums(ifelse(virtual_matrix >= 1, sqrt(virtual_matrix) - 1, 0))
      
      n_virtual <- ncol(virtual_matrix)
      virtual_means <- sapply(1:n_virtual, function(j) {
        present <- virtual_matrix[, j] > 0
        if (sum(present) == 0) return(0)
        mean(sim_TOT[present])
      })
      sim_prestige <- rowSums(sweep(virtual_matrix > 0, 2, virtual_means, `*`))

      if (!is.null(adult_counts) && length(adult_counts) == nrow(df)) {
        sim_TOT        <- sim_TOT        / adult_counts_safe
        sim_sqrt_bonus <- sim_sqrt_bonus / adult_counts_safe
        sim_prestige   <- sim_prestige   / adult_counts_safe
      }
      
      ginis_split_TOT[sim] <- DescTools::Gini(sim_TOT + 1, unbiased = TRUE)
      ginis_split_TOT_sqrt[sim] <- DescTools::Gini(sim_TOT + 1 + sim_sqrt_bonus, unbiased = TRUE)
      ginis_split_prestige[sim] <- DescTools::Gini(sim_prestige + 1, unbiased = TRUE)
      ginis_split_prestige_sqrt[sim] <- DescTools::Gini(sim_prestige + 1 + sim_sqrt_bonus, unbiased = TRUE)
      
      # Store means for absolute Gini
      means_split_TOT[sim] <- mean(sim_TOT + 1)
      means_split_TOT_sqrt[sim] <- mean(sim_TOT + 1 + sim_sqrt_bonus)
      means_split_prestige[sim] <- mean(sim_prestige + 1)
      means_split_prestige_sqrt[sim] <- mean(sim_prestige + 1 + sim_sqrt_bonus)

      # ===== LUMPING SIMULATION =====
      if (n_lump_groups > 0) {
        lumped_matrix <- gg_matrix
        cols_to_remove <- c()
        
        for (grp in lump_groups) {
          # Randomly decide how many to lump (at least 2, up to all)
          n_to_lump <- sample(2:length(grp), 1)
          cols_to_lump <- sample(grp, n_to_lump)
          
          # Sum the columns
          lumped_col <- rowSums(lumped_matrix[, cols_to_lump, drop = FALSE])
          
          # Keep first column, mark others for removal
          lumped_matrix[, cols_to_lump[1]] <- lumped_col
          cols_to_remove <- c(cols_to_remove, cols_to_lump[-1])
        }
        
        # Remove lumped columns
        if (length(cols_to_remove) > 0) {
          lumped_matrix <- lumped_matrix[, -cols_to_remove, drop = FALSE]
          total_lumped_cols[sim] <- ncol(lumped_matrix) #for display int he graphs
        }
        
        lump_TOT <- rowSums(lumped_matrix > 0)
        lump_sqrt_bonus <- rowSums(ifelse(lumped_matrix >= 1, sqrt(lumped_matrix) - 1, 0))
        
        n_lumped <- ncol(lumped_matrix)
        lump_means <- sapply(1:n_lumped, function(j) {
          present <- lumped_matrix[, j] > 0
          if (sum(present) == 0) return(0)
          mean(lump_TOT[present])
        })
        lump_prestige <- rowSums(sweep(lumped_matrix > 0, 2, lump_means, `*`))

        if (!is.null(adult_counts) && length(adult_counts) == nrow(df)) {
          lump_TOT        <- lump_TOT        / adult_counts_safe
          lump_sqrt_bonus <- lump_sqrt_bonus / adult_counts_safe
          lump_prestige   <- lump_prestige   / adult_counts_safe
        }
        
        ginis_lump_TOT[sim] <- DescTools::Gini(lump_TOT + 1, unbiased = TRUE)
        ginis_lump_TOT_sqrt[sim] <- DescTools::Gini(lump_TOT + 1 + lump_sqrt_bonus, unbiased = TRUE)
        ginis_lump_prestige[sim] <- DescTools::Gini(lump_prestige + 1, unbiased = TRUE)
        ginis_lump_prestige_sqrt[sim] <- DescTools::Gini(lump_prestige + 1 + lump_sqrt_bonus, unbiased = TRUE)
        # Store means for absolute Gini
        means_lump_TOT[sim] <- mean(lump_TOT + 1)
        means_lump_TOT_sqrt[sim] <- mean(lump_TOT + 1 + lump_sqrt_bonus)
        means_lump_prestige[sim] <- mean(lump_prestige + 1)
        means_lump_prestige_sqrt[sim] <- mean(lump_prestige + 1 + lump_sqrt_bonus)

      } else {
        # No lumpable groups - use original values
        ginis_lump_TOT[sim] <- orig_gini_TOT
        ginis_lump_TOT_sqrt[sim] <- orig_gini_TOT_sqrt
        ginis_lump_prestige[sim] <- orig_gini_prestige
        ginis_lump_prestige_sqrt[sim] <- orig_gini_prestige_sqrt
        total_lumped_cols[sim] <- n_cols
        # Use original means
        means_lump_TOT[sim] <- mean(orig_TOT + 1)
        means_lump_TOT_sqrt[sim] <- mean(orig_TOT + 1 + orig_sqrt_bonus)
        means_lump_prestige[sim] <- mean(orig_prestige + 1)
        means_lump_prestige_sqrt[sim] <- mean(orig_prestige + 1 + orig_sqrt_bonus)
      }
      
      if (sim %% 20 == 0) safe_log(paste("Bias sim:", sim, "of", n_sims))
    }
    
    list(
      ginis_split_TOT = ginis_split_TOT,
      ginis_split_TOT_sqrt = ginis_split_TOT_sqrt,
      ginis_split_prestige = ginis_split_prestige,
      ginis_split_prestige_sqrt = ginis_split_prestige_sqrt,
      ginis_lump_TOT = ginis_lump_TOT,
      ginis_lump_TOT_sqrt = ginis_lump_TOT_sqrt,
      ginis_lump_prestige = ginis_lump_prestige,
      ginis_lump_prestige_sqrt = ginis_lump_prestige_sqrt,
      orig_gini_TOT = round(orig_gini_TOT, 2),
      orig_gini_TOT_sqrt = round(orig_gini_TOT_sqrt, 2),
      orig_gini_prestige = round(orig_gini_prestige, 2),
      orig_gini_prestige_sqrt = round(orig_gini_prestige_sqrt, 2),
      means_split_TOT = means_split_TOT,
      means_split_TOT_sqrt = means_split_TOT_sqrt,
      means_split_prestige = means_split_prestige,
      means_split_prestige_sqrt = means_split_prestige_sqrt,
      means_lump_TOT = means_lump_TOT,
      means_lump_TOT_sqrt = means_lump_TOT_sqrt,
      means_lump_prestige = means_lump_prestige,
      means_lump_prestige_sqrt = means_lump_prestige_sqrt,
      orig_mean_TOT = mean(orig_TOT + 1),
      orig_mean_TOT_sqrt = mean(orig_TOT + 1 + orig_sqrt_bonus),
      orig_mean_prestige = mean(orig_prestige + 1),
      orig_mean_prestige_sqrt = mean(orig_prestige + 1 + orig_sqrt_bonus),
      n_sims = n_sims,
      n_lump_groups = n_lump_groups,
      adaptive_splits = adaptive_splits,
      total_splits = total_splits,
      total_lumped_cols = total_lumped_cols,
      n_original_cols = n_cols,
      matched_strings = matched_strings,
      col_names = gg_cols,
      max_counts = max_counts
    )
    
  }, error = function(e) {
    safe_log(paste("Bias sim ERROR:", e$message))
    showNotification(paste("Bias sim error:", e$message), type = "error", duration = 15)
    return(NULL)
  })
}
### END OF CATEGORY BIAS FUNCTION ##############################



# Format publication reference with DOI hyperlinks or ISBN lookups
# Handles multiple identifiers separated by ";"
format_publication_link <- function(pub_ref, pub_doi, for_html = FALSE) {
  has_ref <- !is.null(pub_ref) && pub_ref != ""
  has_id <- !is.null(pub_doi) && pub_doi != ""
  if (!has_ref && !has_id) {
    return(if (for_html) "" else "Not provided")}
  # If only reference, no identifiers - just return the text
  if (has_ref && !has_id) {
    return(pub_ref)}
  # Parse multiple identifiers (separated by ;)
  if (has_id) {
    ids <- trimws(unlist(strsplit(pub_doi, ";")))
    ids <- ids[ids != ""]  # Remove empty strings
  } else {
    ids <- c()}
  # Parse multiple references (separated by ;) to try to match with identifiers
  if (has_ref) {
    refs <- trimws(unlist(strsplit(pub_ref, ";")))
    refs <- refs[refs != ""]
  } else {
    refs <- c()}
  # Build linked identifiers
  format_single_id <- function(id, for_html) {
    # Check if it looks like a DOI (contains "doi" or starts with "10.")
    is_doi <- grepl("doi", id, ignore.case = TRUE) || grepl("^10\\.", id)
    if (is_doi) {
      # Clean and create DOI link
      id_clean <- gsub("^https?://(dx\\.)?doi\\.org/", "", id)
      url <- paste0("https://doi.org/", id_clean)
      label <- paste0("DOI: ", id_clean)
    } else {
      # Assume ISBN - link to WorldCat
      isbn_clean <- gsub("[^0-9X]", "", toupper(id))
      if (nchar(isbn_clean) >= 10) {  # Valid ISBN length
        url <- paste0("https://www.worldcat.org/isbn/", isbn_clean)
        label <- paste0("ISBN: ", id)
      } else {
        # Not a valid ISBN either - just return as text
        return(if (for_html) id else id)
      }}
    if (for_html) {
      return(paste0("<a href='", url, "' target='_blank'>", label, "</a>"))
    } else {
      return(htmltools::tags$a(href = url, target = "_blank", label))
    }}
  # Case 1: Same number of refs and IDs - pair them up
  if (length(refs) == length(ids) && length(refs) > 0) {
    if (for_html) {
      parts <- mapply(function(r, id) {
        linked_id <- format_single_id(id, TRUE)
        paste0(r, " (", linked_id, ")")
      }, refs, ids, SIMPLIFY = TRUE)
      return(paste(parts, collapse = "; "))
    } else {
      parts <- lapply(seq_along(refs), function(i) {
        htmltools::tagList(refs[i], " (", format_single_id(ids[i], FALSE), ")")
      })
      return(htmltools::tagList(
        lapply(seq_along(parts), function(i) {
          if (i == 1) parts[[i]] else htmltools::tagList("; ", parts[[i]])
        })))}}
  # Case 2: Only identifiers, no refs
  if (length(refs) == 0 && length(ids) > 0) {
    if (for_html) {
      linked <- sapply(ids, format_single_id, for_html = TRUE)
      return(paste(linked, collapse = "; "))
    } else {
      parts <- lapply(ids, format_single_id, for_html = FALSE)
      return(htmltools::tagList(
        lapply(seq_along(parts), function(i) {
          if (i == 1) parts[[i]] else htmltools::tagList("; ", parts[[i]])
        })))}}
  # Case 3: Only refs, no identifiers (old publications)
  if (length(refs) > 0 && length(ids) == 0) {
    return(paste(refs, collapse = "; "))}
  # Case 4: Mismatched numbers - show refs, then all IDs in parentheses
  if (length(refs) > 0 && length(ids) > 0) {
    ref_text <- paste(refs, collapse = "; ")
    if (for_html) {
      linked <- sapply(ids, format_single_id, for_html = TRUE)
      return(paste0(ref_text, " (", paste(linked, collapse = "; "), ")"))
    } else {
      parts <- lapply(ids, format_single_id, for_html = FALSE)
      id_list <- htmltools::tagList(
        lapply(seq_along(parts), function(i) {
          if (i == 1) parts[[i]] else htmltools::tagList("; ", parts[[i]])
        })
      )
      return(htmltools::tagList(ref_text, " (", id_list, ")"))
    }
  }
  # Fallback
  return(if (has_ref) pub_ref else "")
}

# Helper function to build dynamic plot/table titles
build_title <- function(base_title, culture = NULL, start_year = NULL, end_year = NULL) {
  # If no culture and no dates provided, return base title
  if ((is.null(culture) || culture == "") && 
      (is.null(start_year) || is.null(end_year))) {
    return(base_title)
  }
  
  # Build subtitle parts
  parts <- c()
  
  if (!is.null(culture) && culture != "") {
    parts <- c(parts, culture)
  }
  
  if (!is.null(start_year) && !is.null(end_year)) {
    date_str <- format_dates(start_year, end_year)
    parts <- c(parts, date_str)
  }
  
  if (length(parts) > 0) {
    subtitle <- paste(parts, collapse = ", ")
    return(paste0(base_title, "\n", subtitle))
  }
  
  return(base_title)
}

########################################################################################################################################################
### STORE PLOTS FUNCTION START #########################################################################################################################
save_plots_as_html <- function(file_path, plot_list, metadata, prestige_table = NULL) {
  tryCatch({
    safe_log("Step 1: Starting save_plots_as_html")

    full_path <- file.path(app_root, file_path)
    safe_log(paste("Step 2: full_path:", full_path))


    safe_log("Step 2.1: About to call dirname()")
    dir_path <- dirname(full_path)
    safe_log(paste("Step 2.2: dir_path:", dir_path))

    safe_log("Step 2.3: About to call dir.create()")
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    #dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
    safe_log("Step 3: Directory created")

    # Build the HTML components
    safe_log("Step 4: Building CSS")
    css_styles <- htmltools::tags$style(htmltools::HTML("
      body { font-family: Arial, sans-serif; margin: 20px; background-color: #f9f9f9; }
      .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; }
      .metadata { background: white; padding: 20px; border-radius: 8px; 
                  margin-bottom: 25px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .plot-section { background: white; padding: 20px; border-radius: 8px; 
                      margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .meta-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px; }
      .meta-item { padding: 10px; background: #f8f9fa; border-radius: 5px; }
      .label { font-weight: bold; color: #495057; }
      h1 { margin: 0; font-size: 2em; }
      h2 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
      .footer { text-align: center; padding: 20px; color: #666; margin-top: 30px; }
      a { color: #3498db; text-decoration: none; }
    "))
    
    # Header section
    safe_log("Step 5: Building header")
    header <- htmltools::tags$div(
      class = "header",
      htmltools::tags$h1("🏺 QuantWealth Inequality Analysis"),
      htmltools::tags$p(paste("Analysis Results for", metadata$culture))
    )
    
    # Metadata section
    safe_log("Step 6: Starting metadata section")
    metadata_section <- htmltools::tags$div(
      class = "metadata",
      htmltools::tags$h2("📊 Study Information"),
      htmltools::tags$div(
        class = "meta-grid",
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Culture/Period:"), htmltools::tags$br(),
          metadata$culture
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Date Range:"), htmltools::tags$br(),
          metadata$date_range
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Sample Size:"), htmltools::tags$br(),
          metadata$sample_size
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Method:"), htmltools::tags$br(),
          metadata$method %||% "Not specified"
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Publication:"), htmltools::tags$br(),
          {
            pub_result <- format_publication_link(metadata$publication_ref, metadata$publication_doi, for_html = FALSE)
            if (is.null(pub_result) || length(pub_result) == 0 || identical(pub_result, "")) "Not provided" else pub_result
          }
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Comments:"), htmltools::tags$br(),
          metadata$comments %||% "None"
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Data Type:"), htmltools::tags$br(),
          metadata$data_type %||% "Not specified"
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Contributor:"), htmltools::tags$br(),
          metadata$contributor_name
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Institution:"), htmltools::tags$br(),
          metadata$contributor_institution %||% "Not provided"
        ),
        htmltools::tags$div(class = "meta-item",
          htmltools::tags$span(class = "label", "Analysis Date:"), htmltools::tags$br(),
          metadata$date_added
        )
      )
    )
    
    # Start building the page
    page_elements <- list(css_styles, header, metadata_section)
    
    # Add each plot
    plot_titles <- list(
      prestige_plot = "📊 Prestige Values by Grave Good Type",
      gini_lorenz_plot = "📈 Inequality Analysis (Lorenz Curve, Density, and Decile Plots)",
      #decile_plot = "📊 Wealth Distribution by Decile",
      bias_plot = "🎲 Category Bias Simulation",
      house_gini_plot = "🏠 Size Inequality Analysis" 
      #house_decile_plot = "📊 Size Distribution by Decile"
    )
    
    #for (name in names(plot_list)) {
    #  if (!is.null(plot_list[[name]])) {
    #    plot_section <- htmltools::tags$div(
    #      class = "plot-section",
    #      htmltools::tags$h2(plot_titles[[name]] %||% name),
    #      plot_list[[name]]
    #    )
    #    page_elements <- c(page_elements, list(plot_section))
    #  }
    #}

    for (name in names(plot_list)) {
      if (!is.null(plot_list[[name]])) {
        # Modify bias plot to have explicit height in the widget itself
        plot_content <- if (name == "bias_plot") {
          bias_with_height <- plot_list[[name]] %>% 
            plotly::layout(height = 600)
          bias_with_height$sizingPolicy <- htmlwidgets::sizingPolicy(
            defaultWidth = "100%",
            defaultHeight = 600
          )
          bias_with_height
        } else {
          plot_list[[name]]
        }
        plot_section <- htmltools::tags$div(
          class = "plot-section",
          htmltools::tags$h2(plot_titles[[name]] %||% name),
          plot_content
        )
        page_elements <- c(page_elements, list(plot_section))
      }
    }
    
    # Add prestige table if provided
    if (!is.null(prestige_table) && nrow(prestige_table) > 0) {
      # Select relevant columns for display
      display_cols <- intersect(
        names(prestige_table), 
        c(names(prestige_table)[1], "prestige_total", "combined_total")
      )
      display_table <- prestige_table[, display_cols, drop = FALSE]
      
      table_section <- htmltools::tags$div(
        class = "plot-section",
        htmltools::tags$h2("📋 Prestige Values Data"),
        DT::datatable(
          display_table, 
          options = list(pageLength = 15, scrollX = TRUE),
          rownames = FALSE
        )
      )
      page_elements <- c(page_elements, list(table_section))
    }
    
    # Footer
    footer <- htmltools::tags$div(
      class = "footer",
      htmltools::tags$p("Generated by QuantWealth.eu Archaeological Inequality Tool"),
      htmltools::tags$p(
        "For more information, visit: ",
        htmltools::tags$a(href = "https://quantwealth.eu/documentation.html", target = "_blank", 
                          "QuantWealth Documentation")
      )
    )
    page_elements <- c(page_elements, list(footer))
    
    # Combine all elements
    full_page <- htmltools::tagList(page_elements)
    
    # Save HTML with dependencies in a lib subfolder
    lib_dir <- file.path(dirname(full_path), "lib")
    htmltools::save_html(full_page, full_path, libdir = lib_dir)
    
    Sys.chmod(full_path, mode = "0644")
    safe_log(paste("Successfully created plots HTML at:", full_path))
    return(TRUE)
    
  }, error = function(e) {
    safe_log(paste("Error saving plots HTML:", e$message))
    safe_log(paste("Error call:", deparse(e$call)))
    safe_log(paste("Full error:", conditionMessage(e)))
    return(FALSE)
  })
}
###########################################################################################################################################################
### STORE PLOTS FUNCTION END #############################################################################################################################

# Function to copy input CSV with metadata
# Function to save input data with metadata columns added
save_input_csv <- function(file_path, uploaded_file, metadata) {
  tryCatch({
    # Create full file path
    full_path <- file.path(app_root, file_path)
    safe_log(paste("Attempting to save CSV to:", full_path))
    
    # Ensure directory exists
    dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
    safe_log(paste("Directory exists/created:", dirname(full_path)))
    
    # Check if source file exists
    if (!file.exists(uploaded_file$datapath)) {
      safe_log(paste("ERROR: Source file does not exist:", uploaded_file$datapath))
      return(FALSE)
    }
    
    # Read the uploaded file
    df <- read_data_file(uploaded_file)
    
     # Add metadata columns to each row (will be added to a stored csv file)
    df$culture <- metadata$culture %||% ""
    df$date_range <- metadata$date_range %||% ""
    df$publication_ref <- metadata$publication_ref %||% ""
    df$publication_doi <- metadata$publication_doi %||% ""
    df$contributor_name <- metadata$contributor_name %||% ""
    df$contributor_institution <- metadata$contributor_institution %||% ""
    df$comments <- metadata$comments %||% ""
    df$data_type <- metadata$data_type %||% ""
    df$submission_date <- metadata$date_added %||% ""
    
    # Write the modified data
    write.csv(df, full_path, row.names = FALSE)
    
    # Verify file was created
    if (!file.exists(full_path)) {
      safe_log(paste("ERROR: File was not created:", full_path))
      return(FALSE)
    }
    safe_log(paste("File successfully saved. Size:", file.size(full_path), "bytes"))
    
    # Set file permissions
    Sys.chmod(full_path, mode = "0644")
    
    # Create accompanying metadata file
    metadata_path <- gsub("\\.csv$", "_metadata.txt", full_path)
    metadata_content <- paste0(
      "QuantWealth Data File Metadata\n",
      "==============================\n",
      "Culture/Period: ", metadata$culture, "\n",
      "Date Range: ", metadata$date_range, "\n", 
      "Sample Size: ", metadata$sample_size, "\n",
      "Publication Reference: ", metadata$publication_ref %||% "Not provided", "\n",
      "Publication DOI: ", metadata$publication_doi %||% "Not provided", "\n",
      "Contributor: ", metadata$contributor_name, "\n",
      "Institution: ", metadata$contributor_institution %||% "Not provided", "\n",
      "Comments: ", metadata$comments %||% "None", "\n",
      "Upload Date: ", metadata$date_added, "\n",
      "Original Filename: ", uploaded_file$name, "\n",
      "Calculation Method: ", metadata$method %||% "Not specified", "\n"
    )
    
    writeLines(metadata_content, con = metadata_path)
    Sys.chmod(metadata_path, mode = "0644")
    
    safe_log(paste("✅ Successfully saved data CSV and metadata at:", full_path))
    return(TRUE)
    
  }, error = function(e) {
    safe_log(paste("❌ Error saving input CSV:", e$message))
    return(FALSE)
  })
}


# Helper function to check if NULL
`%||%` <- function(x, y) if (is.null(x)) y else x

# Generate submission form fields (reusable across modals)
submission_form_fields <- function(coords_text = NULL) {
  tagList(
    if (!is.null(coords_text)) {
      p(coords_text, style = "color: #666; font-style: italic;")
    },
    textInput("publicationRef", "Publication Reference(s)", 
              placeholder = "Separate multiple with ;"),
    textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
              placeholder = "Separate multiple with ;"),
    textInput("contributorName", "Contributor Name"),
    textInput("contributorInstitution", "Contributor Institution"),
    textInput("contactEmail", "Contact Email"),
    textAreaInput("submissionComments", "Comments", 
                  placeholder = "Any additional information about the data, collection methods, etc.",
                  rows = 3),
    br(),
    h4("Optional File Attachments")
  )
}

#### This is where the actual app code starts ####
####Get TOT ranges####
get_TOT_range <- function(y){
  safe_log(paste("get_TOT_range called, ncol:", ncol(y), "nrow:", nrow(y)))
  #redo df_bin (because it only existed within the bin_adult function above and is not stored)
  #y[,-1] <- as.numeric(unlist(y[,-1]))
  #df_bin <- data.frame(Grave_ID = as.character(y[,1]), ifelse(is.na(y[,-1]), 0, 1))
  #df_bin$TOT <- rowSums(df_bin[,-1])

    #redo df_bin (because it only existed within the bin_adult function above and is not stored)
  # Handle single column case vs multiple columns
  if (ncol(y) == 2) {
    safe_log("Single column case detected")
    # Single type column case
    y[,2] <- as.numeric(y[,2])
    df_bin <- data.frame(
      Grave_ID = as.character(y[,1]),
      type1 = ifelse(is.na(y[,2]) | y[,2] == 0, 0, 1)
    )
    names(df_bin)[2] <- names(y)[2]
    df_bin$TOT <- df_bin[,2]
    safe_log(paste("df_bin created, ncol:", ncol(df_bin), "nrow:", nrow(df_bin)))
  } else {
    safe_log("Multiple column case")
    y[,-1] <- as.numeric(unlist(y[,-1]))
    df_bin <- data.frame(Grave_ID = as.character(y[,1]), ifelse(is.na(y[,-1]), 0, 1))
    df_bin$TOT <- rowSums(df_bin[,-1, drop = FALSE])
    safe_log(paste("df_bin created, ncol:", ncol(df_bin), "nrow:", nrow(df_bin)))
  }
  
  # Initialize an empty list to store the dataframes for each Type
  df_list <- list()
  
  # Loop over the columns of the dataframe (excluding Grave_ID and TOT)
  type_cols <- setdiff(names(df_bin), c("Grave_ID", "TOT"))
  safe_log(paste("type_cols:", paste(type_cols, collapse=", ")))
  for (i in type_cols) {
    safe_log(paste("Processing type:", i))
    # Create a new dataframe with the TOT scores for graves with the corresponding Type
    df_temp <- filter(df_bin, df_bin[[i]] == 1) %>%
      dplyr::select(Grave_ID, TOT) %>%
      mutate(Type = i)
    # Append the new dataframe to the list
    df_list[[i]] <- df_temp
  }
  safe_log(paste("df_list length:", length(df_list)))
  # Combine the list of dataframes into a single dataframe in long format
  TOT_range <- bind_rows(df_list)
  safe_log(paste("TOT_range created, nrow:", nrow(TOT_range)))
  return(TOT_range)
}

# Function to get TOT range separated by age groups
get_TOT_range_by_age <- function(df) {
  # Find age columns (case-insensitive)
  col_names <- names(df)
  col_names_lower <- tolower(col_names)
  
  adult_col <- col_names[col_names_lower == "adult_count"]
  subadult_col <- col_names[col_names_lower == "subadult_count"]
  
  adult_col <- if(length(adult_col) > 0) adult_col[1] else NULL
  subadult_col <- if(length(subadult_col) > 0) subadult_col[1] else NULL
  
  # Also identify coordinate columns to exclude
  lat_col <- col_names[col_names_lower == "lat"]
  lng_col <- col_names[col_names_lower == "lng"]

  # Get counts
  if (!is.null(adult_col)) {
    adult_counts <- as.numeric(df[[adult_col]])
    adult_counts[is.na(adult_counts)] <- 0
  } else {
    adult_counts <- rep(1, nrow(df))  # Assume all adults if no column
  }
  
  if (!is.null(subadult_col)) {
    subadult_counts <- as.numeric(df[[subadult_col]])
    subadult_counts[is.na(subadult_counts)] <- 0
  } else {
    subadult_counts <- rep(0, nrow(df))
  }
  
  # Classify graves
  is_adult_grave <- adult_counts >= 1 | (adult_counts == 0 & subadult_counts == 0)
  is_subadult_only <- adult_counts == 0 & subadult_counts >= 1
  
  # Remove age columns and coordinate columns from goods data for TOT calculation
  exclude_cols <- c(adult_col, subadult_col, lat_col, lng_col)
  exclude_cols <- exclude_cols[length(exclude_cols) > 0]  # Remove NULLs
  df_goods <- df[, !(names(df) %in% exclude_cols), drop = FALSE]
  
  # Convert to binary presence/absence
  #df_goods[, -1] <- lapply(df_goods[, -1], as.numeric)
  #df_bin <- data.frame(
  #  Grave_ID = as.character(df_goods[, 1]),
  #  ifelse(is.na(df_goods[, -1]), 0, 1)
  #)
  #df_bin$TOT <- rowSums(df_bin[, -1, drop = FALSE])
  
  # Convert to binary presence/absence
  if (ncol(df_goods) == 2) {
    # Single type column case
    df_goods[, 2] <- as.numeric(df_goods[, 2])
    df_bin <- data.frame(
      Grave_ID = as.character(df_goods[, 1]),
      type1 = ifelse(is.na(df_goods[, 2]) | df_goods[, 2] == 0, 0, 1)
    )
    names(df_bin)[2] <- names(df_goods)[2]
    df_bin$TOT <- df_bin[, 2]
  } else {
    df_goods[, -1] <- lapply(df_goods[, -1], as.numeric)
    df_bin <- data.frame(
      Grave_ID = as.character(df_goods[, 1]),
      ifelse(is.na(df_goods[, -1]), 0, 1)
    )
    df_bin$TOT <- rowSums(df_bin[, -1, drop = FALSE])
  }


  # Add age info to df_bin
  df_bin$is_adult_grave <- is_adult_grave
  df_bin$is_subadult_only <- is_subadult_only
  df_bin$adult_count <- adult_counts
  df_bin$subadult_count <- subadult_counts
  
  # Get type columns (exclude metadata)
  meta_cols <- c("Grave_ID", "TOT", "is_adult_grave", "is_subadult_only", "adult_count", "subadult_count")
  type_cols <- setdiff(names(df_bin), meta_cols)
  
  # Split by age
  adult_df <- df_bin[df_bin$is_adult_grave, ]
  subadult_df <- df_bin[df_bin$is_subadult_only, ]
  
  # Find types present in adult graves
  types_in_adult <- c()
  for (type in type_cols) {
    if (nrow(adult_df) > 0 && any(adult_df[[type]] == 1, na.rm = TRUE)) {
      types_in_adult <- c(types_in_adult, type)
    }
  }
  
  # Find subadult-exclusive types
  subadult_exclusive_types <- c()
  for (type in type_cols) {
    in_adult <- nrow(adult_df) > 0 && any(adult_df[[type]] == 1, na.rm = TRUE)
    in_subadult <- nrow(subadult_df) > 0 && any(subadult_df[[type]] == 1, na.rm = TRUE)
    if (!in_adult && in_subadult) {
      subadult_exclusive_types <- c(subadult_exclusive_types, type)
    }
  }
  
  # Build TOT range for adult graves
  adult_list <- list()
  for (type in types_in_adult) {
    temp <- adult_df[adult_df[[type]] == 1, c("Grave_ID", "TOT"), drop = FALSE]
    if (nrow(temp) > 0) {
      temp$Type <- type
      adult_list[[type]] <- temp
    }
  }
  adult_TOT_range <- if (length(adult_list) > 0) do.call(rbind, adult_list) else data.frame()
  
  # Build TOT range for subadult-exclusive types
  subadult_list <- list()
  for (type in subadult_exclusive_types) {
    temp <- subadult_df[subadult_df[[type]] == 1, c("Grave_ID", "TOT"), drop = FALSE]
    if (nrow(temp) > 0) {
      temp$Type <- type
      subadult_list[[type]] <- temp
    }
  }
  subadult_TOT_range <- if (length(subadult_list) > 0) do.call(rbind, subadult_list) else NULL
  
  return(list(
    adult_TOT_range = adult_TOT_range,
    subadult_TOT_range = subadult_TOT_range,
    subadult_exclusive_types = subadult_exclusive_types,
    types_in_adult = types_in_adult,
    df_bin = df_bin,
    df_goods = df_goods,
    adult_grave_count = sum(is_adult_grave),
    subadult_grave_count = sum(is_subadult_only)
  ))
}

####Get TOT medians####
get_TOT_medians <- function(x){
  x %>%
    dplyr::group_by(Type) %>%
    dplyr::summarise(median = median(TOT))
}

get_TOT_means <- function(x){
  x %>%
    dplyr::group_by(Type) %>%
    dplyr::summarise(mean = mean(TOT))
}


apply_prestige_presence <- function(x, y, measure_type = "median"){
  # Find the corresponding column indices in the original dataframe
  indices <- which(names(y) %in% x$Type)
  
  # Depending on measure_type, create a named list with either mean or median values
  if (measure_type == "mean") {
    values_list <- setNames(x$mean, x$Type)
  } else {
    values_list <- setNames(x$median, x$Type)
  }
  
  # Round the values in the values list to two decimals
  values_list <- round(values_list, 2)
  
  # Apply the values in the values list to the presence of the corresponding values in the graves dataframe
  Prestige_values <- y
  for (name in names(values_list)) {
    Prestige_values[[name]] <- ifelse(!is.na(y[[name]]) & y[[name]] > 0, values_list[[name]], 0)
  }
  
  Prestige_values2 <- Prestige_values
  Prestige_values2[is.na(Prestige_values2)] <- 0
  
  Prestige_values2$prestige_total <- round(rowSums(Prestige_values2[,-1, drop = FALSE], na.rm = TRUE), 2)+1
  return(Prestige_values2)
}

# Function to compute the Bayesian posterior mean given count vector y,
# with a Gamma prior: lambda ~ Gamma(alpha0, beta0)
bayesian_exclusivity <- function(y, alpha0 = 0.1, beta0 = 0.1) {
  y <- y[!is.na(y)]
  n <- length(y)
  if(n == 0) return(NA)
  alpha_post <- alpha0 + sum(y)
  beta_post <- beta0 + n
  # Posterior mean
  return(alpha_post / beta_post)
}

# Function to apply Bayesian exclusivity (prestige) weights to the grave good table.
# For each type, if a grave is present (non-NA and > 0), its weight becomes the Bayesian posterior mean.
apply_bayesian_presence <- function(bayes_values, y) {
  Prestige_values <- y
  for (name in bayes_values$Type) {
    exclusivity_val <- round(bayes_values$bayes_est[bayes_values$Type == name], 2)
    Prestige_values[[name]] <- ifelse(!is.na(y[[name]]) & y[[name]] > 0, exclusivity_val, 0)
  }
  Prestige_values[is.na(Prestige_values)] <- 0
  Prestige_values$prestige_total <- round(rowSums(Prestige_values[,-1, drop = FALSE], na.rm = TRUE), 2) + 1
  return(Prestige_values)
}

#### House Size Functions ####
# Function to validate house size data
validate_house_data <- function(house_df) {
  if (ncol(house_df) < 2) {
    return("Physical measure data must have at least 2 columns (Unit ID (e.g. house ID) and Size)")
  }
  
  # Check if house sizes are numeric and within reasonable range
  house_sizes <- as.numeric(house_df[,2])
  
  if (any(is.na(house_sizes))) {
    return("Sizes must be numeric values")
  }
  
  if (any(house_sizes < 1 | house_sizes > 99999)) {
    return("Sizes must be between 1 and 99999 m²")
  }
  
  return(NULL) # No errors
}

# Function to calculate house size Gini coefficient
calculate_house_gini <- function(house_sizes) {
  DescTools::Gini(house_sizes, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 2000)
}


# Add logs before risky commands
write("Checkpoint: functions defined successfully", log_path, append = TRUE)

# Checkpoint before defining user interface (UI):
write("Checkpoint: going to UI definition", log_path, append = TRUE)



#### Shiny app UI ####
ui <- fluidPage(
  #theme = bslib::bs_theme(version = 5),   # enables Bootstrap-5 for bslib popovers (for info button)
  isolate(write("Checkpoint: inside fluidPage UI block", log_path, append = TRUE)),
  useShinyjs(), # Initialize shinyjs
  #titlePanel("QuantWealth - Multivariate Wealth Display and Inequality Analysis and Time Atlas"),
  div(
    style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 15px;",
    h2("QuantWealth - Multivariate Wealth Display and Inequality Analysis and Time Atlas", style = "margin: 0;"),
    textOutput("dbCounts", inline = TRUE) # show how many data points are stored in the atlas
  ),

  # Welcome modal in UI for immediate display
    tags$div(
      id = "welcomeModal",
      class = "modal fade",
      tabindex = "-1",
      `data-backdrop` = "true",
      `data-keyboard` = "true",
      tags$div(
        class = "modal-dialog",
        style = "margin-top: 30px;",
        tags$div(
          class = "modal-content",
          tags$div(
            class = "modal-header",
            style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;",
            tags$h5(class = "modal-title", style = "font-size: 1.3em;", "🏺 Welcome to QuantWealth")
          ),
          tags$div(
            class = "modal-body",
            tags$p(
              tags$strong("QuantWealth"), 
              "is a tool for analyzing and mapping wealth display and inequality in archaeological data through time and space."
            ),
            tags$p(tags$strong("What you can do:"), style = "margin-bottom: 5px;"),
            tags$ul(
              class = "welcome-list",
              tags$li("Calculate ", tags$strong("prestige values"), " for grave goods (and other count data) based on their exclusivity ('Prestige') or simply from", tags$strong("Physical measures"), " (e.g. house areas in m2)"),
              tags$li("Measure ", tags$strong("inequality"), " using Gini coefficients and Lorenz curves"),
              tags$li(tags$em("Optional:"), " Add your results to an ", tags$strong("interactive map"), " with time filtering"),
              tags$li("Compare ", tags$strong("inequality trends"), " across regions and periods")
            ),
            tags$div(
              class = "quick-start",
              tags$p(tags$strong("Quick Start:"), style = "margin-bottom: 10px;"),
              tags$ol(
                tags$li(
                  "Upload a file (CSV or Excel) with your data",
                  tags$ul(
                    class = "data-format",
                    tags$li(tags$em("Grave goods:"), " First column = ID, other columns = numeric counts"),
                    tags$li(tags$em("Physical measures (e.g. house m2):"), " First column = ID, second column = measure data (e.g. house m2)")
                  )
                ),
                tags$li("Fill in culture/period name and date range"),
                tags$li('Click "Calculate Prestige Values" or "Calculate Physical measure (e.g. house m2) Gini"'),
                tags$li('Click "Calculate Gini & Lorenz Curve"'),
                tags$li(tags$em("Optional:"), ' Click "Add Results to Map" and select a location (or include lng/lat coordinates in your data)')
              )
            ),
            tags$p(
              tags$strong("Need help?"), 
              " See the ",
              tags$a(href = "documentation.html", target = "_blank", "QuantWealth Guide"),
              " for detailed instructions and explanations of the methods."
            )
          ),
          tags$div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-primary", `data-dismiss` = "modal", "Get Started")
          )
        )
      )
    ),

  # Attribution and documentation banner
  div(
    style = "background-color: #f8f9fa; border-bottom: 1px solid #dee2e6; padding: 8px 15px; margin-bottom: 10px; font-size: 0.85em; color: #495057; display: flex; justify-content: space-between; align-items: center;",
    span(
      "Created by ",
      tags$a(
        href = "https://researchprofiles.ku.dk/en/persons/mikkel-johansen-n%C3%B8rtoft/",
        target = "_blank",
        "Mikkel Nørtoft"
      ),
      " assisted by Claude, ",
      tags$a(
        href = "https://doi.org/10.31235/osf.io/wzcqy_v2",
        target = "_blank",
        "see paper for more details."  
      ), "This is only a calculation and visualization tool. All results and plots should be interpreted with caution and attention to the individual context behind each data point."
    ),
    tags$a(
      href = "documentation.html",
      target = "_blank",
      style = "color: #3498db;",
      icon("info-circle"),
      " What does this app do?"
    )
  ),

  tags$head(
        # GoatCounter analytics (privacy-friendly, no cookies)
    tags$script(`data-goatcounter` = "https://quantwealth.goatcounter.com/count",
                async = TRUE,
                src = "//gc.zgo.at/count.js"),
    tags$style(HTML("
     /* Reduce padding inside the sidebarPanel container */
      .sidebarPanel {
        padding-top: 5px; /* Reduces the top padding */
        padding-bottom: 5px; /* Reduces the bottom padding */
      }
     /* Style for input fields */
      .shiny-input-container input[type='number'],
      .shiny-input-container input[type='text'] {
        height: 2em; /* Adjust the height as needed */
        font-size: 0.8em; /* Adjust the font size as needed */
      }
      /* Style for input labels */
      .shiny-input-container label {
        margin-bottom: 0.01em; /* Adjust the space below labels */
        font-size: 0.9em; /* Adjust the label font size as needed */
      }
      /* Style for buttons */
      #go, #calcGiniLorenz, #addResult, #calcHouseGini, #addHouseResult {
        font-size: 1em; /* Adjust the font size for specific buttons */
        line-height: 1.0; /* Adjust line height for specific buttons */
      }
      /* Section dividers */
      .section-divider {
        border-top: 2px solid #ddd;
        margin: 15px 0;
        padding-top: 10px;
      }

      /* Welcome modal styling */
      .welcome-modal .modal-content {
        border-radius: 10px;
      }
      .welcome-modal .modal-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 8px 8px 0 0;
      }
      .welcome-modal .modal-title {
        font-weight: bold;
      }
      .welcome-list {
        padding-left: 20px;
      }
      .welcome-list li {
        margin-bottom: 8px;
      }
      .quick-start {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        margin: 15px 0;
      }
      .quick-start ol {
        margin-bottom: 0;
        padding-left: 20px;
      }
      .quick-start li {
        margin-bottom: 8px;
      }
      .data-format {
        font-size: 0.9em;
        color: #666;
        margin-left: 10px;
      }
    ")),
    tags$script(HTML("
  $(document).ready(function() {
    // For Firefox caching: Immediately disable buttons on page load
    $('#go, #calcGiniLorenz, #testCategoryBias, #addResult, #downloadData, #calcHouseGini, #addHouseResult')
      .prop('disabled', true)
      .addClass('disabled');
    
    // For Firefox caching: Immediately uncheck checkboxes on page load
    $('#hasAdultCount, #hasSubadultCount, #graveHasCoords, #houseHasCoords')
      .prop('checked', false);
  });
")),
    tags$script(HTML("
      $(document).ready(function() {
        Shiny.addCustomMessageHandler('immediate-modal', function(message) {});
        // Show welcome modal immediately
        $('#welcomeModal').modal('show');
      });
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Show welcome modal immediately
        $('#welcomeModal').modal('show');
        
        // Close modal on Enter key
        $(document).on('keydown', function(e) {
          if (e.key === 'Enter' && $('#welcomeModal').is(':visible')) {
            $('#welcomeModal').modal('hide');
          }
        });
      });
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          var slider = $('#timeSlider').data('ionRangeSlider');
          var throttleTimer = null;
          if (slider) {
            slider.update({
              onChange: function(data) {
                clearTimeout(throttleTimer);
                throttleTimer = setTimeout(function() {
                  Shiny.setInputValue('timeSlider', [data.from, data.to]);
                }, 1);
              },
              onFinish: function(data) {
                Shiny.setInputValue('timeSliderFinal', [data.from, data.to], {priority: 'event'});
              }
            });
          }
        }, 500);
      });
    "))
  ),
  
  # Map output
  leaflet::leafletOutput("mymap", height = "50vh"),
  

  absolutePanel(
    id = "timeSliderPanel",
    class = "panel panel-default",
    top = "45vh",
    left = "1vw",
    style = "z-index: 400; width: 400px; padding: 10px 15px; margin-bottom: -10vh; margin-top: 0.1vh;",
    sliderInput("timeSlider", "Select Time Range",
                min = -5500, max = 1500,
                value = c(-4000, -2000),
                step = 300, # Define step size if needed
                animate = TRUE # This can make the slider animate when dragging
    )
  ),
  tags$head(
    tags$style(HTML("
    @media (max-width: 768px) {
      #timeSliderPanel {
        top: 60vh;  // Adjust for smaller screens
        left: 5vw;
        right: 5vw;
      }
    }
  "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("showTrendPlot","Show Inequality Trend"),
      tags$a(
  href = "data_preparation_guide.html", 
  target = "_blank",
  style = "margin-left: 10px;",
  icon("file-alt"), " Data Guide"
),
    tags$a(
  href = "quantwealth_example.csv", 
  target = "_blank",
  style = "margin-left: 10px;",
  icon("file-alt"), " Get example data"
),

      

      verbatimTextOutput("selectedArea"),
      verbatimTextOutput("fileReadyStatus"),
      
      #### GRAVE GOODS SECTION ####
      h4("Count data Analysis (e.g. Grave Goods/Features)"),
      fileInput("datafile", "Upload data file (CSV or Excel)", accept = c(".csv", ".xlsx", ".xls")),
      p("Format: First column = (e.g. Grave) ID, Other columns = grave good counts (or other count data)", 
      style = "margin-top: -20px;"), 
      radioButtons("countType", "Count data type",
                   choices = list("Grave good/feature types" = "gravegoods", 
                   "Hoard objects" = "hoardTypes",
                                  "House objects" = "houseObjects"),
                   selected = "gravegoods", 
                   inline = TRUE),
      textInput("culture", "Culture or Period", value = ""),
      numericInput("startYear", "Start Year (BCE/CE)", value = -2500),
      numericInput("endYear", "End Year (BCE/CE)", value = -2200),
      # New radio button to choose the calculation method:
      radioButtons("calcMethod", "Prestige Calculation Method",
                   choices = list("Mean" = "mean",
                                  #"Median" = "median",
                                  #"Bayesian (gamma)" = "bayesian_default",
                                  "Bayesian (local empirical for n > 1, recommended)" = "bayesian_empirical"),
                   selected = "bayesian_empirical"),

      checkboxInput("hasAdultCount", "Data includes adult_count column", value = FALSE),
      checkboxInput("hasSubadultCount", "Data includes subadult_count column", value = FALSE),
      checkboxInput("graveHasCoords", "Data includes coordinates (lat, lng columns)", value = FALSE),
      actionButton("go", "Calculate Prestige Values", disabled = TRUE),
      actionButton("calcGiniLorenz", "Calculate Gini & Lorenz Curve", disabled = TRUE),
      actionButton("testCategoryBias", "Test Category Bias", disabled = TRUE),
      actionButton("addResult", "Add Results to Map", disabled = TRUE),
      downloadButton("downloadData", "Download Prestige Data", disabled = TRUE),
      
      
      #### HOUSE SIZE SECTION ####
      div(class = "section-divider",
          h4("Size Analysis (e.g. house floor m2)"),
          fileInput("houseDatafile", "Upload Physical measure (e.g. house m2) file (CSV or Excel)", accept = c(".csv", ".xlsx", ".xls")),
          p("Format: First column = Unit (e.g. house) ID, Second column = Physical measure (e.g. house m²)", 
            style = "font-size: 0.8em; color: #666;"),
          textInput("houseCulture", "Culture or Period", value = ""),
          numericInput("houseStartYear", "Start Year (BCE/CE)", value = -2500),
          numericInput("houseEndYear", "End Year (BCE/CE)", value = -2000),
          radioButtons("physicalDataType", "Data Type",
                       choices = c("house size (m2)",
                                   "storage area (m2)", 
                                   "mound area (m2)",
                                   "mound volume (m3)",
                                   "grave pit depth (cm)"),
                       selected = "house size (m2)"),
          conditionalPanel(
            condition = "input.physicalDataType == 'grave pit depth (cm)'",
            p("Note: Use adult graves only (children's graves are typically shallower due to body size)", 
              style = "font-size: 0.8em; color: #d9534f; font-style: italic;")
          ),
          actionButton("calcHouseGini", "Calculate physical measure (e.g. house) Gini", disabled = TRUE),
          checkboxInput("houseHasCoords", "Data includes coordinates (lat, lng columns)", value = FALSE),
          actionButton("addHouseResult", "Add Size Gini Results to Map", disabled = TRUE)
      )
    ),
    mainPanel(
      uiOutput("dynamicPlotsUI"),
      uiOutput("tablesUI"), #includes all three tables
      verbatimTextOutput("debug")
    )
  )
) # end of UI block

#Checkpoint for map load
write("Checkpoint: map UI loaded successfully, moving to server function", log_path, append = TRUE)

#### Shiny Server (server-specific code in the start) ####
server <- function(input, output, session) {
  tryCatch({
    safe_log("=== server() has started ===")
    
    
    
    

    safe_log(sprintf("app_root : %s", app_root))
    app_dir <- app_root
    safe_log(sprintf("Working directory inside server(): %s", app_dir))
    
    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    
    session$onSessionEnded(function() {
      DBI::dbDisconnect(db)
    })
    

    

    # Register resource paths for serving static files
    saved_files_path <- file.path(app_root, "saved_files")
    if (dir.exists(saved_files_path)) {
      addResourcePath("saved_files", saved_files_path)
      safe_log(paste("Added resource path for saved_files:", saved_files_path))
    } else {
      dir.create(saved_files_path, recursive = TRUE, showWarnings = FALSE)
      addResourcePath("saved_files", saved_files_path)
      safe_log(paste("Created and added resource path for saved_files:", saved_files_path))
    }
    
    # DB checkpoint
    safe_log("Checkpoint: DB connected and resource paths registered")
    
        # Database entry counts (read at startup)
    output$dbCounts <- renderText({
      tryCatch({
        count_results <- dbGetQuery(db, "SELECT COUNT(*) as n FROM results")$n
        count_houses <- dbGetQuery(db, "SELECT COUNT(*) as n FROM house_results")$n
        paste0("📊 ", count_results, " count | ", count_houses, " physical")
      }, error = function(e) {
        ""
      })
    })

    #### Reactive values (cross-function variables) ####
    giniLorenzGenerated <- reactiveVal(FALSE) # Reactive value to track if Gini/Lorenz plot is generated
    trendPlotGenerated <- reactiveVal(FALSE)
    
    
    prestigePlotGenerated <- reactiveVal(FALSE) # reactive value to track if prestige values plot has been generated
    
    #to check if plots can be stored:
    graveFilesReady <- reactiveVal(FALSE)
    houseFilesReady <- reactiveVal(FALSE)

    # House-related reactive values
    houseGiniGenerated <- reactiveVal(FALSE)
    houseDataUploaded <- reactiveVal(FALSE)
    
    # Track currently visible markers for efficient updates
    visibleGraveIds <- reactiveVal(c())
    visibleHouseIds <- reactiveVal(c())

    # Store actual plot objects for saving to HTML
    storedPrestigePlot <- reactiveVal(NULL) # relevant for graves only
    storedGiniLorenzPlot <- reactiveVal(NULL) #for graves
    storedHouseGiniPlot <- reactiveVal(NULL) # for houses
    storedDecilePlot <- reactiveVal(NULL) # for decile distribution
    storedHouseDecilePlot <- reactiveVal(NULL) # for house decile distribution
    categoryBiasResults <- reactiveVal(NULL) # for category bias simulation results
    storedBiasPlot <- reactiveVal(NULL) # for category bias plot
    storedBiasIQR <- reactiveVal(NULL)

    # Store plot outputs in a reactive list
    plotList <- reactiveVal(list())
    # Function to update the dynamic plot UI
    updatePlotUI <- function(newPlot) {
      currentPlots <- plotList()
      # Prepend newPlot so it appears at the top; adjust as desired.
      plotList(c(list(newPlot), currentPlots))
    }
    
    # Render the dynamic plots UI
    output$dynamicPlotsUI <- renderUI({
      do.call(tagList, plotList())
    })
    
    # Reactive value to store the coordinates of the drawn polygon
    drawnBounds <- reactiveVal(NULL)
    
    # Observe draw events
    observeEvent(input$mymap_draw_new_feature, {
      leafletProxy("mymap") %>% clearGroup('drawnItems') # Clear previous drawings when a new feature is started
      
      features <- input$mymap_draw_new_feature ## Handle the drawing features
      if (features$geometry$type == "Polygon" || features$geometry$type == "Rectangle"){
        drawnBounds(features$geometry$coordinates[[1]])
      }
    })
    
    # Use this observeEvent to clear drawings when desired, e.g., by a button or automatically
    observeEvent(input$resetDrawing, {
      # Reset the drawing layer
      leafletProxy("mymap") %>% clearGroup('drawnItems')
      drawnBounds(NULL)  # Optionally reset the bounds reactive value
    })
    
    # Capture form data and write to database when 'Submit' is clicked
    observeEvent(input$submitData, {
      removeModal()
      
      tryCatch({
        # Validate all required data exists
        if (is.null(giniCoeff()) || is.null(meanWealth()) || is.null(gini_x_mean()) || is.null(sampleSize())) {
          showNotification("Missing grave calculation data. Please calculate Prestige Values and Gini first.", type = "error")
          return()
        }
        
        if (is.null(clickPosition$lat) || is.null(clickPosition$lng)) {
          showNotification("Missing location data. Please click on the map to select a location.", type = "error")
          return()
        }
        
        # Calculate number of sites and area from uploaded data
        sites_info <- list(num_sites = 1, area_or_distance = NA, area_unit = NA)
        if (!is.null(input$datafile)) {
          tryCatch({
            df <- read_data_file(input$datafile)
            sites_info <- calculate_sites_and_area(df)
          }, error = function(e) {
            safe_log(paste("Error calculating sites/area:", e$message))
          })
        }

        #extract form inputs
        cultureInput <- input$culture
        currentDate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "CET")
        contributorNameInput <- input$contributorName #for the map popup of the new dot
        
        if (is.null(cultureInput) || cultureInput == "") {
          showNotification("Culture cannot be empty.", type = "error")
          return()
        }
        
#write data to the database
# Initialize file paths  
plot_path <- NULL
data_path <- NULL
files_date <- NULL

# Safe logical checks
submit_plots <- isTRUE(input$submitGravePlots)
submit_data <- isTRUE(input$submitGraveData)
grave_ready <- isTRUE(graveFilesReady())

safe_log(paste("Checkbox states - plots:", submit_plots, "data:", submit_data, "grave_ready:", grave_ready))

# Save files if checkboxes were checked
if (submit_plots && grave_ready) {
  contributor_name <- if(is.null(input$contributorName) || input$contributorName == "") "unknown" else input$contributorName
  safe_name <- gsub("[^a-zA-Z0-9\\-\\.]", "_", contributor_name)
  safe_culture <- gsub("[^a-zA-Z0-9\\-\\.]", "_", input$culture)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  plot_filename <- paste0("grave_", timestamp, "_", safe_culture, "_", safe_name, "_plots.html")
  plot_path <- file.path("saved_files", "graves", "plots", plot_filename)
  files_date <- currentDate
  safe_log(paste("Will save grave plots to:", plot_path))
}

if (submit_data && !is.null(input$datafile)) {
  contributor_name <- if(is.null(input$contributorName) || input$contributorName == "") "unknown" else input$contributorName
  safe_name <- gsub("[^a-zA-Z0-9\\-\\.]", "_", contributor_name)
  safe_culture <- gsub("[^a-zA-Z0-9\\-\\.]", "_", input$culture)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  data_filename <- paste0("grave_", timestamp, "_", safe_culture, "_", safe_name, "_data.csv")
  data_path <- file.path("saved_files", "graves", "data", data_filename)
  if (is.null(files_date)) files_date <- currentDate
  safe_log(paste("Will save grave data to:", data_path))
}

# Ensure all parameters have proper length and type for database
plot_path_db <- if(is.null(plot_path)) "" else as.character(plot_path)
data_path_db <- if(is.null(data_path)) "" else as.character(data_path)  
files_date_db <- if(is.null(files_date)) "" else as.character(files_date)

safe_log(paste("Final db params - plot_path:", plot_path_db, "data_path:", data_path_db, "files_date:", files_date_db))


# Use adult-only data if age separation exists
if (hasAgeData() && !is.null(df_with_prestige()$is_adult_grave)) {
  adult_wealth <- df_with_prestige()$combined_total[df_with_prestige()$is_adult_grave]
  medianWealthVal <- median(adult_wealth)
  minWealthVal <- min(adult_wealth)
  maxWealthVal <- max(adult_wealth)
} else {
  medianWealthVal <- median(df_with_prestige()$combined_total)
  minWealthVal <- min(df_with_prestige()$combined_total)
  maxWealthVal <- max(df_with_prestige()$combined_total)
}

# Prepare subadult values (NULL if not available)
gini_subadult_val <- if (!is.null(giniCoeffSubadult())) giniCoeffSubadult() else NA
mean_x_gini_subadult_val <- if (!is.null(giniCoeffSubadult()) && !is.null(meanWealthSubadult())) {
  round(giniCoeffSubadult() * meanWealthSubadult(), 2)
} else NA
mean_wealth_subadult_val <- if (!is.null(meanWealthSubadult())) round(meanWealthSubadult(), 2) else NA
median_wealth_subadult_val <- if (!is.null(medianWealthSubadult())) round(medianWealthSubadult(), 2) else NA
adult_count_val <- if (!is.null(adultGraveCount())) adultGraveCount() else NA
subadult_count_val <- if (!is.null(subadultGraveCount())) subadultGraveCount() else NA
        
# Get bias IQR values (NULL if not calculated)
bias_iqr <- storedBiasIQR()
bias_iqr_low <- if (!is.null(bias_iqr)) round(bias_iqr[1], 3) else NA
bias_iqr_high <- if (!is.null(bias_iqr)) round(bias_iqr[2], 3) else NA

# Get bias IQR values (NULL if not calculated)
bias_iqr <- storedBiasIQR()
bias_iqr_low <- if (!is.null(bias_iqr)) round(bias_iqr[1], 3) else NA
bias_iqr_high <- if (!is.null(bias_iqr)) round(bias_iqr[2], 3) else NA

#Grave database write
dbExecute(db, "INSERT INTO results (lat, lng, gini, mean_wealth, median_wealth, min_wealth, max_wealth, mean_x_gini, sample_size, start_year, end_year, culture, publication_ref, publication_doi, contributor_name, contributor_institution, contact_email, comments, data_type, date_added, plot_file_path, data_file_path, files_uploaded_date, gini_subadult, mean_x_gini_subadult, mean_wealth_subadult, median_wealth_subadult, adult_count, subadult_count, num_sites, area_or_distance, area_unit, bias_iqr_low, bias_iqr_high) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", 
          params = list(clickPosition$lat, clickPosition$lng, giniCoeff(), meanWealth(), medianWealthVal, minWealthVal, maxWealthVal, gini_x_mean(), sampleSize(), startYear(), endYear(), cultureInput, input$publicationRef, input$publicationDOI, input$contributorName, input$contributorInstitution, input$contactEmail, input$submissionComments, input$dataType, currentDate, plot_path_db, data_path_db, files_date_db, gini_subadult_val, mean_x_gini_subadult_val, mean_wealth_subadult_val, median_wealth_subadult_val, adult_count_val, subadult_count_val, sites_info$num_sites, sites_info$area_or_distance, sites_info$area_unit, bias_iqr_low, bias_iqr_high))
formSubmitted(TRUE)
#          #update form submission state (so the map can be clicked again with a new form appearing):


# After successful database insert, create actual files (for Graves)
if (!is.null(plot_path) && plot_path != "") {
  safe_log("=== ATTEMPTING TO SAVE PLOTS ===")
  safe_log(paste("plot_path:", plot_path))
  safe_log(paste("app_root:", app_root))
  
  # Gather actual plot objects from stored reactive values
  plot_data <- list(
    prestige_plot = storedPrestigePlot(),
    gini_lorenz_plot = storedGiniLorenzPlot(), #also stores the decile plot all in one combined plot
    #decile_plot = storedDecilePlot(),
    bias_plot = storedBiasPlot()
  )
  
    # Build sample size text with age breakdown
  sample_size_text <- if (!is.null(adultGraveCount()) && !is.null(subadultGraveCount())) {
    paste0(sampleSize(), " total (", adultGraveCount(), " adult graves, ", subadultGraveCount(), " subadult-only graves)")
  } else {
    as.character(sampleSize())
  }

  # Create grave metadata object
  metadata <- list(
    culture = cultureInput,
    date_range = format_dates(startYear(), endYear()),
    sample_size = sampleSize(),
    publication_ref = input$publicationRef,
    publication_doi = input$publicationDOI,
    contributor_name = input$contributorName,
    contributor_institution = input$contributorInstitution,
    comments = input$submissionComments,
    date_added = currentDate,
    method = input$calcMethod,
    data_type = input$dataType,
    data_file_path = data_path_db,  # Add data file path for linking
    gini_subadult = if (!is.null(giniCoeffSubadult())) giniCoeffSubadult() else NULL,
    mean_x_gini_subadult = if (!is.null(giniCoeffSubadult()) && !is.null(meanWealthSubadult())) round(giniCoeffSubadult() * meanWealthSubadult(), 2) else NULL
  )
  
  # Debug: log metadata values
  safe_log(paste("DEBUG metadata - culture:", metadata$culture))
  safe_log(paste("DEBUG metadata - publication_ref:", metadata$publication_ref))
  safe_log(paste("DEBUG metadata - publication_doi:", metadata$publication_doi))
  safe_log(paste("DEBUG metadata - method:", metadata$method))
  # Save plots as HTML (include prestige table)
  success <- save_plots_as_html(plot_path, plot_data, metadata, df_with_prestige())
  if (success) {
    safe_log(paste("✅ Successfully saved grave plots to:", plot_path))
  } else {
    safe_log(paste("❌ Failed to save grave plots to:", plot_path))
  }
}

if (!is.null(data_path) && data_path != "") {
  # Create grave metadata for CSV
  metadata <- list(
    culture = cultureInput,
    date_range = format_dates(startYear(), endYear()),
    sample_size = sampleSize(),
    publication_ref = input$publicationRef,
    publication_doi = input$publicationDOI,
    contributor_name = input$contributorName,
    contributor_institution = input$contributorInstitution,
    comments = input$submissionComments,
    date_added = currentDate,
    method = input$calcMethod, 
    data_type = input$dataType
  )
  
  # Save input CSV file
  success <- save_input_csv(data_path, input$datafile, metadata)
  if (success) {
    safe_log(paste("✅ Successfully saved grave data to:", data_path))
  } else {
    safe_log(paste("❌ Failed to save grave data to:", data_path))
  }
}

        # Define color based on Gini value
        giniColor <- scales::col_numeric(palette = "Greens", domain = c(0, 20))(gini_x_mean())
        
        # Build sample size text
        sample_text <- if (!is.null(adultGraveCount()) && !is.null(subadultGraveCount())) {
          paste0(adultGraveCount(), " adults, ", subadultGraveCount(), " subadults")
        } else {
          as.character(sampleSize())
        }
        
        # Build subadult info
        subadult_popup <- ""
        if (!is.null(giniCoeffSubadult())) {
          subadult_popup <- paste0(
            "<br><b style='color:orange;'>Subadult Gini:</b> ", giniCoeffSubadult(),
            "<br><b style='color:orange;'>Subadult Abs Gini:</b> ", round(giniCoeffSubadult() * meanWealthSubadult(), 2)
          )
        }
        
        # Build publication link
        pub_link <- format_publication_link(input$publicationRef, input$publicationDOI, for_html = TRUE)
        
        # Build sites/area display string for the map popup
        sites_display <- if (sites_info$num_sites == 1) {
          "1 site"
        } else if (sites_info$num_sites == 2) {
          paste0(sites_info$num_sites, " sites (", sites_info$area_or_distance, " ", sites_info$area_unit, " apart)")
        } else {
          paste0(sites_info$num_sites, " sites (", sites_info$area_or_distance, " ", sites_info$area_unit, ")")
        }

        # Build IQR string if available
        iqr_popup <- ""
        if (!is.null(storedBiasIQR())) {
          iqr_vals <- storedBiasIQR()
          iqr_popup <- paste0("<br><b>Category Bias IQR:</b> [", round(iqr_vals[1], 3), " - ", round(iqr_vals[2], 3), "]")
        }

        popup_content <- paste0("<b>Culture:</b> ", culture(),
                              "<br><b>Prestige+√count Gini (relative):</b> ", giniCoeff(),
                              "<br><b>Mean/Median Wealth:</b> ", round(meanWealth(), 2), "/", round(medianWealthVal, 2),
                              "<br><b>Absolute Gini:</b> ", round(gini_x_mean(), 2),
                              iqr_popup,
                              "<br><b>Sample Size:</b> ", sample_text,
                              "<br><b>Sites:</b> ", sites_display,
                              subadult_popup,
                              "<br><b>Data type:</b> ", input$dataType,
                              "<br><b>Date range:</b> ", format_dates(startYear(), endYear()),
                              "<br><b>Contributor:</b> ", contributorNameInput,
                              if (pub_link != "") paste0("<br><b>Publication:</b> ", pub_link) else "",
                              if (!is.null(input$submissionComments) && input$submissionComments != "") 
                                paste0("<br><b>Comments:</b> ", input$submissionComments) else "")
        
        # Add plot link if it was saved
        #if (!is.null(plot_path_db) && plot_path_db != "") {
          #plot_link <- paste0("/", plot_path_db)
        if (!is.null(plot_path_db) && plot_path_db != "") {
          plot_link <- build_file_url(plot_path_db)  # addResourcePath handles the routing
          popup_content <- paste0(popup_content,
            "<br>📊 <a href='", plot_link, "' target='_blank'><b>View Analysis Plots</b></a>")
        }
        
        # Add data link if it was saved
        if (!is.null(data_path_db) && data_path_db != "") {
          #data_link <- paste0("/", data_path_db)  # addResourcePath handles the routing
          data_link <- build_file_url(data_path_db)
          popup_content <- paste0(popup_content,
            "<br>📥 <a href='", data_link, "' download target='_blank'><b>Download Data</b></a>")
        }
        
        #Update the map with a heatmap-like dot
        leafletProxy("mymap") %>%
          addCircleMarkers(lng = clickPosition$lng, lat = clickPosition$lat,
                           radius = sqrt(sampleSize()), color = giniColor,
                           fillColor = giniColor, fillOpacity = 0.8,
                           group = "graveResults",
                           popup = popup_content)
        
        showNotification("Grave results successfully added to map!", type = "message")
        
      }, error = function(e) {
        safe_log(paste("Grave submission error:", e$message))
        showNotification(paste("Error adding grave results to map:", e$message), type = "error")
      })
    })
    
    # House data submission
    observeEvent(input$submitHouseDataBtn, {
      removeModal()
      
      tryCatch({
        # Validate all required data exists
        if (is.null(houseGiniCoeff()) || is.null(meanHouseSize()) || is.null(house_gini_x_mean()) || is.null(houseSampleSize())) {
          showNotification("Missing calculation data. Please calculate Gini first.", type = "error")
          return()
        }
        
        if (is.null(houseClickPosition$lat) || is.null(houseClickPosition$lng)) {
          showNotification("Missing location data. Please click on the map to select a location.", type = "error")
          return()
        }
        
        # Calculate number of sites and area from uploaded data
        sites_info <- list(num_sites = 1, area_or_distance = NA, area_unit = NA)
        if (!is.null(input$houseDatafile)) {
          tryCatch({
            df <- read_data_file(input$houseDatafile)
            sites_info <- calculate_sites_and_area(df)
          }, error = function(e) {
            safe_log(paste("Error calculating sites/area:", e$message))
          })
        }

        # Extract form inputs using house-specific fields
        houseCultureInput <- houseCulture()
        currentDate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "CET")
        contributorNameInput <- input$contributorName
        
        if (is.null(houseCultureInput) || houseCultureInput == "") {
          showNotification("Culture/Period cannot be empty.", type = "error")
          return()
        }
        
        # Write house data to the database with error handling
        # Initialize file paths
plot_path <- NULL
data_path <- NULL
files_date <- NULL

# Safe logical checks for house checkboxes
submit_plots <- isTRUE(input$submitHousePlots)
submit_data <- isTRUE(input$submitHouseData)  
house_ready <- isTRUE(houseFilesReady())

safe_log(paste("House checkbox states - plots:", submit_plots, "data:", submit_data, "house_ready:", house_ready))

# Save files if checkboxes were checked
if (input$submitHousePlots && houseFilesReady()) {
  # Generate file paths
  contributor_name <- if(is.null(input$contributorName) || input$contributorName == "") "unknown" else input$contributorName
  safe_name <- gsub("[^a-zA-Z0-9\\-\\.]", "_", contributor_name)  # Simple sanitization
  safe_culture <- gsub("[^a-zA-Z0-9\\-\\.]", "_", houseCultureInput)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  plot_filename <- paste0("house_", timestamp, "_", safe_culture, "_", safe_name, "_plots.html")
  plot_path <- file.path("saved_files", "houses", "plots", plot_filename)
  files_date <- currentDate
  safe_log(paste("Will save house plots to:", plot_path))
}

if (input$submitHouseData && !is.null(input$houseDatafile)) {
  contributor_name <- if(is.null(input$contributorName) || input$contributorName == "") "unknown" else input$contributorName
  safe_name <- gsub("[^a-zA-Z0-9\\-\\.]", "_", contributor_name)  # Simple sanitization
  safe_culture <- gsub("[^a-zA-Z0-9\\-\\.]", "_", houseCultureInput)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  data_filename <- paste0("house_", timestamp, "_", safe_culture, "_", safe_name, "_data.csv")
  data_path <- file.path("saved_files", "houses", "data", data_filename)
  if (is.null(files_date)) files_date <- currentDate
  safe_log(paste("Will save house data to:", data_path))
}

#Ensure all parameters have proper length and type for database
plot_path_db <- if(is.null(plot_path)) "" else as.character(plot_path)
data_path_db <- if(is.null(data_path)) "" else as.character(data_path)  
files_date_db <- if(is.null(files_date)) "" else as.character(files_date)

safe_log(paste("Final db params - plot_path:", plot_path_db, "data_path:", data_path_db, "files_date:", files_date_db))



medianHouseSizeVal <- median(house_data_processed()$Value)
minHouseSizeVal <- min(house_data_processed()$Value)
maxHouseSizeVal <- max(house_data_processed()$Value)

#House database write
safe_log(paste("House checkbox states - plots:", input$submitHousePlots, "data:", input$submitHouseData))
dbExecute(db, "INSERT INTO house_results (lat, lng, gini, mean_house_size, median_house_size, min_house_size, max_house_size, mean_x_gini, sample_size, start_year, end_year, culture, publication_ref, publication_doi, contributor_name, contributor_institution, contact_email, comments, data_type, date_added, plot_file_path, data_file_path, files_uploaded_date, num_sites, area_or_distance, area_unit) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", 
          params = list(houseClickPosition$lat, houseClickPosition$lng, houseGiniCoeff(), meanHouseSize(), medianHouseSizeVal, minHouseSizeVal, maxHouseSizeVal, house_gini_x_mean(), houseSampleSize(), houseStartYear(), houseEndYear(), houseCultureInput, input$publicationRef, input$publicationDOI, input$contributorName, input$contributorInstitution, input$contactEmail, input$submissionComments, input$dataType, currentDate, plot_path_db, data_path_db, files_date_db, sites_info$num_sites, sites_info$area_or_distance, sites_info$area_unit))        
houseFormSubmitted(TRUE)
        
#HOUSE SUBMISSION HANDLER
# After successful house database insert, create actual files  (for Houses)
if (!is.null(plot_path) && plot_path != "") {
  safe_log("=== ATTEMPTING TO SAVE PLOTS ===")
  safe_log(paste("plot_path:", plot_path))
  safe_log(paste("app_root:", app_root))
  
  # Gather actual house plot object
  plot_data <- list(
    house_gini_plot = storedHouseGiniPlot()
    #house_decile_plot = storedHouseDecilePlot()
  )
  
  # House metadata
  metadata <- list(
    culture = houseCultureInput,
    date_range = format_dates(houseStartYear(), houseEndYear()),
    sample_size = houseSampleSize(),
    publication_ref = input$publicationRef,
    publication_doi = input$publicationDOI,
    contributor_name = input$contributorName,
    contributor_institution = input$contributorInstitution,
    comments = input$submissionComments,
    date_added = currentDate,
    method = "Physical measure Gini",
    data_type = input$dataType,
    data_file_path = data_path_db  # Add data file path for linking
  )
  
  # Save house plots
  success <- save_plots_as_html(plot_path, plot_data, metadata)
  if (success) {
    safe_log(paste("✅ Successfully saved house plots to:", plot_path))
  }
}

if (!is.null(data_path) && data_path != "") {
  # House CSV metadata
  metadata <- list(
    culture = houseCultureInput,
    date_range = format_dates(houseStartYear(), houseEndYear()),
    sample_size = houseSampleSize(),
    publication_ref = input$publicationRef,
    publication_doi = input$publicationDOI,
    contributor_name = input$contributorName,
    contributor_institution = input$contributorInstitution,
    comments = input$submissionComments,
    date_added = currentDate,
    method = "Physical Measure Analysis", 
    data_type = input$dataType
  )
  
  # Save house CSV (note: use houseDatafile, not datafile!)
  success <- save_input_csv(data_path, input$houseDatafile, metadata)
  if (success) {
    safe_log(paste("✅ Successfully saved house data to:", data_path))
  }
}

        # Define color for house results (purple)
        houseColor <- scales::col_numeric(palette = "Purples", domain = c(0, 30))(house_gini_x_mean())
        
        # Build publication link (houses)
        pub_link <- format_publication_link(input$publicationRef, input$publicationDOI, for_html = TRUE)
        
        # Build sites/area display string
        sites_display <- if (sites_info$num_sites == 1) {
          "1 site"
        } else if (sites_info$num_sites == 2) {
          paste0(sites_info$num_sites, " sites (", sites_info$area_or_distance, " ", sites_info$area_unit, " apart)")
        } else {
          paste0(sites_info$num_sites, " sites (", sites_info$area_or_distance, " ", sites_info$area_unit, ")")
        }

        popup_content <- paste("<b>Culture:</b> ", houseCultureInput,
                              "<br><b>House Gini (relative):</b> ", houseGiniCoeff(),
                              "<br><b>Mean phys. measure:</b> ", round(meanHouseSize(), 2), "m²",
                              "<br><b>Absolute Gini:</b> ", round(house_gini_x_mean(), 2),
                              "<br><b>Sample Size:</b> ", houseSampleSize(),
                              "<br><b>Sites:</b> ", sites_display,
                              "<br><b>Data type:</b> ", input$dataType,
                              "<br><b>Date range:</b> ", format_dates(houseStartYear(), houseEndYear()),
                              "<br><b>Contributor:</b> ", contributorNameInput,
                              if (pub_link != "") paste0("<br><b>Publication:</b> ", pub_link) else "", 
                              if (!is.null(input$submissionComments) && input$submissionComments != "") 
                                paste0("<br><b>Comments:</b> ", input$submissionComments) else "")
        
        # Add plot link if it was saved
        if (!is.null(plot_path_db) && plot_path_db != "") {
          #plot_link <- paste0("/", plot_path_db)  # addResourcePath handles the routing
          plot_link <- build_file_url(plot_path_db)
          popup_content <- paste0(popup_content,
            "<br>📊 <a href='", plot_link, "' target='_blank'><b>View Analysis Plots</b></a>")
        }
        
        # Add data link if it was saved
        #if (!is.null(data_path_db) && data_path_db != "") {
          #data_link <- paste0("/", data_path_db)  # addResourcePath handles the routing
        if (!is.null(data_path_db) && data_path_db != "") {
          data_link <- build_file_url(data_path_db)
          popup_content <- paste0(popup_content,
            "<br>📥 <a href='", data_link, "' download target='_blank'><b>Download Data</b></a>")
        }
        
        # Update the map with house result
        leafletProxy("mymap") %>%
          addCircleMarkers(lng = houseClickPosition$lng, lat = houseClickPosition$lat,
                           radius = sqrt(houseSampleSize()), color = houseColor,
                           fillColor = houseColor, fillOpacity = 0.8,
                           group = "houseResults",
                           popup = popup_content)
        
        showNotification("Results successfully added to map!", type = "message")
        
      }, error = function(e) {
        safe_log(paste("House submission error:", e$message))
        showNotification(paste("Error adding house results to map:", e$message), type = "error")
      })
    })
    
    
    
    # Observer: When a file is uploaded, enable the "go" button and extract coordinates
    observe({
      if (!is.null(input$datafile)) {
        shinyjs::enable("go")
        
        # Try to extract coordinates and detect age columns
        tryCatch({
          df <- read_data_file(input$datafile)
          
          # Validate coordinates FIRST (before extracting)
          coord_validation <- validate_coordinates(df)
          if (!coord_validation$valid) {
            showNotification(coord_validation$error_message, type = "error", duration = 15)
            graveUploadCoords(NULL)
            updateCheckboxInput(session, "graveHasCoords", value = FALSE)
            # Don't return - still allow processing without coordinates
          } else {
            # Extract coordinates only if validation passed
            coords <- extract_coordinates(df)
            graveUploadCoords(coords)
            if (!is.null(coords)) {
              updateCheckboxInput(session, "graveHasCoords", value = TRUE)
              
              # Check for coordinate grouping
              sites_info <- calculate_sites_and_area(df)
              
              if (sites_info$coords_were_grouped) {
                showNotification(
                  paste0("Coordinates detected: ", sites_info$num_coords_raw, 
                         " unique coordinates grouped into ", sites_info$num_sites,
                         " site(s) (coordinates within ~1km treated as same site). ",
                         "Centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
                  type = "message", duration = 10
                )
              } else {
                showNotification(
                  paste0("Coordinates detected. ", sites_info$num_sites, " site(s). ",
                         "Centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
                  type = "message", duration = 5
                )
              }
            } else {
              updateCheckboxInput(session, "graveHasCoords", value = FALSE)
            }
          }
          
          # Auto-detect age columns (case-insensitive)
          col_names_lower <- tolower(names(df))
          has_adult <- "adult_count" %in% col_names_lower
          has_subadult <- "subadult_count" %in% col_names_lower
          # Always update checkboxes based on detection (check or uncheck)
          updateCheckboxInput(session, "hasAdultCount", value = has_adult)
          updateCheckboxInput(session, "hasSubadultCount", value = has_subadult)
          if (has_adult) {
            updateCheckboxInput(session, "hasAdultCount", value = TRUE)
          }
          if (has_subadult) {
            updateCheckboxInput(session, "hasSubadultCount", value = TRUE)
          }
          
          # Notify user about detected columns
          age_cols_found <- c()
          if (has_adult) age_cols_found <- c(age_cols_found, "adult_count")
          if (has_subadult) age_cols_found <- c(age_cols_found, "subadult_count")
          
          if (length(age_cols_found) > 0) {
            showNotification(
              paste("Detected age columns:", paste(age_cols_found, collapse = ", ")),
              type = "message", duration = 5
            )
          }
          
        }, error = function(e) {
          graveUploadCoords(NULL)
          showNotification(paste("Error reading file:", e$message), type = "error")
        })
      }
    })
    
    
    # Observer: When house file is uploaded, enable house buttons and extract coordinates
    observe({
      if (!is.null(input$houseDatafile)) {
        shinyjs::enable("calcHouseGini")
        houseDataUploaded(TRUE)
        
        # Try to extract coordinates from uploaded data
        tryCatch({
          df <- read_data_file(input$houseDatafile)
          
          # Validate coordinates FIRST
          coord_validation <- validate_coordinates(df)
          if (!coord_validation$valid) {
            showNotification(coord_validation$error_message, type = "error", duration = 15)
            houseUploadCoords(NULL)
            updateCheckboxInput(session, "houseHasCoords", value = FALSE)
          } else {
            coords <- extract_coordinates(df)
            houseUploadCoords(coords)
            
            # Auto-check the checkbox if coordinates found
            if (!is.null(coords)) {
              updateCheckboxInput(session, "houseHasCoords", value = TRUE)
              
              # Check for coordinate grouping
              sites_info <- calculate_sites_and_area(df)
              
              if (sites_info$coords_were_grouped) {
                showNotification(
                  paste0("Coordinates detected: ", sites_info$num_coords_raw, 
                         " unique coordinates grouped into ", sites_info$num_sites,
                         " site(s) (coordinates within ~1km treated as same site). ",
                         "Centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
                  type = "message", duration = 10
                )
              } else {
                showNotification(
                  paste0("Coordinates detected. ", sites_info$num_sites, " site(s). ",
                         "Centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
                  type = "message", duration = 5
                )
              }
            } else {
              updateCheckboxInput(session, "houseHasCoords", value = FALSE)
            }
          }
        }, error = function(e) {
          houseUploadCoords(NULL)
          showNotification(paste("Error reading file:", e$message), type = "error")
        })
      }
    })
    
    
    
    # Load preloaded map data from the DB when the app starts or when timeSlider changes.
    observe({
      timeSliderValues <- input$timeSlider
      loadDataAndPlot(timeSliderValues)
    })
    
    # Reactive expression for df_range from the input file
    df_range_reactive <- reactive({
      req(input$datafile)
      inFile <- input$datafile
      df <- read.csv(inFile$datapath, sep = ";")
      get_TOT_range(df)
    })
    
    # House data reactive
    house_data_reactive <- reactive({
      req(input$houseDatafile)
      inFile <- input$houseDatafile
      df <- read_data_file(inFile)
      
      # Validate data
      validation_error <- validate_house_data(df)
      if (!is.null(validation_error)) {
        showNotification(validation_error, type = "error")
        return(NULL)
      }
      
      # Clean and prepare data - use generic internal column names from previously defined reactive values 
      names(df)[1:2] <- c("Unit_ID", "Value")
      df$Value <- as.numeric(df$Value)
      df <- df[!is.na(df$Value), ]
      
      return(df)
    })
    
    #also compute the rawTOT for the density plot
    rawTOT_values <- reactive({
      req(input$datafile)
      TOTdf <- read_data_file(input$datafile)
      # Only count grave good columns (purely numeric, not metadata)
      gg_cols <- identify_grave_good_columns(TOTdf)
      if (length(gg_cols) == 0) return(rep(0, nrow(TOTdf)))
      TOTdf_bin <- as.data.frame(ifelse(is.na(TOTdf[, gg_cols, drop = FALSE]), 0, 1))
      rowSums(TOTdf_bin)  # return the raw TOT vector
    })
    
    culture <- reactive({ input$culture })
    startYear <- reactive({ input$startYear })
    endYear <- reactive({ input$endYear })
    
    # House-specific reactive values
    houseCulture <- reactive({ input$houseCulture })
    houseStartYear <- reactive({ input$houseStartYear })
    houseEndYear <- reactive({ input$houseEndYear })
    
    # Helper reactives for physical data type labels (ID)
    physicalIdLabel <- reactive({
      switch(input$physicalDataType,
        "house size (m2)" = "House ID",
        "storage area (m2)" = "Storage ID",
        "mound area (m2)" = "Mound ID",
        "mound volume (m3)" = "Mound ID",
        "grave pit depth (cm)" = "Grave ID",
        "Unit ID"
      )
    })
    # Helper reactives for physical data type labels (Value)
    physicalValueLabel <- reactive({
      switch(input$physicalDataType,
        "house size (m2)" = "Size (m²)",
        "storage area (m2)" = "Area (m²)",
        "mound area (m2)" = "Area (m²)",
        "mound volume (m3)" = "Volume (m³)",
        "grave pit depth (cm)" = "Depth (cm)",
        "Value"
      )
    })
    # Helper reactives for physical data type labels (unit)
    physicalUnit <- reactive({
      switch(input$physicalDataType,
        "house size (m2)" = "m²",
        "storage area (m2)" = "m²",
        "mound area (m2)" = "m²",
        "mound volume (m3)" = "m³",
        "grave pit depth (cm)" = "cm",
        ""
      )
    })



    # Reactive value for the processed table with prestige values
    df_with_prestige <- reactiveVal()
    
    # Age-separated analysis tracking
    hasAgeData <- reactiveVal(FALSE)
    adultGraveCount <- reactiveVal(NULL)
    subadultGraveCount <- reactiveVal(NULL)
    subadultExclusiveTypes <- reactiveVal(NULL)
    df_bin_age_info <- reactiveVal(NULL)

    # House-related reactive values
    house_data_processed <- reactiveVal()
    houseGiniCoeff <- reactiveVal()
    meanHouseSize <- reactive({
      if (is.null(house_data_processed())) return(NULL)
      mean(house_data_processed()$Value)
    })
    house_gini_x_mean <- reactive({ 
      if (is.null(houseGiniCoeff()) || is.null(meanHouseSize())) return(NULL)
      houseGiniCoeff() * meanHouseSize() 
    })
    houseSampleSize <- reactive({
      if (is.null(house_data_processed())) return(NULL)
      nrow(house_data_processed())
    })
    
    giniCoeff <- reactiveVal()
    giniCoeffSubadult <- reactiveVal()
    meanWealthSubadult <- reactiveVal()
    medianWealthSubadult <- reactiveVal()
    sampleSizeSubadult <- reactiveVal()

    #meanWealth <- reactive({
    #  if (is.null(df_with_prestige())) return(NULL)
    #  mean(df_with_prestige()$combined_total)
    #})
    meanWealth <- reactiveVal()

    #gini_x_mean <- reactive({ giniCoeff() * meanWealth() }) #same as Absolute gini
    gini_x_mean <- reactive({
      if (is.null(giniCoeff()) || is.null(meanWealth())) return(NULL)
      giniCoeff() * meanWealth()
    }) #same as Absolute gini
    
    sampleSize <- reactive({
      if (is.null(df_with_prestige())) return(NULL)
      nrow(df_with_prestige())
    })
    
    addResultsClicked <- reactiveVal(FALSE)
    formSubmitted <- reactiveVal(FALSE)
    clickPosition <- reactiveValues(lat = NULL, lng = NULL)

    # Track if we're verifying coordinates from uploaded data
    graveVerifyingCoords <- reactiveVal(FALSE)
    houseVerifyingCoords <- reactiveVal(FALSE)
    
    # Coordinates extracted from uploaded data
    graveUploadCoords <- reactiveVal(NULL)
    houseUploadCoords <- reactiveVal(NULL)

    # House-specific click handling
    addHouseResultsClicked <- reactiveVal(FALSE)
    houseFormSubmitted <- reactiveVal(FALSE)
    houseClickPosition <- reactiveValues(lat = NULL, lng = NULL)
    
    format_dates <- function(start_year, end_year) {
      if (start_year < 0) {
        start_date <- paste0(abs(start_year), " BCE")
      } else {
        start_date <- paste0(start_year, " CE")
      }
      if (end_year < 0) {
        end_date <- paste0(abs(end_year), " BCE")
      } else {
        end_date <- paste0(end_year, " CE")
      }
      if (start_year < 0 && end_year < 0) {
        date_range <- paste0(abs(start_year), " - ", abs(end_year), " BCE")
      } else if (start_year >= 0 && end_year >= 0) {
        date_range <- paste0(start_year, " - ", end_year, " CE")
      } else {
        date_range <- paste0(start_date, " - ", end_date)
      }
      return(date_range)
    }
    


# ============================================================================
# BAYESIAN PRESTIGE ESTIMATION
# - Method of moments prior for items with n ≥ 2
# - Weak prior for unique items (n = 1): α₀=0.01, β₀=0.001
# - Linear N-based CI adjustment ONLY for n = 1: 1/(1 + N*0.001)
# ============================================================================

bayesian_exclusivity_empirical <- function(df_range) {
  
  types <- unique(df_range$Type)
  types <- types[types != "TOT"]
  k <- length(types)
  
  # Get TOT distribution for all graves (deduplicated)
  all_graves <- df_range %>%
    dplyr::group_by(Grave_ID) %>%
    dplyr::summarise(TOT = dplyr::first(TOT), .groups = "drop")
  
  all_TOT <- all_graves$TOT
  N <- length(all_TOT)
  overall_mean <- mean(all_TOT)
  
  safe_log(paste("Total graves:", N, "| Types:", k))
  
  # Calculate estimates for each type
  bayes_values <- lapply(types, function(t) {
    
    y <- df_range$TOT[df_range$Type == t]
    y <- y[!is.na(y)]
    n <- length(y)
    
    if (n == 0) {
      return(data.frame(
        Type = t, bayes_est = NA, lowerCI = NA, upperCI = NA,
        prior_type = "no_data", item_n = 0, observed_mean = NA
      ))
    }
    
    observed_mean <- mean(y)
    sum_y <- sum(y)
    
    # Determine prior based on n
    if (n >= 2) {
      # Method of moments from item's own data
      M <- observed_mean
      V <- var(y)
      
      # If variance is 0 (all identical TOT), use small epsilon
      if (V == 0) {
        V <- 0.1
      }
      
      alpha0 <- M^2 / V
      beta0 <- M / V
      prior_type <- "local_empirical"
      
    } else {
      # n = 1: weak prior (minimally informative, estimate ≈ observed)
      alpha0 <- 0.01
      beta0 <- 0.001
      prior_type <- "weak_prior"
    }
    
    # Posterior
    alpha_post <- alpha0 + sum_y
    beta_post <- beta0 + n
    post_mean <- alpha_post / beta_post
    
    # Bayesian CI
    bayes_lower <- qgamma(0.025, shape = alpha_post, rate = beta_post)
    bayes_upper <- qgamma(0.975, shape = alpha_post, rate = beta_post)
    bayes_width <- bayes_upper - bayes_lower
    
    # Apply N-based CI adjustment ONLY for n = 1
    if (n == 1) {
      # Linear adjustment: 1/(1 + N*0.001)
      # N=1000 gives 50% narrowing, scales linearly
      ratio <- N * 0.001
      adjustment <- 1 / (1 + ratio)
      
      adj_width <- bayes_width * adjustment
      adj_lower <- post_mean - adj_width / 2
      adj_upper <- post_mean + adj_width / 2
      
      safe_log(paste("  ", t, ": n=1, N=", N, 
                    ", CI narrowing=", round((1-adjustment)*100, 1), "%", sep=""))
    } else {
      # n >= 2: use pure Bayesian CI (method of moments handles confidence)
      adj_lower <- bayes_lower
      adj_upper <- bayes_upper
    }
    
    data.frame(
      Type = t,
      bayes_est = round(post_mean, 2),
      lowerCI = round(adj_lower, 2),
      upperCI = round(adj_upper, 2),
      prior_type = prior_type,
      item_n = n,
      observed_mean = round(observed_mean, 2)
    )
  })
  
  result <- do.call(rbind, bayes_values)
  
  # Add metadata
  attr(result, "N") <- N
  attr(result, "k") <- k
  attr(result, "overall_mean") <- overall_mean
  
  # Log summary
  unique_count <- sum(result$item_n == 1, na.rm = TRUE)
  safe_log(paste("CI adjustment applied to", unique_count, "unique items (n=1)"))
  
  return(result)
}
    
    # Function to load data and plot on map (differential updates to avoid blinking)
    loadDataAndPlot <- function(timeSlider) {
      # Read data from both SQLite tables
      results <- dbReadTable(db, "results")
      
      # Try to read house results, create empty if table doesn't exist
      house_results <- tryCatch({
        dbReadTable(db, "house_results")
      }, error = function(e) {
        data.frame() # Empty dataframe if table doesn't exist
      })
      
      # Add placeholder columns for house_results (houses don't have category bias)
      if (nrow(house_results) > 0) {
        house_results$bias_iqr_low <- NA
        house_results$bias_iqr_high <- NA
      }

      # --- GRAVE RESULTS ---
      newGraveIds <- c()
      filteredResults <- data.frame()
      
      if (nrow(results) > 0 && any(colnames(results) %in% c("start_year", "end_year"))) {
        # Apply time filter for graves
        filteredResults <- results[!is.na(results$start_year) & !is.na(results$end_year) &
                                     ((results$start_year <= timeSlider[2] & results$start_year >= timeSlider[1]) |
                                        (results$end_year >= timeSlider[1] & results$end_year <= timeSlider[2]) |
                                        (results$start_year <= timeSlider[1] & results$end_year >= timeSlider[2])), ]
        
        if (nrow(filteredResults) > 0) {
          newGraveIds <- paste0("grave_", filteredResults$id)
        }
      }
      
      # Get currently visible grave IDs
      currentGraveIds <- visibleGraveIds()
      
      # Determine which to add and remove
      graveIdsToRemove <- setdiff(currentGraveIds, newGraveIds)
      graveIdsToAdd <- setdiff(newGraveIds, currentGraveIds)
      
      # Remove markers no longer in range
      if (length(graveIdsToRemove) > 0) {
        for (markerId in graveIdsToRemove) {
          leafletProxy("mymap") %>% removeMarker(layerId = markerId)
        }
      }
      
      # Add new markers that came into range
      if (length(graveIdsToAdd) > 0 && nrow(filteredResults) > 0) {
        
        # Fill NA data_type with default
        filteredResults$data_type[is.na(filteredResults$data_type)] <- "grave goods/features (counts)"
        
        # Get unique data types present
        grave_data_types <- unique(filteredResults$data_type)
        
        for (dt in grave_data_types) {
          # Filter to this data_type
          dt_results <- filteredResults[filteredResults$data_type == dt, ]
          
          if (nrow(dt_results) == 0) next
          
          # Calculate color range for this data_type only
          graveGiniRange <- range(dt_results$mean_x_gini, na.rm = TRUE)
          if (graveGiniRange[1] == graveGiniRange[2]) {
            graveGiniRange <- c(graveGiniRange[1] - 0.1, graveGiniRange[2] + 0.1)
          }
          graveColorScale <- scales::col_numeric(palette = c("#c7e9c0", "#74c476", "#238b45"), domain = graveGiniRange)
          
          for(i in 1:nrow(dt_results)) {
            markerId <- paste0("grave_", dt_results$id[i])
            if (markerId %in% graveIdsToAdd) {
              dateText <- format_dates(dt_results$start_year[i], dt_results$end_year[i])
              giniColor <- graveColorScale(dt_results$mean_x_gini[i])
              
              # Handle median_wealth (may be NULL for old entries)
              median_wealth_display <- if (!is.null(dt_results$median_wealth[i]) && !is.na(dt_results$median_wealth[i])) 
                                         round(dt_results$median_wealth[i], 2) else "N/A"
              
              # Handle sample size with age breakdown
              sample_text <- if (!is.na(dt_results$adult_count[i]) && !is.na(dt_results$subadult_count[i])) {
                paste0(dt_results$adult_count[i], " adults, ", dt_results$subadult_count[i], " subadults")
              } else {
                as.character(dt_results$sample_size[i])
              }
              
              # Handle subadult Gini
              subadult_popup <- ""
              if (!is.na(dt_results$gini_subadult[i]) && dt_results$gini_subadult[i] > 0) {
                subadult_popup <- paste0(
                  "<br><b style='color:orange;'>Subadult Gini:</b> ", dt_results$gini_subadult[i],
                  "<br><b style='color:orange;'>Subadult Abs Gini:</b> ", round(dt_results$mean_x_gini_subadult[i], 2)
                )
              }
              
              # Build publication link
              pub_link <- format_publication_link(dt_results$publication_ref[i], 
                                                  dt_results$publication_doi[i], 
                                                  for_html = TRUE)
              
              # Build sites/area display string (handle NAs for old data)
              sites_display <- if (is.na(dt_results$num_sites[i])) {
                "unknown"
              } else if (dt_results$num_sites[i] == 1) {
                "1 site"
              } else if (dt_results$num_sites[i] == 2) {
                paste0(dt_results$num_sites[i], " sites (", dt_results$area_or_distance[i], " ", dt_results$area_unit[i], " apart)")
              } else {
                paste0(dt_results$num_sites[i], " sites (", dt_results$area_or_distance[i], " ", dt_results$area_unit[i], ")")
              }

              # Handle bias IQR
              iqr_popup <- ""
              if (!is.na(dt_results$bias_iqr_low[i]) && !is.na(dt_results$bias_iqr_high[i])) {
                iqr_popup <- paste0("<br><b>Category Bias IQR:</b> [", 
                                   round(dt_results$bias_iqr_low[i], 3), " - ", 
                                   round(dt_results$bias_iqr_high[i], 3), "]")
              }
              popup_content <- paste0("<br><b>Culture:</b> ", dt_results$culture[i], 
                            "<br><b>Data type:</b> ", dt_results$data_type[i],
                            "<br><b>Prestige+√count Gini (relative):</b> ", dt_results$gini[i], 
                            "<br><b>Mean/Median wealth:</b> ", round(dt_results$mean_wealth[i], 2), "/", median_wealth_display,
                            "<br><b>Absolute Gini:</b> ", round(dt_results$mean_x_gini[i], 2),
                            iqr_popup,
                            "<br><b>Sample size:</b> ", sample_text,
                            "<br><b>Sites:</b> ", sites_display,
                            subadult_popup,
                            "<br><b>Date range:</b> ", dateText, 
                            "<br><b>Contributor:</b> ", dt_results$contributor_name[i],
                            if (pub_link != "") paste0("<br><b>Publication:</b> ", pub_link) else "",
                            if (!is.na(dt_results$comments[i]) && dt_results$comments[i] != "") 
                              paste0("<br><b>Comments:</b> ", dt_results$comments[i]) else "")
              
              # Add plot link if available
              if (!is.na(dt_results$plot_file_path[i]) && dt_results$plot_file_path[i] != "") {
                plot_link <- build_file_url(dt_results$plot_file_path[i])
                popup_content <- paste0(popup_content, 
                  "<br>📊 <a href='", plot_link, "' target='_blank'><b>View Analysis Plots</b></a>")
              }
              
              # Add data link if available
              if (!is.na(dt_results$data_file_path[i]) && dt_results$data_file_path[i] != "") {
                data_link <- build_file_url(dt_results$data_file_path[i])
                popup_content <- paste0(popup_content,
                  "<br>📥 <a href='", data_link, "' download target='_blank'><b>Download Data</b></a>")
              }
              
              leafletProxy("mymap") %>% addCircleMarkers(
                lng = dt_results$lng[i], lat = dt_results$lat[i],
                radius = sqrt(dt_results$sample_size[i]), color = giniColor,
                fillColor = giniColor, fillOpacity = 0.8,
                group = dt,  # Use data_type as group name
                layerId = markerId,
                popup = popup_content
              )
            }
          }
        }
      }
      
      # Update tracked grave IDs
      visibleGraveIds(newGraveIds)
      
      # --- HOUSE RESULTS ---
      newHouseIds <- c()
      filteredHouseResults <- data.frame()
      
      if (nrow(house_results) > 0 && any(colnames(house_results) %in% c("start_year", "end_year"))) {
        # Apply time filter for houses
        filteredHouseResults <- house_results[!is.na(house_results$start_year) & !is.na(house_results$end_year) &
                                                ((house_results$start_year <= timeSlider[2] & house_results$start_year >= timeSlider[1]) |
                                                   (house_results$end_year >= timeSlider[1] & house_results$end_year <= timeSlider[2]) |
                                                   (house_results$start_year <= timeSlider[1] & house_results$end_year >= timeSlider[2])), ]
        
        if (nrow(filteredHouseResults) > 0) {
          newHouseIds <- paste0("house_", filteredHouseResults$id)
        }
      }
      
      # Get currently visible house IDs
      currentHouseIds <- visibleHouseIds()
      
      # Determine which to add and remove
      houseIdsToRemove <- setdiff(currentHouseIds, newHouseIds)
      houseIdsToAdd <- setdiff(newHouseIds, currentHouseIds)
      
      # Remove markers no longer in range
      if (length(houseIdsToRemove) > 0) {
        for (markerId in houseIdsToRemove) {
          leafletProxy("mymap") %>% removeMarker(layerId = markerId)
        }
      }
      
      # Add new markers that came into range
      if (length(houseIdsToAdd) > 0 && nrow(filteredHouseResults) > 0) {
        
        # Fill NA data_type with default
        filteredHouseResults$data_type[is.na(filteredHouseResults$data_type)] <- "house size (m2)"
        
        # Get unique data types present
        house_data_types <- unique(filteredHouseResults$data_type)
        
        for (dt in house_data_types) {
          # Filter to this data_type
          dt_results <- filteredHouseResults[filteredHouseResults$data_type == dt, ]
          
          if (nrow(dt_results) == 0) next
          
          # Calculate color range for this data_type only
          houseGiniRange <- range(dt_results$mean_x_gini, na.rm = TRUE)
          if (houseGiniRange[1] == houseGiniRange[2]) {
            houseGiniRange <- c(houseGiniRange[1] - 0.1, houseGiniRange[2] + 0.1)
          }
          houseColorScale <- scales::col_numeric(palette = c("#dadaeb", "#9e9ac8", "#6a51a3"), domain = houseGiniRange)
          
          for(i in 1:nrow(dt_results)) {
            markerId <- paste0("house_", dt_results$id[i])
            if (markerId %in% houseIdsToAdd) {
              dateText <- format_dates(dt_results$start_year[i], dt_results$end_year[i])
              houseColor <- houseColorScale(dt_results$mean_x_gini[i])
              
              # Handle median_house_size (may be NULL for old entries)
              median_house_display <- if (!is.null(dt_results$median_house_size[i]) && !is.na(dt_results$median_house_size[i])) 
                                        round(dt_results$median_house_size[i], 2) else "N/A"
              
              # Build publication link
              pub_link <- format_publication_link(dt_results$publication_ref[i], 
                                                  dt_results$publication_doi[i], 
                                                  for_html = TRUE)
              
              # Build sites/area display string (handle NAs for old data)
              sites_display <- if (is.na(dt_results$num_sites[i])) {
                "unknown"
              } else if (dt_results$num_sites[i] == 1) {
                "1 site"
              } else if (dt_results$num_sites[i] == 2) {
                paste0(dt_results$num_sites[i], " sites (", dt_results$area_or_distance[i], " ", dt_results$area_unit[i], " apart)")
              } else {
                paste0(dt_results$num_sites[i], " sites (", dt_results$area_or_distance[i], " ", dt_results$area_unit[i], ")")
              }

              popup_content <- paste("<br><b>Culture/Period:</b> ", dt_results$culture[i], 
                            "<br><b>Data type:</b> ", dt_results$data_type[i],
                            "<br><b>Gini (relative):</b> ", dt_results$gini[i], 
                            "<br><b>Mean/Median:</b> ", round(dt_results$mean_house_size[i], 2), "/", median_house_display,
                            "<br><b>Absolute Gini:</b> ", round(dt_results$mean_x_gini[i], 2),
                            "<br><b>Sample size:</b> ", dt_results$sample_size[i],
                            "<br><b>Sites:</b> ", sites_display,
                            "<br><b>Date range:</b> ", dateText, 
                            "<br><b>Contributor:</b> ", dt_results$contributor_name[i],
                            if (pub_link != "") paste0("<br><b>Publication:</b> ", pub_link) else "",
                            if (!is.na(dt_results$comments[i]) && dt_results$comments[i] != "") 
                              paste0("<br><b>Comments:</b> ", dt_results$comments[i]) else "")
              
              # Add plot link if available
              if (!is.na(dt_results$plot_file_path[i]) && dt_results$plot_file_path[i] != "") {
                plot_link <- build_file_url(dt_results$plot_file_path[i])
                popup_content <- paste0(popup_content,
                  "<br>📊 <a href='", plot_link, "' target='_blank'><b>View Analysis Plots</b></a>")
              }
              
              # Add data link if available
              if (!is.na(dt_results$data_file_path[i]) && dt_results$data_file_path[i] != "") {
                data_link <- build_file_url(dt_results$data_file_path[i])
                popup_content <- paste0(popup_content,
                  "<br>📥 <a href='", data_link, "' download target='_blank'><b>Download Data</b></a>")
              }
              
              leafletProxy("mymap") %>% addCircleMarkers(
                lng = dt_results$lng[i], lat = dt_results$lat[i],
                radius = sqrt(dt_results$sample_size[i]), color = houseColor,
                fillColor = houseColor, fillOpacity = 0.8,
                group = dt,  # Use data_type as group name
                layerId = markerId,
                popup = popup_content
              )
            }
          }
        }
      }
      
      # Update tracked house IDs
      visibleHouseIds(newHouseIds)
    }
    
    # Initial map
    output$mymap <- renderLeaflet({
      leaflet() %>% addTiles()
    })
    

    #### Map Drawing Handling ####
    observeEvent(input$mymap_draw_new_feature, {
      leafletProxy("mymap") %>% clearGroup("drawnItems")
      features <- input$mymap_draw_new_feature
      if (features$geometry$type %in% c("Polygon", "Rectangle")) {
        drawnBounds(features$geometry$coordinates[[1]])
      }
    })
    
    observeEvent(input$resetDrawing, {
      leafletProxy("mymap") %>% clearGroup("drawnItems")
      drawnBounds(NULL)
    })
    
    
    # helper to insert an interactive ggplotly chart into your dynamic UI
    addInteractivePlot <- function(plotObj, height = "400px", width = "100%") {
    # generate a unique output ID
    tms <- gsub("\\D", "", format(Sys.time(), "%Y%m%d%H%M%OS3")) #time stamp
    newId <- paste0("interactivePlot", tms)
  # define it with renderPlotly
    output[[newId]] <- renderPlotly({
    # if it's a ggplot, wrap in ggplotly; otherwise assume it's already a plotly
    if (inherits(plotObj, "ggplot")) {
      ggplotly(plotObj)
    } else {
      plotObj
    }
    })
  # push a plotlyOutput into your UI stack with scrollable container if needed
    updatePlotUI(
    div(
      style = "overflow-x: auto;",  # horizontal scroll if needed
      plotlyOutput(newId, height = height, width = width)
    )
    )
  }


  
    
    
    #### Calculate Prestige Values ####
    observeEvent(input$go, {
      req(input$datafile)
      safe_log("=== Starting prestige calculation ===")

      withProgress(message = "Calculating Prestige Values…", value = 0, { #Progress Bar, ends after this line: output$debug <- renderPrint({ paste("An error occurred: ", e$message) })}) 
      incProgress(0.05, detail = "Reading data file")
      inFile <- input$datafile
      df <- read_data_file(inFile)
      safe_log(paste("File read, rows:", nrow(df), "cols:", ncol(df)))
      
      # Check if using age-separated analysis
      col_names_lower <- tolower(names(df))
      has_adult_col <- input$hasAdultCount && "adult_count" %in% col_names_lower
      has_subadult_col <- input$hasSubadultCount && "subadult_count" %in% col_names_lower
      use_age_separation <- has_adult_col || has_subadult_col
      
      hasAgeData(use_age_separation)
      
      # Prepare data for calculations
      # Identify grave good columns early (purely numeric only)
      grave_good_cols <- identify_grave_good_columns(df)
      safe_log(paste("grave_good_cols after filtering:", length(grave_good_cols), "-", paste(grave_good_cols, collapse=", ")))

      # Check for columns with actual data
      cols_with_data <- sapply(grave_good_cols, function(col) {
      any(df[[col]] > 0, na.rm = TRUE)
      })
      
      grave_good_cols <- grave_good_cols[cols_with_data]

      if (length(grave_good_cols) == 0) {
        showNotification("No grave good columns contain data.", type = "error")
      return()
      }

      if (length(grave_good_cols) == 1) {
        showNotification(
        paste0("Only one type column ('", grave_good_cols[1], "') contains data. ",
               "All prestige values will be 1."),
        type = "warning", duration = 10
        )
      }
      safe_log(paste("grave_good_cols after filtering:", length(grave_good_cols), "-", paste(grave_good_cols, collapse=", ")))
      safe_log(paste("use_age_separation:", use_age_separation))
      if (use_age_separation) {
        age_result <- get_TOT_range_by_age(df)
        df_for_calc <- df[, c(names(df)[1], grave_good_cols), drop = FALSE]  # Grave goods without age columns
        df_bin_age_info(age_result)
        subadultExclusiveTypes(age_result$subadult_exclusive_types)
        adultGraveCount(age_result$adult_grave_count)
        subadultGraveCount(age_result$subadult_grave_count)
        
        safe_log(paste("Age separation enabled. Adult graves:", age_result$adult_grave_count, 
                       "Subadult-only graves:", age_result$subadult_grave_count,
                       "Subadult-exclusive types:", length(age_result$subadult_exclusive_types)))
        if (length(age_result$subadult_exclusive_types) > 0) {
          safe_log(paste("  Types:", paste(age_result$subadult_exclusive_types, collapse = ", ")))
        }
      } else {
        safe_log("Creating df_for_calc (no age separation)")
        # Remove any age columns and coordinate columns if present
        df_for_calc <- df[, c(names(df)[1], grave_good_cols), drop = FALSE]
        safe_log(paste("df_for_calc created, ncol:", ncol(df_for_calc)))
        df_bin_age_info(NULL)
        subadultExclusiveTypes(NULL)
        adultGraveCount(NULL)
        subadultGraveCount(NULL)
      }
      

      # --- Compute sqrt‐transformed original frequencies ----
      # Assumes first column is Grave_ID, the rest are counts/frequencies
      safe_log("About to compute sqrt_tbl")
      sqrt_tbl <- df_for_calc %>%
        #dplyr::mutate(across(-1, ~ round(asinh(.),2)))   # apply asinh() to every column except the ID
        #dplyr::mutate(across(-1, ~ round(sqrt(.),2)))    # apply sqrt() to every column except the ID (1st column)
        dplyr::mutate(
          across(                                                     # all columns …
            -1,                                                       # … except the first column
            ~ ifelse(!is.na(.x) & .x >= 1,                            # only where value ≥ 1
                     round(sqrt(.x) - 1, 2),                          #   apply √x − 1
                     .x)                                              # otherwise leave unchanged
          )
        )
      safe_log("sqrt_tbl created")  
        
      sqrt_tbl$sqrt_sum <- rowSums(sqrt_tbl[ , -1, drop = FALSE], na.rm = TRUE)  # sqrt counts RowSums column
      safe_log("sqrt_sum calculated")
      incProgress(0.15, detail = "Computing TOT ranges")

      output$sqrtTable <- renderDT({
        # Merge back all original columns, preserving original order
        sqrt_tbl_pretty <- df
        for (col in grave_good_cols) {
          sqrt_tbl_pretty[[col]] <- sqrt_tbl[[col]]
        }
        sqrt_tbl_pretty$`Total √-count bonus-1` <- sqrt_tbl$sqrt_sum
        
        if (use_age_separation) {
          adult_col <- names(df)[tolower(names(df)) == "adult_count"]
          adult_counts_display <- as.numeric(df[[adult_col[1]]])
          adult_counts_display[is.na(adult_counts_display) | adult_counts_display < 1] <- 1
          sqrt_tbl_pretty$`÷ adult_count` <- ifelse(
            adult_counts_display > 1,
            paste0("÷ ", adult_counts_display),
            ""
          )
        }

        datatable(
          sqrt_tbl_pretty,
          options = list(pageLength = 10),
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: left;',
            'squareroot‑transformed counts (only counts above 1 included here, added bonus to the green density curve and Gini, and in the mean wealth for the Absolute Gini)'
          )
        )
      })
      
      # keep the vector for later calculations
      sqrt_row_sums <- sqrt_tbl$sqrt_sum          # <‑‑ numeric vector
      
      # Compute the TOT table from the current file
      if (use_age_separation) {
        # Use adult TOT range for main prestige calculation
        df_range <- age_result$adult_TOT_range
        subadult_df_range <- age_result$subadult_TOT_range
      } else {
        df_range <- get_TOT_range(df_for_calc)
        subadult_df_range <- NULL
      }
      
      # Handle single-column case: all TOT values are 1
      if (length(grave_good_cols) == 1 && (is.null(df_range) || nrow(df_range) == 0)) {
      # Create minimal df_range manually
      graves_with_data <- df_for_calc[[1]][df_for_calc[[grave_good_cols[1]]] > 0 & !is.na(df_for_calc[[grave_good_cols[1]]])]
      df_range <- data.frame(
        Grave_ID = graves_with_data,
        TOT = rep(1, length(graves_with_data)),
        Type = rep(grave_good_cols[1], length(graves_with_data))
      )
      }
      safe_log(paste("df_range created, rows:", if(is.null(df_range)) "NULL" else nrow(df_range)))
      safe_log(paste("df_range types:", if(is.null(df_range)) "NULL" else paste(unique(df_range$Type), collapse=", ")))

      incProgress(0.25, detail = "Preparing prestige calculation")
      # (Optional) capture static snapshots of reactives:
      local_df_range <- isolate(df_range)
      # Note: TOT_plot is computed later so do not snapshot it here.
      # For Bayesian methods, we'll snapshot bayes_values after computing them.
      
      tryCatch({
        incProgress(0.30, detail = paste("Computing prestige values (", input$calcMethod, ")"))
        if (input$calcMethod == "mean") {
          means_GG <- get_TOT_means(df_range)
          
          # If age separation and subadult-exclusive types exist
          if (use_age_separation && !is.null(subadult_df_range) && nrow(subadult_df_range) > 0) {
            subadult_means <- get_TOT_means(subadult_df_range)
            # DON'T add "*" here - keep original names for column matching
            means_GG <- rbind(means_GG, subadult_means)
            safe_log(paste("Added", nrow(subadult_means), "subadult-exclusive types:",
                          paste(subadult_means$Type, collapse = ", ")))
          }
          
          processed_data <- apply_prestige_presence(means_GG, df_for_calc, "mean")
          processed_data$combined_total <- processed_data$prestige_total + sqrt_row_sums
          
          # Apply per-capita division if age data present
          if (use_age_separation) {
            age_info <- df_bin_age_info()
            adult_counts <- age_info$df_bin$adult_count
            subadult_counts <- age_info$df_bin$subadult_count
            is_adult_grave <- age_info$df_bin$is_adult_grave
            is_subadult_only <- age_info$df_bin$is_subadult_only
            
            # Adult graves: (combined_total + subadult_count) / adult_count
            # Subadult-only graves: combined_total / subadult_count
            #adjusted_total <- processed_data$combined_total
            #adjusted_total[is_adult_grave] <- (processed_data$combined_total[is_adult_grave] + subadult_counts[is_adult_grave]) / pmax(adult_counts[is_adult_grave], 1)
            #adjusted_total[is_subadult_only] <- processed_data$combined_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
            adjusted_total <- processed_data$combined_total
            adjusted_total[is_adult_grave] <- processed_data$combined_total[is_adult_grave] / pmax(adult_counts[is_adult_grave], 1)
            adjusted_total[is_subadult_only] <- processed_data$combined_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
            processed_data$combined_total <- adjusted_total
            
            adjusted_prestige <- processed_data$prestige_total
            adjusted_prestige[is_adult_grave] <- processed_data$prestige_total[is_adult_grave] / pmax(adult_counts[is_adult_grave], 1)
            adjusted_prestige[is_subadult_only] <- processed_data$prestige_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
            processed_data$prestige_total <- adjusted_prestige

            #processed_data$combined_total <- adjusted_total
            processed_data$is_adult_grave <- is_adult_grave
            processed_data$is_subadult_only <- is_subadult_only
            processed_data$adult_count <- adult_counts
            processed_data$subadult_count <- subadult_counts
          }
          
          incProgress(0.50, detail = "Building data tables")

#        } else if (input$calcMethod == "median") {
#          medians_GG <- get_TOT_medians(df_range)
#          processed_data <- apply_prestige_presence(medians_GG, df, "median")
#          processed_data$combined_total <- processed_data$prestige_total + sqrt_row_sums
#          incProgress(0.50, detail = "Building data tables")
#        } else if (input$calcMethod == "bayesian_default") {
#          # Default Bayesian using fixed prior: alpha0 = 0.1, beta0 = 0.1.
#          bayes_values <- data.frame(
#            Type = unique(df_range$Type),
#            bayes_est = sapply(unique(df_range$Type), function(type) {
#              y <- df_range$TOT[df_range$Type == type]
#              bayesian_exclusivity(y)
#            }),
#            lowerCI = sapply(unique(df_range$Type), function(type) {
#              y <- df_range$TOT[df_range$Type == type]
#              n <- length(y[!is.na(y)])
#              if(n == 0) return(NA) else {
#                shape <- 0.1 + sum(y)
#                rate <- 0.1 + n
#                qgamma(0.025, shape = shape, rate = rate)
#              }
#            }),
#            upperCI = sapply(unique(df_range$Type), function(type) {
#              y <- df_range$TOT[df_range$Type == type]
#              n <- length(y[!is.na(y)])
#              if(n == 0) return(NA) else {
#                shape <- 0.1 + sum(y)
#                rate <- 0.1 + n
#                qgamma(0.975, shape = shape, rate = rate)
#              }
#            })
#          )
#          processed_data <- apply_bayesian_presence(bayes_values, df)
#          processed_data$combined_total <- processed_data$prestige_total + sqrt_row_sums
#          incProgress(0.50, detail = "Building data tables")

        } else if (input$calcMethod == "bayesian_empirical") {
          safe_log(paste("Starting Bayesian empirical. df_range rows:", nrow(df_range)))
          
          # Calculate prestige from adult graves
          bayes_values <- bayesian_exclusivity_empirical(df_range)
          safe_log(paste("Adult bayes_values rows:", nrow(bayes_values)))
          
          # If age separation and subadult-exclusive types exist, calculate those separately
          if (use_age_separation && !is.null(subadult_df_range) && nrow(subadult_df_range) > 0) {
            safe_log(paste("Calculating Bayesian for subadult-exclusive types. Rows:", nrow(subadult_df_range)))
            safe_log(paste("Subadult df_range types:", paste(unique(subadult_df_range$Type), collapse = ", ")))
            
            subadult_bayes <- tryCatch({
              bayesian_exclusivity_empirical(subadult_df_range)
            }, error = function(e) {
              safe_log(paste("Error in subadult Bayesian calculation:", e$message))
              NULL
            })
            
            if (!is.null(subadult_bayes) && nrow(subadult_bayes) > 0) {
              safe_log(paste("Subadult bayes rows:", nrow(subadult_bayes)))
              safe_log(paste("Subadult bayes types:", paste(subadult_bayes$Type, collapse = ", ")))
              bayes_values <- rbind(bayes_values, subadult_bayes)
              safe_log(paste("Combined bayes_values rows:", nrow(bayes_values)))
            } else {
              safe_log("Subadult Bayesian calculation returned NULL or 0 rows")
            }
          }
          
          safe_log(paste("Applying bayesian presence. df_for_calc rows:", nrow(df_for_calc), "cols:", ncol(df_for_calc)))
          safe_log(paste("bayes_values types:", paste(bayes_values$Type, collapse = ", ")))
          safe_log(paste("df_for_calc columns:", paste(names(df_for_calc), collapse = ", ")))
          
          processed_data <- tryCatch({
            apply_bayesian_presence(bayes_values, df_for_calc)
          }, error = function(e) {
            safe_log(paste("Error in apply_bayesian_presence:", e$message))
            stop(e)
          })
          
          processed_data$combined_total <- processed_data$prestige_total + sqrt_row_sums
          safe_log(paste("processed_data rows:", nrow(processed_data)))
          
          # Apply per-capita division if age data present
          if (use_age_separation) {
            age_info <- df_bin_age_info()
            safe_log(paste("age_info$df_bin rows:", nrow(age_info$df_bin)))
            
            adult_counts <- age_info$df_bin$adult_count
            subadult_counts <- age_info$df_bin$subadult_count
            is_adult_grave <- age_info$df_bin$is_adult_grave
            is_subadult_only <- age_info$df_bin$is_subadult_only
            
            safe_log(paste("Vector lengths - adult_counts:", length(adult_counts), 
                          "is_adult_grave:", length(is_adult_grave),
                          "processed_data rows:", nrow(processed_data)))
            safe_log(paste("Adult graves:", sum(is_adult_grave), "Subadult-only:", sum(is_subadult_only)))
            
            # Adult graves: (combined_total + subadult_count) / adult_count
            # Subadult-only graves: combined_total / subadult_count
            adjusted_total <- processed_data$combined_total
            
            tryCatch({
              #adjusted_total[is_adult_grave] <- (processed_data$combined_total[is_adult_grave] + subadult_counts[is_adult_grave]) / pmax(adult_counts[is_adult_grave], 1)
              #adjusted_total[is_subadult_only] <- processed_data$combined_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
              adjusted_total[is_adult_grave] <- processed_data$combined_total[is_adult_grave] / pmax(adult_counts[is_adult_grave], 1)
              adjusted_total[is_subadult_only] <- processed_data$combined_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
              processed_data$combined_total <- adjusted_total
              
              adjusted_prestige <- processed_data$prestige_total
              adjusted_prestige[is_adult_grave] <- processed_data$prestige_total[is_adult_grave] / pmax(adult_counts[is_adult_grave], 1)
              adjusted_prestige[is_subadult_only] <- processed_data$prestige_total[is_subadult_only] / pmax(subadult_counts[is_subadult_only], 1)
              processed_data$prestige_total <- adjusted_prestige
              #processed_data$combined_total <- adjusted_total
              processed_data$is_adult_grave <- is_adult_grave
              processed_data$is_subadult_only <- is_subadult_only
              processed_data$adult_count <- adult_counts
              processed_data$subadult_count <- subadult_counts
              
              safe_log("Per-capita division completed successfully")
            }, error = function(e) {
              safe_log(paste("Error in per-capita division:", e$message))
              stop(e)
            })
          }
          
          incProgress(0.50, detail = "Building data tables")
        }
        # Merge back all original columns, preserving original order
        # Start with original df structure, then replace grave good cols with prestige values
        final_data <- df
        for (col in grave_good_cols) {
          final_data[[col]] <- processed_data[[col]]
        }
        # Calculate TOT (count of grave good types present)
        tot_values <- rowSums(!is.na(df[, grave_good_cols, drop = FALSE]) & df[, grave_good_cols, drop = FALSE] > 0)
        # Add the result columns at the end
        final_data$raw_TOT <- tot_values
        final_data$prestige_total <- processed_data$prestige_total
        final_data$combined_total <- processed_data$combined_total
        # Keep internal tracking columns if they exist (but we'll hide them in display)
        if ("is_adult_grave" %in% names(processed_data)) {
          final_data$is_adult_grave <- processed_data$is_adult_grave
          final_data$is_subadult_only <- processed_data$is_subadult_only
        }
        processed_data <- final_data

        if ("subadult_count" %in% tolower(names(df))) {
          subadult_col <- names(df)[tolower(names(df)) == "subadult_count"]
          processed_data[[subadult_col]] <- df[[subadult_col]]
        }
        if ("lat" %in% tolower(names(df))) {
          lat_col <- names(df)[tolower(names(df)) == "lat"]
          processed_data[[lat_col]] <- df[[lat_col]]
        }
        if ("lng" %in% tolower(names(df))) {
          lng_col <- names(df)[tolower(names(df)) == "lng"]
          processed_data[[lng_col]] <- df[[lng_col]]
        }
        
        df_with_prestige(processed_data)
        
        incProgress(0.50, detail = "Building data tables")
        

        # --- print out the min and max of the prestige_total, regardless of method ---
        min_val <- min(processed_data$combined_total, na.rm = TRUE)
        max_val <- max(processed_data$combined_total, na.rm = TRUE)
        output$debug <- renderPrint({
          cat("Prestige total range: min =", min_val, ", max =", max_val, "\n")
        })
        
        # Recompute TOT plot from the file (non-reactively).
        df_TOT_range_p <- get_TOT_range(df_for_calc)
        medians_GG_p <- get_TOT_medians(df_TOT_range_p)
        TOT_plot <- merge(x = df_TOT_range_p, y = medians_GG_p, by = "Type")
        
        incProgress(0.55, detail = "Rendering original data table")

        output$originalTable <- renderDT({ 
          cap_text <- 'Original counts table (all values changed to 1s for the boxplot y-scale)'
          if (input$culture != "" || (!is.null(input$startYear) && !is.null(input$endYear))) {
            cap_text <- paste0(cap_text, " — ", 
                              build_title("", input$culture, input$startYear, input$endYear))
          }
          datatable(df, options = list(pageLength = 10), 
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: left;',
                      cap_text
                    )) 
        })
        output$updatedTable <- renderDT({ 
          cap_text <- 'Prestige values (derived from Total Object Types (TOT))'
          if (input$culture != "" || (!is.null(input$startYear) && !is.null(input$endYear))) {
            cap_text <- paste0(cap_text, " — ", 
                              build_title("", input$culture, input$startYear, input$endYear))
          }
          # Hide internal columns from display
          display_data <- df_with_prestige()
          hide_cols <- c("is_adult_grave", "is_subadult_only")
          display_data <- display_data[, !(names(display_data) %in% hide_cols), drop = FALSE]
          datatable(display_data, options = list(pageLength = 10), 
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: left;',
                      cap_text
                    )) 
        })
        
        incProgress(0.60, detail = "Preparing plot data")
        
        # Freeze the plot data into local variables so that new calculations do not update previous plots.
        # When age separation is on, use adult-only data so the boxplot matches the Bayesian estimate's data scope.
        # When age separation is off, use full TOT range (treats empty age cols as adult).
        if (use_age_separation) {
          local_df_range <- age_result$adult_TOT_range
        } else {
          local_df_range <- get_TOT_range(df_for_calc)
        }
        # Include subadult-exclusive types' TOT data when age separation is on
        if (use_age_separation && !is.null(subadult_df_range) && nrow(subadult_df_range) > 0) {
          local_df_range <- rbind(local_df_range, subadult_df_range)
        }
        local_TOT_plot <- TOT_plot         # snapshot of TOT_plot
        
        # Calculate plot width based on number of categories
        n_categories <- length(unique(local_df_range$Type))
        plot_width <- paste0(max(600, n_categories * 20))

        incProgress(0.70, detail = "Building prestige boxplot") 

        if (input$calcMethod %in% c("bayesian_empirical")) {
          local_bayes_values <- bayes_values  # snapshot of bayes_values
          
          # Get subadult-exclusive types for marking with "*"
          subadult_types <- subadultExclusiveTypes()
          if (is.null(subadult_types)) subadult_types <- c()

          # Conditional x-axis label
          x_label <- if (length(subadult_types) > 0) {
            "Grave good type (* = subadults only)"
          } else {
            "Grave good type"
          }
          
          # Add "*" prefix to subadult-exclusive types for display
          local_bayes_values$Type_display <- ifelse(
            local_bayes_values$Type %in% subadult_types,
            paste0("*", local_bayes_values$Type),
            as.character(local_bayes_values$Type)
          )
          local_df_range$Type_display <- ifelse(
            local_df_range$Type %in% subadult_types,
            paste0("*", local_df_range$Type),
            as.character(local_df_range$Type)
          )
          
          # compute an ordering of the types by bayes_est, with subadult types at the end
          levels_ordered <- local_bayes_values %>%
            mutate(is_subadult = grepl("^\\*", Type_display)) %>%
            arrange(is_subadult, bayes_est) %>%
            pull(Type_display)
          
          # turn your data frames' Type_display into that ordered factor
          local_df_range$Type_display <- factor(local_df_range$Type_display, levels = levels_ordered)
          local_bayes_values$Type_display <- factor(local_bayes_values$Type_display, levels = levels_ordered)
          
          safe_log("About to create boxplot")
          # now make the Bayesian boxplot
          newPlot <- isolate({
            ggplot() +
              geom_boxplot(
                data = local_df_range,
                aes(x = Type_display, y = TOT),
                outlier.shape = NA
              ) +
              geom_jitter(
                data = local_df_range,
                aes(x = Type_display, y = TOT),
                width = 0.1, height = 0.1, size = 0.7, alpha = 0.5
              ) +
              geom_errorbar(
                data = local_bayes_values,
                aes(x = Type_display, ymin = lowerCI, ymax = upperCI),
                width = 0.1, color = "blue"
              ) +
              geom_point(
                data = local_bayes_values,
                aes(x = Type_display, y = bayes_est),
                color = "blue", size = 1.2
              ) +
              theme_bw() +
              theme(
                axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1, size= 8),
                axis.title.y = element_text(color = "red", face = "bold", size = 10)
              ) +
              labs(title = build_title(
                  "Bayesian Prestige Estimates (Empirical prior)",
                  input$culture, input$startYear, input$endYear),
                x = x_label,
                y = "Total object types (TOT)")+
                theme(plot.title = element_text(size = 10))
          })
        } else {
          # Get subadult-exclusive types for marking with "*"
          subadult_types <- subadultExclusiveTypes()
          if (is.null(subadult_types)) subadult_types <- c()

          # Conditional x-axis label
          x_label <- if (length(subadult_types) > 0) {
            "Grave good/feature type (* = subadults only)"
          } else {
            "Count data type"
          }

          
          # Add "*" prefix to subadult-exclusive types for display
          local_TOT_plot$Type_display <- ifelse(
            local_TOT_plot$Type %in% subadult_types,
            paste0("*", local_TOT_plot$Type),
            as.character(local_TOT_plot$Type)
          )
          
          # Create custom ordering: adult types first (sorted by mean/median), then subadult types
          fun_reorder <- if (input$calcMethod == "mean") mean else median
          
          # Calculate mean/median TOT for each type
          type_order <- local_TOT_plot %>%
            group_by(Type_display) %>%
            summarise(avg_TOT = fun_reorder(TOT), .groups = "drop") %>%
            mutate(is_subadult = grepl("^\\*", Type_display)) %>%
            arrange(is_subadult, avg_TOT) %>%
            pull(Type_display)
          
          local_TOT_plot$Type_display <- factor(local_TOT_plot$Type_display, levels = type_order)
          
          newPlot <- isolate({
            p_mean <- ggplot(local_TOT_plot, aes(x = Type_display, y = TOT)) +
              geom_boxplot(outlier.shape = NA) +
              geom_jitter(width = 0.1, height = 0.1, size = 0.3, alpha = 0.5)
            
            # 2. only if method == "mean", tack on the mean‐points
            if (input$calcMethod == "mean") {
              p_mean <- p_mean + stat_summary(fun = mean, geom = "point", color = "blue", size = 1, shape = 18
              )
            }
            
            p_mean +  theme_bw() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
                    axis.title.y = element_text(color = "red", face = "bold", size = 10)) +
              labs(title = build_title(
                            "Prestige Estimates (Mean) from Total Object Types (TOT)",
                     input$culture, input$startYear, input$endYear),
                   x = x_label, y = "Total object types (TOT)") +
              theme(plot.title = element_text(size = 10))
          })
        }
        
        incProgress(0.80, detail = "Converting to interactive plot")
        # convert your ggplot to a Plotly object
        newPlot <- plotly::ggplotly(newPlot, tooltip = c("x", "y", "ymin", "ymax")) %>%
          layout(
            hoverlabel = list(
              bgcolor = "white",
              font = list(size = 12),
              align = "left"
            ),
            hovermode = "x unified"
          ) %>% 
          # force the in‐page canvas size (optional)
          layout(width = plot_width) %>%
          # override the 'download as png' dimensions
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = "prestige_boxplot",
              width  = plot_width,
              height = 600,
              scale  = 3    # 2× for retina-quality
            )
          )
        
        # loop over its traces and turn off box‑outliers
        for(i in seq_along(newPlot$x$data)) {
          if (newPlot$x$data[[i]]$type == "box") {
            newPlot$x$data[[i]]$boxpoints <- FALSE
          }
        }
        # our addStaticPlot function is not used anymore.
        #addStaticPlot(newPlot, height = "400px")
        # Calculate width based on number of categories (min 600px, ~40px per category)
        #n_categories <- length(unique(df_range$Type))
        #plot_width <- paste0(max(600, n_categories * 20), "px")
        addInteractivePlot(newPlot, height = "400px", width = plot_width)

        # Add link to Bayesian method documentation (appears above plot since updatePlotUI prepends)
        if (input$calcMethod %in% c("bayesian_default", "bayesian_empirical")) {
          updatePlotUI(
            tags$p(
              style = "margin-bottom: 5px; font-size: 0.9em;",
              tags$a(
                href = "https://quantwealth.eu/bayesian_method.html",
                target = "_blank",
                "📖 Read about the Bayesian prestige estimation method"
              )
            )
          )
        }
        storedPrestigePlot(newPlot)  # Store for later saving
        
        output$downloadData <- downloadHandler(
          filename = function() { paste("prestige_data-", Sys.Date(), ".csv", sep = "") },
          content = function(file) {
            write.table(df_with_prestige(), file, row.names = FALSE, sep = ";", quote = FALSE)
          }
        )
        
        
        
        shinyjs::enable("calcGiniLorenz")
        shinyjs::enable("downloadData")  # Enable download right after prestige calculation
        #shinyjs::enable("testCategoryBias")
        prestigePlotGenerated(TRUE)

        incProgress(1, detail = "Done!")

      }, error = function(e) {
        output$debug <- renderPrint({ paste("An error occurred: ", e$message) })
      })
      }) # closes withProgress() Progress Bar
    })
    
    #### Calculate House Size Gini ####
    observeEvent(input$calcHouseGini, {
      req(input$houseDatafile)
      
      tryCatch({
        house_data <- house_data_reactive()
        if (is.null(house_data)) {
          showNotification("Error processing data. Please check your file format.", type = "error")
          return()
        }
        
        house_data_processed(house_data)
        
        withProgress(message = "Computing Size Gini …", value = 0, {
          incProgress(0.1, detail = "Preparing data")
          
          house_sizes <- house_data$Value
          
          if (length(house_sizes) < 2) {
            showNotification("Need at least 2 physical measure (e.g. house m2) data points to calculate Gini.", type = "error")
            return()
          }
          
          incProgress(0.3, detail = "Calculating House Gini")
          house_gini_result <- calculate_house_gini(house_sizes)
          
          if (!is.na(house_gini_result[1])) {
            houseGiniCoeff(round(house_gini_result[1], 2))
          } else {
            showNotification("House Gini coefficient calculation returned NA.", type = "error")
            return(NULL)
          }
          
          incProgress(0.5, detail = "Building house plots")
          
          # Display data table with dynamic column names
          output$houseTable <- renderDT({ 
            display_data <- house_data
            names(display_data)[1:2] <- c(physicalIdLabel(), physicalValueLabel())
            
            datatable(display_data, options = list(pageLength = 10), 
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: left;',
                        paste(input$physicalDataType, 'data')
                      )) 
          })
          
          incProgress(0.7, detail = "Creating density plot")
          
          # Create density plot with dynamic labels
          house_density_plot <- ggplot(house_data, aes(x = Value)) +
            stat_density(geom = "line", colour = "purple", linewidth = 1) +
            geom_vline(aes(xintercept = max(Value, na.rm = TRUE)), 
                       color = "purple", linetype = "dotted", size = 0.7) +
            theme_bw() +
            xlab(physicalValueLabel()) +
            ylab("Density") +
            ggtitle(paste(input$physicalDataType, "Distribution"))
          
          incProgress(0.8, detail = "Creating Lorenz plot")
          
          # Create Lorenz curve with dynamic labels
          house_lorenz_plot <- ggplot(house_data, aes(Value)) +
            stat_lorenz(color = "purple", geom = "path", linetype = "solid", linewidth = 1) +
            geom_abline(color = "grey") +
            theme_bw() +
            xlab("Sample population %") +
            ylab(paste(physicalValueLabel(), "%")) +
            ggtitle(paste(input$physicalDataType, "Lorenz Curve"))
          
          
          # Calculate ± value from confidence interval (half the CI width)
          ci_house <- if(length(house_gini_result) >= 3) round((house_gini_result[3] - house_gini_result[2]) / 2, 2) else NA
          ci_house_str <- if(!is.na(ci_house)) paste0(" ±", ci_house) else ""
          
          medianHouseSizeVal <- median(house_data_processed()$Value)
          minHouseSizeVal <- min(house_data_processed()$Value)
          maxHouseSizeVal <- max(house_data_processed()$Value)
          
          house_html_label <- paste0(
            "<b style='color:purple; font-weight:bold;'>", input$physicalDataType, " Absolute Gini: ", round(house_gini_x_mean(), 2), "</b>", "\n",
            "<span style='color:purple;'>", input$physicalDataType, " Gini: ", round(houseGiniCoeff(), 2), ci_house_str, "</span>", "\n",
            "Mean/Median: ", round(meanHouseSize(), 2), "/", round(medianHouseSizeVal, 2), " ", physicalUnit(), "\n",
            "Sample Size: ", houseSampleSize()
          )

          incProgress(0.9, detail = "Combining house plots")
          
          # Convert to plotly
          hp1 <- ggplotly(house_lorenz_plot) %>%
            layout(
              margin = list(l = 30, r = 50, t = 50, b = 40),
              annotations = list(
                list(
                  x    = 0.05,
                  xref = "x1 domain",
                  y    = 0.95, 
                  yref = "y1 domain",
                  text = house_html_label,
                  showarrow = FALSE,
                  align = "left"
                )
              )
            )
          
          hp2 <- ggplotly(house_density_plot)
          for (i in seq_along(hp2$x$data)) {
            tr <- hp2$x$data[[i]]
            if (!is.null(tr$fill) && tr$fill != "none") {
              tr$fill       <- "none"
              tr$fillcolor  <- "rgba(0,0,0,0)"
              tr$showlegend <- FALSE
            }
            hp2$x$data[[i]] <- tr
          }
          
          # --- HOUSE DECILE PLOT ---
          house_decile_data <- house_data %>%
            arrange(Value) %>%
            mutate(
              wealth_rank = ntile(Value, 10),
              wealth_share = Value / sum(Value)
            ) %>%
            group_by(wealth_rank) %>%
            summarize(
              total_wealth = round(sum(wealth_share), 2),
              n_in_decile = n(),
              .groups = "drop"
            ) %>%
            arrange(wealth_rank) %>%
            mutate(
              cumulative_wealth = cumsum(total_wealth),
              mid_point = cumulative_wealth - total_wealth/2
            )
          
          max_house_val <- max(house_data$Value)
          house_decile_data$wealth_rank <- factor(house_decile_data$wealth_rank, levels = rev(1:10))
          
          house_x_label <- if (input$houseCulture != "") {
            paste0(input$houseCulture, "\n(max: ", round(max_house_val, 2), " ", physicalUnit(), ")")
          } else {
            paste0("Max: ", round(max_house_val, 2), " ", physicalUnit())
          }
          
          house_decile_gg <- ggplot(house_decile_data, aes(x = 1, y = round(total_wealth, 2), fill = wealth_rank)) +
            geom_col(width = 0.6) +
            geom_text(aes(y = mid_point, label = n_in_decile), size = 3, color = "black") +
            scale_x_continuous(breaks = 1, labels = house_x_label) +
            labs(
              #title = "Decile distribution",
              x = NULL,
              y = "Proportion of total",
              fill = "Decile"
            ) +
            theme_minimal() +
            scale_fill_brewer(palette = "Spectral") +
            theme(
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
            )
          
          hp3 <- ggplotly(house_decile_gg, tooltip = c("fill", "y"))
          # Disable hover on text labels
          for (i in seq_along(hp3$x$data)) {
            trace_mode <- hp3$x$data[[i]]$mode
            if (!is.null(trace_mode) && trace_mode == "text") {
              hp3$x$data[[i]]$hoverinfo <- "skip"
            }
          }
          hp3 <- hp3 %>% layout(
              margin = list(l = 30, r = 10, t = 50, b = 80),
              annotations = list(
                list(
                  x = 0.5,
                  xref = "x3 domain",
                  y = 1.08,
                  yref = "y3 domain",
                  text = "Decile distribution",
                  showarrow = FALSE,
                  font = list(size = 14, color = "black"),
                  align = "center"
                )
              )
            )
          
          storedHouseDecilePlot(hp3)
          
          # Combine house plots
          houseCombinedPlot <- subplot(
            hp1, hp2, hp3,
            nrows    = 1,
            widths   = c(0.45, 0.45, 0.10),
            shareX   = FALSE,
            shareY   = FALSE,
            titleX   = TRUE,
            titleY   = TRUE,
            margin   = 0.04
          ) %>%
            config(
              toImageButtonOptions = list(
                format   = "png",
                filename = "house_size_analysis",
                width    = 1400,
                height   = 500,
                scale    = 2
              )
            )
          
          addInteractivePlot(houseCombinedPlot, height = "400px", width = "100%")
          storedHouseGiniPlot(houseCombinedPlot)  # Store for later saving
          houseFilesReady(TRUE)

          incProgress(1)
          houseGiniGenerated(TRUE)
          shinyjs::enable("addHouseResult")
          
          showNotification("Gini calculated successfully!", type = "message")
        })
        
      }, error = function(e) {
        safe_log(paste("Gini calculation error:", e$message))
        showNotification(paste("Error calculating Gini: ", e$message), type = "error")
      })
    })
    
    
    #### Calculate Gini and Lorenz Curves ####
    observeEvent(input$calcGiniLorenz, {
      tryCatch({
        if (!is.null(df_with_prestige())) {
          
          # ── progress bar wrapper ─────────────────────────────────────────────
          withProgress(message = "Computing Gini & Lorenz …", value = 0, {
            incProgress(0.05, detail = "Preparing data")
            # ---- pick all three wealth measures --------------------------
            prestige_vec  <- df_with_prestige()$prestige_total           # as before
            combined_vec  <- df_with_prestige()$combined_total           # NEW  (added above)
            tot_vec       <- rawTOT_values() + 1                         # TOT+1 (unchanged)
            
            # ---- Gini coefficients ---------------------------------------
            incProgress(0.25, detail = "Calculating Ginis")
            if (length(prestige_vec) > 500) {
	    Gini_prestige <- DescTools::Gini(prestige_vec,
                                             unbiased = TRUE) 
	    } else {
	    Gini_prestige <- DescTools::Gini(prestige_vec,
                                             conf.level = 0.80, unbiased = TRUE,
                                             type = "basic", R = 200)
      incProgress(0.30, detail = "Calculating Ginis")}

      if (length(combined_vec) > 500) {
	    Gini_combined <- DescTools::Gini(combined_vec,
                                             unbiased = TRUE)
	    } else {
	    Gini_combined <- DescTools::Gini(combined_vec,
                                             conf.level = 0.80, unbiased = TRUE,
                                             type = "basic", R = 200)}
            
      incProgress(0.35, detail = "Calculating Ginis")
	    if (length(tot_vec) > 500) {
            Gini_tot      <- DescTools::Gini(tot_vec,
                                             unbiased = TRUE)
	    } else {
            Gini_tot      <- DescTools::Gini(tot_vec,
                                             conf.level = 0.80, unbiased = TRUE,
                                             type = "basic", R = 200)}
            
            abs_gini_prestige <- round(Gini_prestige[1] * mean(prestige_vec),  2)
            abs_gini_combined <- round(Gini_combined[1] * mean(combined_vec), 2)
            
            incProgress(0.40, detail = "Calculating prestige+√count Gini")
            
            # Check if we have age-separated data
            has_age <- hasAgeData() && !is.null(df_with_prestige()$is_adult_grave)
            
            if (has_age) {
              # Separate data by age group
              adult_data <- df_with_prestige()[df_with_prestige()$is_adult_grave, ]
              subadult_data <- df_with_prestige()[df_with_prestige()$is_subadult_only, ]
              
              safe_log(paste("Gini calculation - Adult graves:", nrow(adult_data), "Subadult-only graves:", nrow(subadult_data)))
              
              # Adult Gini (this becomes the main Gini)
              Gini_coeff <- DescTools::Gini(unlist(adult_data$combined_total),
                                            conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
              Gini_coeff_simple <- DescTools::Gini(unlist(adult_data$prestige_total),
                                                   conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
              
              # Subadult Gini (if we have subadult-only graves)
              if (nrow(subadult_data) >= 2) {
                Gini_subadult <- DescTools::Gini(unlist(subadult_data$combined_total),
                                                 conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
                giniCoeffSubadult(round(Gini_subadult[1], 2))
                meanWealthSubadult(mean(subadult_data$combined_total))
                medianWealthSubadult(median(subadult_data$combined_total))
                sampleSizeSubadult(nrow(subadult_data))
                safe_log(paste("Subadult Gini:", round(Gini_subadult[1], 2), "n =", nrow(subadult_data)))
              } else {
                giniCoeffSubadult(NULL)
                meanWealthSubadult(NULL)
                medianWealthSubadult(NULL)
                sampleSizeSubadult(nrow(subadult_data))
                safe_log("Not enough subadult-only graves for Gini calculation")
              }
              
              # Update sample size to reflect adult graves only for main Gini
              # (We'll show both in the display)
              meanWealth(mean(adult_data$combined_total))
              
            } else {
              # No age separation - calculate as before (all graves)
              Gini_coeff <- DescTools::Gini(unlist(df_with_prestige()$combined_total),
                                            conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
              Gini_coeff_simple <- DescTools::Gini(unlist(df_with_prestige()$prestige_total),
                                                   conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
              giniCoeffSubadult(NULL)
              meanWealthSubadult(NULL)
              medianWealthSubadult(NULL)
              sampleSizeSubadult(NULL)
              meanWealth(mean(df_with_prestige()$combined_total))
            }
            
            incProgress(0.45, detail = "Calculating Prestige Gini")
            
            # Set mean wealth based on age separation
            #meanWealthVal <- mean(df_with_prestige()$combined_total)

            if (!is.na(Gini_coeff[1])) {
              giniCoeff(round(Gini_coeff[1], 2))
            } else {
              print("Gini coefficient calculation returned NA.")
              return(NULL)
            }
            
            # Create a friendly label for the method:
            methodText <- switch(input$calcMethod,
                                 "mean" = "Mean",
                                 "median" = "Median",
                                 #"bayesian_default" = "Bayesian (default)",
                                 "bayesian_empirical" = "Bayesian (empirical)")
            
            
            # Get the raw TOT vector from rawTOT_values() and add 1 (as done in your prestige calculation).
            rawTOT <- rawTOT_values()  # rawTOT_values() is a reactive returning a vector
            
            # If age separation, use only adult graves for TOT Gini
            if (has_age) {
              adult_indices <- which(df_with_prestige()$is_adult_grave)
              rawTOT_adult <- rawTOT[adult_indices]
              TOTGini <- DescTools::Gini(rawTOT_adult + 1, conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
            } else {
              TOTGini <- DescTools::Gini(rawTOT + 1, conf.level = 0.80, unbiased = TRUE, type = "basic", R = 200)
            }
            incProgress(0.50, detail = "Calculating TOT Gini")
            
            
            # Calculate ± values from confidence intervals (half the CI width)
            ci_combined <- if(length(Gini_coeff) >= 3) round((Gini_coeff[3] - Gini_coeff[2]) / 2, 2) else NA
            ci_prestige <- if(length(Gini_coeff_simple) >= 3) round((Gini_coeff_simple[3] - Gini_coeff_simple[2]) / 2, 2) else NA
            ci_tot <- if(length(TOTGini) >= 3) round((TOTGini[3] - TOTGini[2]) / 2, 2) else NA
            
            # Format CI strings (show ± only if CI was computed)
            ci_combined_str <- if(!is.na(ci_combined)) paste0(" ±", ci_combined) else ""
            ci_prestige_str <- if(!is.na(ci_prestige)) paste0(" ±", ci_prestige) else ""
            ci_tot_str <- if(!is.na(ci_tot)) paste0(" ±", ci_tot) else ""
            
            # Use adult-only data if age separation exists
            if (hasAgeData() && !is.null(df_with_prestige()$is_adult_grave)) {
               adult_wealth <- df_with_prestige()$combined_total[df_with_prestige()$is_adult_grave]
               medianWealthVal <- median(adult_wealth)
               minWealthVal <- min(adult_wealth)
               maxWealthVal <- max(adult_wealth)
            } else {
                 medianWealthVal <- median(df_with_prestige()$combined_total)
                 minWealthVal <- min(df_with_prestige()$combined_total)
                 maxWealthVal <- max(df_with_prestige()$combined_total)
            }
            
            # Build subadult info string if available
            subadult_info <- ""
            if (!is.null(giniCoeffSubadult())) {
              subadult_abs_gini <- round(giniCoeffSubadult() * meanWealthSubadult(), 2)
              subadult_info <- paste0(
                "\n<span style='color:orange;'>Subadult G: ", giniCoeffSubadult(), 
                " | Abs: ", subadult_abs_gini,
                " | n=", sampleSizeSubadult(), "</span>"
              )
            } else if (!is.null(sampleSizeSubadult()) && sampleSizeSubadult() > 0) {
              subadult_info <- paste0("\n<span style='color:orange;'>Subadult n=", sampleSizeSubadult(), " (too few for Gini)</span>")
            }
            
            # Update sample size display to show adult count if age-separated
            sample_size_text <- if (hasAgeData() && !is.null(adultGraveCount())) {
              paste0(adultGraveCount(), " adults")
            } else {
              as.character(sampleSize())
            }
            
            html_label <- paste0(
              "<b style='color:green; font-weight:bold;'>Absolute Gini (G×mean): ", round(gini_x_mean(), 2), "</b>", "\n",
              "<span style='color:green;'>G (prestige+√count): ", round(Gini_coeff[1], 2), ci_combined_str, "</span>", "\n",
              " (<span style='color:blue;'>G: ", round(Gini_coeff_simple[1], 2), ci_prestige_str, "</span>",
              ", <span style='color:red;'>TOT G: ", round(TOTGini[1], 2), ci_tot_str, "</span>)", "\n",
              "Mean/Median Wealth: ", round(meanWealth(), 2), "/", round(medianWealthVal, 2), "\n",
              "Sample Size: ", sample_size_text,
              subadult_info, "\n",
              "Method: ", methodText
            )
            
            incProgress(0.55, detail = "Building Lorenz plot")
            
            Lorenz_raw_Gini.p <- ggplot() +
              stat_lorenz(data = df_with_prestige(), aes(prestige_total), color = "blue", geom = "path", linetype = "dashed", linewidth = 0.5) +
              stat_lorenz(data = data.frame(rawTOT = rawTOT + 1), aes(rawTOT), color = "red", geom = "path", linetype = "dashed", linewidth = 0.5) +
              stat_lorenz(data = df_with_prestige(), aes(combined_total), color = "green", geom = "path", linetype = "solid", linewidth = 0.5) +
              geom_abline(color = "grey") +
              theme_bw() +
              theme(legend.position = "none") +
              xlab("Sample population %") +
              ylab("Prestige sum %")
            # Use geom_richtext instead of geom_text
            # geom_richtext(
            #   data = Gini_label_df,
            #   aes(x = x, y = y, label = label),
            #   size = 5, hjust = 1,
            #   fill = NA,         # Remove background box
            #   label.color = NA   # Remove border around text
            # )
            
            incProgress(0.65, detail = "Building prestige plot")
            prestige_density.p <- ggplot() +
              stat_density(data = data.frame(rawTOT = rawTOT + 1), aes(x = rawTOT), geom = "line", colour = "red", linewidth = .5, linetype = "dashed") +
              stat_density(data = df_with_prestige(), aes(x = prestige_total), geom = "line", colour = "blue", linewidth = .5, linetype = "dashed") +
              stat_density(data = df_with_prestige(), aes(x = combined_total), geom = "line", colour = "green", linewidth = .5) +
              geom_vline(aes(xintercept = max(rawTOT + 1, na.rm = TRUE),
                             text = paste("Max TOT+1:", round(max(rawTOT + 1, na.rm = TRUE), 2))), 
                         color = "red", linetype = "dotted", size = 0.7) +
              geom_vline(aes(xintercept = max(df_with_prestige()$prestige_total, na.rm = TRUE),
                             text = paste("Max Prestige:", round(max(df_with_prestige()$prestige_total, na.rm = TRUE), 2))),
                         color = "blue", linetype = "dotted", size = 0.7) +
              geom_vline(aes(xintercept = max(df_with_prestige()$combined_total, na.rm = TRUE),
                             text = paste("Max Combined:", round(max(df_with_prestige()$combined_total, na.rm = TRUE), 2))),
                         color = "green", linetype = "dotted", size = 0.7) +
              theme_bw() +
              xlab("Value") +
              ylab("Density")
            #ggtitle("Density of Prestige Values (blue) vs TOT+1 (red dashed)") 
            

            
            # 1. convert each ggplot to plotly
            incProgress(0.75, detail = "Rendering Lorenz plot")
            p1 <- ggplotly(Lorenz_raw_Gini.p) %>%
              layout(
                margin = list(l = 30, r = 50, t = 50, b = 40),
                annotations = list( #this is the Gini results annotation inside the plot
                  list(
                    x    = 0.05,            # 5% into panel 1
                    xref = "x1 domain",     # panel 1's domain
                    y    = 0.95, 
                    yref = "y1 domain",
                    text = html_label,
                    showarrow = FALSE,
                    align = "left"
                  ), 
                  # Lorenz‐Curve title in panel 1
                  # Lorenz‐Curve title in panel 1
                  list( #this is the title
                    x         = 0,
                    xref      = "x1 domain",
                    xanchor   = "left",
                    y         = 1.08,
                    yref      = "y1 domain",
                    text      = {
                      # Build single-line title for Lorenz plot
                      parts <- "Lorenz Curve and Ginis"
                      if (input$culture != "") parts <- paste(parts, "—", input$culture)
                      if (!is.null(input$startYear) && !is.null(input$endYear)) {
                        parts <- paste(parts, "—", format_dates(input$startYear, input$endYear))
                      }
                      parts
                    },
                    showarrow = FALSE,
                    font      = list(size = 14, color = "black"),
                    align     = "left"
                  )
                )
              )    
            
            incProgress(0.80, detail = "Rendering density plot")
            p2 <- ggplotly(prestige_density.p, tooltip = c("x", "y", "text"))
            for (i in seq_along(p2$x$data)) {
              tr <- p2$x$data[[i]]
              if (!is.null(tr$fill) && tr$fill != "none") {   # a density ribbon
                tr$fill       <- "none"                      # no polygon
                tr$fillcolor  <- "rgba(0,0,0,0)"             # fully transparent
                tr$showlegend <- FALSE                       # drop "fill = NA" legend
              }
              p2$x$data[[i]] <- tr
            }
            # remove the ribbon for every density trace
            #plotly::style(fill = "none", traces = seq_along(.) ) %>% 
            p2 <- p2 %>% 
              layout(margin = list(l = 30, r = 50, t = 50, b = 40),
                     annotations = list(
                       list(
                         x         = 0.5,          # centred in panel‑2
                         xref      = "x2 domain",  # "x2" → 2nd subplot
                         y         = 1.08,         # 8 % above top axis
                         yref      = "y2 domain",
                         text = paste0(
                           "Density curves: ",
                           "<span style='color:blue;'>Prestige</span>",
                           ", ",
                           "<span style='color:green;'>+√count</span>",
                           ", ",
                           "<span style='color:red;'>TOT+1</span>"
                         ),
                         showarrow = FALSE,
                         font      = list(size = 14, color = "black"),
                         align     = "center"
                       )
                     )
              )
            incProgress(0.85, detail = "Building decile plot")
            
            # --- DECILE PLOT ---
            decile_data <- df_with_prestige() %>%
              arrange(combined_total) %>%
              mutate(
                wealth_rank = ntile(combined_total, 10),
                wealth_share = combined_total / sum(combined_total)
              ) %>%
              group_by(wealth_rank) %>%
              summarize(
                total_wealth = sum(wealth_share),
                n_in_decile = n(),
                .groups = "drop"
              ) %>%
              arrange(wealth_rank) %>%
              mutate(
                cumulative_wealth = cumsum(total_wealth),
                mid_point = cumulative_wealth - total_wealth/2
              )
            
            max_wealth_val <- max(df_with_prestige()$combined_total)
            decile_data$wealth_rank <- factor(decile_data$wealth_rank, levels = rev(1:10))
            
            x_label <- if (input$culture != "") {
              paste0(input$culture, "\n(max: ", round(max_wealth_val, 2), ")")
            } else {
              paste0("Max wealth: ", round(max_wealth_val, 2))
            }
            
            decile_gg <- ggplot(decile_data, aes(x = 1, y = total_wealth, fill = wealth_rank)) +
              geom_col(width = 0.6) +
              geom_text(aes(y = mid_point, label = n_in_decile), size = 3, color = "black") +
              scale_x_continuous(breaks = 1, labels = x_label) +
              labs(
                #title = "Decile distribution",
                x = NULL,
                y = "Proportion of total wealth",
                fill = "Decile"
              ) +
              theme_minimal() +
              scale_fill_brewer(palette = "Spectral") +
              theme(
                axis.ticks.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
              )
            
            p3 <- ggplotly(decile_gg, tooltip = c("fill", "y"))
            # Disable hover on text labels
            for (i in seq_along(p3$x$data)) {
              trace_mode <- p3$x$data[[i]]$mode
              if (!is.null(trace_mode) && trace_mode == "text") {
                p3$x$data[[i]]$hoverinfo <- "skip"
              }
            }
            p3 <- p3 %>% layout(
                margin = list(l = 30, r = 10, t = 50, b = 80),
                annotations = list(
                  list(
                    x = 0.5,
                    xref = "x3 domain",
                    y = 1.08,
                    yref = "y3 domain",
                    text = "Decile distribution",
                    showarrow = FALSE,
                    font = list(size = 14, color = "black"),
                    align = "center"
                  )
                )
              )
            
            storedDecilePlot(p3)
            
            incProgress(0.88, detail = "Combining plots")
            #put the sub plots side by side
            combinedPlot <- subplot(
              p1, p2, p3,
              nrows    = 1,
              widths   = c(0.50, 0.40, 0.10),
              shareX   = FALSE,
              shareY   = FALSE,
              titleX   = TRUE,
              titleY   = TRUE,
              margin   = 0.04
            ) %>%
              config(
                toImageButtonOptions = list(
                  format   = "png",
                  filename = "prestige_and_lorenz",
                  width    = 1400,
                  height   = 500,
                  scale    = 2
                )
              )
            incProgress(0.90, detail = "Rendering UI")
            
            addInteractivePlot(combinedPlot, height = "450px", width = "110%")
            
            #addStaticPlot(combinedPlot, height = "300px")
            #addInteractivePlot(combinedPlot, height="400px")
            storedGiniLorenzPlot(combinedPlot)  # Store grave density-Lorenz plot for later saving
            graveFilesReady(TRUE)
            incProgress(0.95, detail = "Rendering UI")
            giniLorenzGenerated(TRUE)
            
            incProgress(1)  # done
          }) # closes withProgress
        } # closes if-block
      }, error = function(e) {
        output$debug <- renderPrint({ paste("An error occurred: ", e$message) })
      })
      
      #shinyjs::enable("addResult")
      shinyjs::enable("testCategoryBias")
      shinyjs::enable("downloadData")
    })
    

#### Test Category Bias ####
#### Test Category Bias ####
observeEvent(input$testCategoryBias, {
  req(input$datafile)
  req(df_with_prestige())
  
  safe_log("=== Category bias button clicked ===")
  
  withProgress(message = "Simulating category bias...", value = 0, {
    
    incProgress(0.1, detail = "Preparing data")
    
    inFile <- input$datafile
    df <- read_data_file(inFile)
    
    # Filter to adult graves only if age columns exist
    col_names_lower <- tolower(names(df))
    has_adult_col <- "adult_count" %in% col_names_lower
    has_subadult_col <- "subadult_count" %in% col_names_lower
    
    if (has_adult_col || has_subadult_col) {
      adult_col <- names(df)[col_names_lower == "adult_count"]
      subadult_col <- names(df)[col_names_lower == "subadult_count"]
      
      adult_counts <- if (length(adult_col) > 0) as.numeric(df[[adult_col[1]]]) else rep(1, nrow(df))
      adult_counts[is.na(adult_counts)] <- 0
      
      subadult_counts <- if (length(subadult_col) > 0) as.numeric(df[[subadult_col[1]]]) else rep(0, nrow(df))
      subadult_counts[is.na(subadult_counts)] <- 0
      
      # Keep adult graves and graves with no age info (adult_count >= 1 OR both counts are 0)
      is_adult_grave <- adult_counts >= 1 | (adult_counts == 0 & subadult_counts == 0)
      df <- df[is_adult_grave, ]
      
      safe_log(paste("Bias: filtered to adult graves, rows=", nrow(df)))
    }
    
    safe_log(paste("Bias: data loaded, rows=", nrow(df), "cols=", ncol(df)))
    
    incProgress(0.2, detail = "Running simulations (this may take a moment)")
    
    sim_results <- simulate_category_bias(
      df = df,
      n_sims = 100,
      base_max_split = 5,
      adult_counts = if (exists("adult_counts") && exists("is_adult_grave")) adult_counts[is_adult_grave] else NULL
    )
    
    if (is.null(sim_results)) {
      showNotification("Could not run simulation - check data format", type = "error")
      return()
    }
    
    incProgress(0.8, detail = "Building plots")
    
    # Build data with splits info for tooltips
    hist_data <- data.frame(
      Gini = c(
        sim_results$ginis_split_TOT,
        sim_results$ginis_lump_TOT,
        sim_results$ginis_split_TOT_sqrt,
        sim_results$ginis_lump_TOT_sqrt,
        sim_results$ginis_split_prestige,
        sim_results$ginis_lump_prestige,
        sim_results$ginis_split_prestige_sqrt,
        sim_results$ginis_lump_prestige_sqrt
      ),
      Columns = c(
        sim_results$total_splits,
        sim_results$total_lumped_cols,
        sim_results$total_splits,
        sim_results$total_lumped_cols,
        sim_results$total_splits,
        sim_results$total_lumped_cols,
        sim_results$total_splits,
        sim_results$total_lumped_cols
      ),
      Direction = factor(rep(
        c("Split", "Lump"),
        times = 4,
        each = sim_results$n_sims
      ), levels = c("Lump", "Split")),
      Method = factor(rep(
        c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count"),
        each = sim_results$n_sims * 2
      ), levels = c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count"))
    )
    
    # Original Gini reference lines
    orig_lines <- data.frame(
      Method = factor(
        c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count"),
        levels = c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count")
      ),
      orig_gini = c(
        sim_results$orig_gini_TOT,
        sim_results$orig_gini_TOT_sqrt,
        sim_results$orig_gini_prestige,
        sim_results$orig_gini_prestige_sqrt
      )
    )
    
    # Summary stats for subtitle
    split_range <- paste0(min(sim_results$total_splits), "-", max(sim_results$total_splits))
    lump_range <- paste0(min(sim_results$total_lumped_cols), "-", max(sim_results$total_lumped_cols))
    
    # Summary stats for annotations
    split_min <- min(sim_results$total_splits)
    split_max <- max(sim_results$total_splits)
    lump_min <- min(sim_results$total_lumped_cols)
    lump_max <- max(sim_results$total_lumped_cols)
    n_orig <- sim_results$n_original_cols
    
    # Calculate IQR per method (combining split and lump) for the Bias plot
    iqr_per_method <- data.frame(
      Method = factor(
        c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count"),
        levels = c("TOT+1", "TOT+1+√count", "Mean Prestige+1", "Mean Prestige+1+√count")
      ),
      iqr_low = c(
        quantile(c(sim_results$ginis_split_TOT, sim_results$ginis_lump_TOT), 0.25),
        quantile(c(sim_results$ginis_split_TOT_sqrt, sim_results$ginis_lump_TOT_sqrt), 0.25),
        quantile(c(sim_results$ginis_split_prestige, sim_results$ginis_lump_prestige), 0.25),
        quantile(c(sim_results$ginis_split_prestige_sqrt, sim_results$ginis_lump_prestige_sqrt), 0.25)
      ),
      iqr_high = c(
        quantile(c(sim_results$ginis_split_TOT, sim_results$ginis_lump_TOT), 0.75),
        quantile(c(sim_results$ginis_split_TOT_sqrt, sim_results$ginis_lump_TOT_sqrt), 0.75),
        quantile(c(sim_results$ginis_split_prestige, sim_results$ginis_lump_prestige), 0.75),
        quantile(c(sim_results$ginis_split_prestige_sqrt, sim_results$ginis_lump_prestige_sqrt), 0.75)
      ),
      # IQR of absolute Gini (Gini × mean) across all simulations
      abs_gini_iqr_low = c(
        quantile(c(sim_results$ginis_split_TOT * sim_results$means_split_TOT,
                   sim_results$ginis_lump_TOT * sim_results$means_lump_TOT), 0.25),
        quantile(c(sim_results$ginis_split_TOT_sqrt * sim_results$means_split_TOT_sqrt,
                   sim_results$ginis_lump_TOT_sqrt * sim_results$means_lump_TOT_sqrt), 0.25),
        quantile(c(sim_results$ginis_split_prestige * sim_results$means_split_prestige,
                   sim_results$ginis_lump_prestige * sim_results$means_lump_prestige), 0.25),
        quantile(c(sim_results$ginis_split_prestige_sqrt * sim_results$means_split_prestige_sqrt,
                   sim_results$ginis_lump_prestige_sqrt * sim_results$means_lump_prestige_sqrt), 0.25)
      ),
      abs_gini_iqr_high = c(
        quantile(c(sim_results$ginis_split_TOT * sim_results$means_split_TOT,
                   sim_results$ginis_lump_TOT * sim_results$means_lump_TOT), 0.75),
        quantile(c(sim_results$ginis_split_TOT_sqrt * sim_results$means_split_TOT_sqrt,
                   sim_results$ginis_lump_TOT_sqrt * sim_results$means_lump_TOT_sqrt), 0.75),
        quantile(c(sim_results$ginis_split_prestige * sim_results$means_split_prestige,
                   sim_results$ginis_lump_prestige * sim_results$means_lump_prestige), 0.75),
        quantile(c(sim_results$ginis_split_prestige_sqrt * sim_results$means_split_prestige_sqrt,
                   sim_results$ginis_lump_prestige_sqrt * sim_results$means_lump_prestige_sqrt), 0.75)
      ),
      # Original absolute Gini for reference
      orig_abs_gini = c(
        sim_results$orig_gini_TOT * sim_results$orig_mean_TOT,
        sim_results$orig_gini_TOT_sqrt * sim_results$orig_mean_TOT_sqrt,
        sim_results$orig_gini_prestige * sim_results$orig_mean_prestige,
        sim_results$orig_gini_prestige_sqrt * sim_results$orig_mean_prestige_sqrt
      )
    )
    
    subtitle_text <- if (n_orig == 1) {
      paste0("Original: 1 column (split only, no lumping possible) | Black = original Gini")
    } else {
      paste0(
        "Original: ", n_orig, " columns | ",
        "Black = original Gini | ",
        "Gray dashed = IQR (middle 50%)"
      )
    }
    
    # Use jittered points with per-method CI
    bias_plot <- ggplot(hist_data, aes(x = Gini, y = 0, color = Direction, 
                                        text = paste0("Gini: ", round(Gini, 3), 
                                                      "<br>Columns: ", Columns,
                                                      "<br>Direction: ", Direction))) +
      geom_jitter(height = 0.4, alpha = 0.5, size = 1.5) +
      geom_vline(data = orig_lines, aes(xintercept = orig_gini), 
                 color = "black", linetype = "solid", linewidth = 1) +
      geom_vline(data = iqr_per_method, aes(xintercept = iqr_low), 
                 color = "darkgray", linetype = "dashed", linewidth = 0.8) +
      geom_vline(data = iqr_per_method, aes(xintercept = iqr_high), 
                 color = "darkgray", linetype = "dashed", linewidth = 0.8) +
      #geom_label(data = iqr_per_method, 
      #          aes(x = 0.02, y = 0.4, 
      #              label = paste0("IQR: [", round(iqr_low, 3), " - ", round(iqr_high, 3), "]\n",
      #                             "Abs G IQR: [", round(abs_gini_iqr_low, 2), " - ", round(abs_gini_iqr_high, 2), "]\n",
      #                             "(orig Abs G: ", round(orig_abs_gini, 2), ")")),
      #          inherit.aes = FALSE,
      #          hjust = 0, vjust = 1, size = 2.5, 
      #          fill = "white", alpha = 0.8, label.size = 0) +
      scale_color_manual(values = c("Split" = "steelblue", "Lump" = "coral")) +
      facet_wrap(~Method, ncol = 2) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(limits = c(-0.5, 0.5), breaks = NULL) +
      labs(
        #title = paste0("Category Bias Simulation (", sim_results$n_sims, " iterations)"), # added in plotly instead for better placement
        #subtitle = subtitle_text,
        x = "Simulated Gini",
        y = NULL,
        color = "Direction"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    # Helper function to wrap text with <br> at specified width
    wrap_text <- function(text, max_width = 55) {
      if (nchar(text) <= max_width) return(text)
      
      words <- strsplit(text, ", ")[[1]]
      lines <- c()
      current_line <- ""
      
      for (word in words) {
        test_line <- if (current_line == "") word else paste0(current_line, ", ", word)
        if (nchar(test_line) > max_width && current_line != "") {
          lines <- c(lines, current_line)
          current_line <- word
        } else {
          current_line <- test_line
        }
      }
      if (current_line != "") lines <- c(lines, current_line)
      
      paste(lines, collapse = ",<br>")
    }
    
    
    # Identify columns that can actually split (potential >= 2)
    split_potential <- pmin(sim_results$adaptive_splits, sim_results$max_counts)
    eligible_idx <- which(split_potential >= 2)
    
    if (length(eligible_idx) == 0) {
      top_split_text <- "Splits: none (no columns eligible to split)"
    } else {
      ordered_eligible <- eligible_idx[order(split_potential[eligible_idx], decreasing = TRUE)]
      split_info <- paste0(
        sim_results$col_names[ordered_eligible], 
        " (", split_potential[ordered_eligible], ")"
      )
      top_split_text <- paste0("Eligible to split (", length(eligible_idx), "): ", 
                               wrap_text(paste(split_info, collapse = ", ")))
    }

    # Identify unique matched strings used for lumping
    unique_strings <- unique(sim_results$matched_strings[!is.na(sim_results$matched_strings)])
    if (length(unique_strings) > 0) {
      lump_strings_text <- paste0("Matched: ", wrap_text(paste(unique_strings, collapse = ", ")))
    } else {
      lump_strings_text <- "No shared strings found"
    }
    
    bias_plotly <- ggplotly(bias_plot, tooltip = "text") %>%
      layout(
        margin = list(t = 160, b = 100),
        legend = list(orientation = "h", x = 0.3, y = -0.15),
        annotations = list(
        # Title
        list(
          x = 0.5, y = 1.45,
          xref = "paper", yref = "paper",
          text = paste0("<b>Category Bias Simulation — ", culture(), " — ", format_dates(startYear(), endYear()), "</b><br>",
                        "<span style='font-size:11px;color:gray'>", sim_results$n_sims, " iterations | ", subtitle_text, "</span>"),
          showarrow = FALSE, font = list(size = 14), align = "center", xanchor = "center"
        ),
        # Lump info - left
        list(
          x = 0.02, y = 1.25,
          xref = "paper", yref = "paper",
          text = paste0("<b>Lump:</b> ", n_orig, " cols → ", lump_min, "-", lump_max, " cols (", sim_results$n_lump_groups, " groups)<br>",
                        "<span style='font-size:9px'>", lump_strings_text, "</span>"),
          showarrow = FALSE, font = list(size = 10, color = "coral"), align = "left", xanchor = "left"
        ),
        # Split info - right
        list(
          x = 0.98, y = 1.25,
          xref = "paper", yref = "paper",
          text = paste0("<b>Split:</b> ", n_orig, " cols → ", split_min, "-", split_max, " cols<br>",
                        "<span style='font-size:9px'>", top_split_text, "</span>"),
          showarrow = FALSE, font = list(size = 10, color = "steelblue"), align = "right", xanchor = "right"
        ),
        # IQR annotations for each facet (2x2 grid) - using paper coordinates
        # Top-left: TOT+1
        list(
          x = 0.01, y = 0.99,
          xref = "paper", yref = "paper",
          text = paste0("IQR: [", round(iqr_per_method$iqr_low[1], 3), "-", round(iqr_per_method$iqr_high[1], 3), "]<br>",
                        "Abs G: [", round(iqr_per_method$abs_gini_iqr_low[1], 1), "-", round(iqr_per_method$abs_gini_iqr_high[1], 1), "]",
                        " (orig: ", round(iqr_per_method$orig_abs_gini[1], 1), ")"),
          showarrow = FALSE, font = list(size = 9), align = "left", xanchor = "left"
        ),
        # Top-right: TOT+1+√count
        list(
          x = 0.51, y = 0.99,
          xref = "paper", yref = "paper",
          text = paste0("IQR: [", round(iqr_per_method$iqr_low[2], 3), "-", round(iqr_per_method$iqr_high[2], 3), "]<br>",
                        "Abs G: [", round(iqr_per_method$abs_gini_iqr_low[2], 1), "-", round(iqr_per_method$abs_gini_iqr_high[2], 1), "]",
                        " (orig: ", round(iqr_per_method$orig_abs_gini[2], 1), ")"),
          showarrow = FALSE, font = list(size = 9), align = "left", xanchor = "left"
        ),
        # Bottom-left: Mean Prestige+1
        list(
          x = 0.01, y = 0.49,
          xref = "paper", yref = "paper",
          text = paste0("IQR: [", round(iqr_per_method$iqr_low[3], 3), "-", round(iqr_per_method$iqr_high[3], 3), "]<br>",
                        "Abs G: [", round(iqr_per_method$abs_gini_iqr_low[3], 1), "-", round(iqr_per_method$abs_gini_iqr_high[3], 1), "]",
                        " (orig: ", round(iqr_per_method$orig_abs_gini[3], 1), ")"),
          showarrow = FALSE, font = list(size = 9), align = "left", xanchor = "left"
        ),
        # Bottom-right: Mean Prestige+1+√count
        list(
          x = 0.51, y = 0.49,
          xref = "paper", yref = "paper",
          text = paste0("IQR: [", round(iqr_per_method$iqr_low[4], 3), "-", round(iqr_per_method$iqr_high[4], 3), "]<br>",
                        "Abs G: [", round(iqr_per_method$abs_gini_iqr_low[4], 1), "-", round(iqr_per_method$abs_gini_iqr_high[4], 1), "]",
                        " (orig: ", round(iqr_per_method$orig_abs_gini[4], 1), ")"),
          showarrow = FALSE, font = list(size = 9), align = "left", xanchor = "left"
        )
       )
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "category_bias_simulation",
          width = 900,
          height = 600,
          scale = 2
        )
      )
    
    incProgress(0.95, detail = "Rendering")
    
    # Add documentation link above plot
    updatePlotUI(
      tags$p(
        style = "margin-bottom: 5px; font-size: 0.9em;",
        tags$a(
          href = "category_bias.html",
          target = "_blank",
          "📖 What does this simulation mean? (Documentation)"
        )
      )
    )

    addInteractivePlot(bias_plotly, height = "600px", width = "100%")
    categoryBiasResults(sim_results)
    storedBiasPlot(bias_plotly)

    # Store IQR for the main method (Mean Prestige+1+√count)
    main_iqr <- c(
      quantile(c(sim_results$ginis_split_prestige_sqrt, sim_results$ginis_lump_prestige_sqrt), 0.25),
      quantile(c(sim_results$ginis_split_prestige_sqrt, sim_results$ginis_lump_prestige_sqrt), 0.75)
    )
    storedBiasIQR(main_iqr)
    
    showNotification(
      paste0("Simulation complete. Lumpable groups found: ", sim_results$n_lump_groups),
      type = "message",
      duration = 8
    )
    
    shinyjs::enable("addResult")
    incProgress(1)
  })
})


#### Trend Plot using drawn map bounds ####
    #### Trend Plot using drawn map bounds ####
    makeTrendPlot <- function(bounds, timeRange) {
      safe_log(paste("makeTrendPlot called with timeRange:", timeRange[1], "to", timeRange[2]))
      
      lats    <- sapply(bounds, `[[`, 2)
      lngs    <- sapply(bounds, `[[`, 1)
      min_lat <- min(lats); max_lat <- max(lats)
      min_lng <- min(lngs); max_lng <- max(lngs)
      
      min_year <- timeRange[1]
      max_year <- timeRange[2]
      
      # Define data type categories
      count_types <- c("grave goods/features (counts)", "hoards (counts)", "house objects (counts)")
      physical_types <- c("house size (m2)", "storage area (m2)", "mound area (m2)", "mound volume (m3)", "grave pit depth (cm)")
      
      # Color palettes - greens for counts, purples for physical
      green_colors <- c("#238b45", "#41ab5d", "#74c476")  # dark to light green
      purple_colors <- c("#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb")  # dark to light purple
      
      grave_query <- sprintf(
        "SELECT start_year, end_year, mean_x_gini, gini, mean_wealth, median_wealth, gini_subadult, mean_x_gini_subadult, culture, contributor_name, data_type, num_sites, area_or_distance, area_unit, bias_iqr_low, bias_iqr_high, 'count' as category
         FROM results
         WHERE lat BETWEEN %f AND %f AND lng BETWEEN %f AND %f
           AND start_year IS NOT NULL AND end_year IS NOT NULL
           AND ((start_year <= %d AND end_year >= %d) 
                OR (start_year >= %d AND start_year <= %d)
                OR (end_year >= %d AND end_year <= %d))",
        min_lat, max_lat, min_lng, max_lng,
        max_year, min_year, min_year, max_year, min_year, max_year
      )
      
      house_query <- sprintf(
        "SELECT start_year, end_year, mean_x_gini, gini, mean_house_size as mean_wealth, median_house_size as median_wealth, NULL as gini_subadult, NULL as mean_x_gini_subadult, culture, contributor_name, data_type, num_sites, area_or_distance, area_unit, 'physical' as category
         FROM house_results
         WHERE lat BETWEEN %f AND %f AND lng BETWEEN %f AND %f
           AND start_year IS NOT NULL AND end_year IS NOT NULL
           AND ((start_year <= %d AND end_year >= %d) 
                OR (start_year >= %d AND start_year <= %d)
                OR (end_year >= %d AND end_year <= %d))",
        min_lat, max_lat, min_lng, max_lng,
        max_year, min_year, min_year, max_year, min_year, max_year
      )
      
      grave_results <- tryCatch({ dbGetQuery(db, grave_query) }, error = function(e) {
        safe_log(paste("Error in grave_query:", e$message))
        data.frame()
      })
      house_results <- tryCatch({ dbGetQuery(db, house_query) }, error = function(e) {
        safe_log(paste("Error in house_query:", e$message))
        data.frame()
      })
      
      # Add placeholder columns for house_results (houses don't have category bias)
      if (nrow(house_results) > 0) {
        house_results$bias_iqr_low <- NA
        house_results$bias_iqr_high <- NA
      }

      safe_log(paste("Query results - graves:", nrow(grave_results), "houses:", nrow(house_results)))
      
      
      # Combine results
      all_results <- rbind(grave_results, house_results)
      
      # Pre-compute bias IQR text for tooltips (handles NULLs safely)
      all_results$bias_iqr_text <- ifelse(
        !is.na(all_results$bias_iqr_low) & !is.na(all_results$bias_iqr_high),
        paste0("<br>Bias IQR: [", round(all_results$bias_iqr_low, 3), " - ", round(all_results$bias_iqr_high, 3), "]"),
        ""
      )
      
      if (nrow(all_results) == 0) {
        return(plot_ly() %>% layout(
          title = "No data in selected area and time range",
          annotations = list(list(x = 0.5, y = 0.5, text = "No data points found.\nTry expanding the area or time range.",
                                  showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 14)))
        ))
      }
      
      # Fill NA data_type with defaults
      all_results$data_type[is.na(all_results$data_type) & all_results$category == "count"] <- "grave goods/features (counts)"
      all_results$data_type[is.na(all_results$data_type) & all_results$category == "physical"] <- "house size (m2)"
      
      all_results$midpoint <- (all_results$start_year + all_results$end_year) / 2
      
      # Normalize function
      norm_range <- function(x, rng) {
        if (length(x) == 0) return(x)
        if (rng[1] == rng[2]) return(rep(0.5, length(x)))
        (x - rng[1]) / (rng[2] - rng[1])
      }
      
      # Normalize within each data_type (adults and subadults together)
      all_results <- all_results %>%
        group_by(data_type) %>%
        mutate(
          # Combined range of adult and subadult absolute Gini
          combined_abs_range_min = min(c(mean_x_gini, mean_x_gini_subadult), na.rm = TRUE),
          combined_abs_range_max = max(c(mean_x_gini, mean_x_gini_subadult), na.rm = TRUE),
          # Normalize both using combined range
          norm_abs = if(n() > 1) norm_range(mean_x_gini, c(combined_abs_range_min[1], combined_abs_range_max[1])) else 0.5,
          norm_abs_subadult = if(n() > 1 & any(!is.na(mean_x_gini_subadult))) norm_range(mean_x_gini_subadult, c(combined_abs_range_min[1], combined_abs_range_max[1])) else NA,
          norm_mean = if(n() > 1) norm_range(mean_wealth, range(mean_wealth, na.rm = TRUE)) else 0.5,
          norm_median = if(n() > 1) norm_range(median_wealth, range(median_wealth, na.rm = TRUE)) else 0.5,
          # Calculate and normalize absolute Gini IQR bounds
          abs_iqr_low = bias_iqr_low * mean_wealth,
          abs_iqr_high = bias_iqr_high * mean_wealth,
          norm_abs_iqr_low = if(n() > 1 & any(!is.na(abs_iqr_low))) norm_range(abs_iqr_low, c(combined_abs_range_min[1], combined_abs_range_max[1])) else NA,
          norm_abs_iqr_high = if(n() > 1 & any(!is.na(abs_iqr_high))) norm_range(abs_iqr_high, c(combined_abs_range_min[1], combined_abs_range_max[1])) else NA
        ) %>%
        select(-combined_abs_range_min, -combined_abs_range_max) %>%
        ungroup()
      
      # Filter subadult data for plotting
      subadult_data <- all_results %>%
        filter(!is.na(gini_subadult) & gini_subadult > 0 & !is.na(norm_abs_subadult))
      
      # Get unique data types present in the data
      present_data_types <- unique(all_results$data_type)
      present_count_types <- intersect(present_data_types, count_types)
      present_physical_types <- intersect(present_data_types, physical_types)
      
      safe_log(paste("Present count types:", paste(present_count_types, collapse = ", ")))
      safe_log(paste("Present physical types:", paste(present_physical_types, collapse = ", ")))
      
      # Start building plot
      p <- plot_ly()
      
      # --- COUNT-BASED DATA TYPES (greens) ---
      for (i in seq_along(present_count_types)) {
        dt <- present_count_types[i]
        dt_data <- all_results[all_results$data_type == dt, ]
        color <- green_colors[min(i, length(green_colors))]
        
        if (nrow(dt_data) > 1) {
          # Aggregate by midpoint
          dt_agg <- dt_data %>%
            group_by(midpoint) %>%
            summarise(
              median_abs = median(norm_abs, na.rm = TRUE),
              median_gini = median(gini, na.rm = TRUE),
              median_gini_x_mean = median(mean_x_gini, na.rm = TRUE),
              median_mean_wealth = median(mean_wealth, na.rm = TRUE),
              median_median_wealth = median(median_wealth, na.rm = TRUE),
              .groups = "drop"
            )
          
          # Absolute Gini IQR ribbon - VISIBLE by default
          # Filter to rows with IQR data
          dt_data_with_iqr <- dt_data %>% filter(!is.na(norm_abs_iqr_low) & !is.na(norm_abs_iqr_high))
          if (nrow(dt_data_with_iqr) > 0) {
            dt_iqr_agg <- dt_data_with_iqr %>%
              group_by(midpoint) %>%
              summarise(
                median_iqr_low = median(norm_abs_iqr_low, na.rm = TRUE),
                median_iqr_high = median(norm_abs_iqr_high, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              arrange(midpoint)
            
            # Add ribbon as filled area
            p <- p %>% add_trace(
              x = c(dt_iqr_agg$midpoint, rev(dt_iqr_agg$midpoint)),
              y = c(dt_iqr_agg$median_iqr_low, rev(dt_iqr_agg$median_iqr_high)),
              type = 'scatter', mode = 'none',
              fill = 'toself',
              fillcolor = paste0(color, "33"),  # 20% opacity
              line = list(color = 'transparent'),
              legendgroup = dt, showlegend = FALSE, visible = TRUE,
              hoverinfo = 'skip',
              name = paste0(dt, " IQR")
            )
          }
          
          # Absolute Gini line - VISIBLE by default
          p <- p %>% add_lines(
            x = dt_agg$midpoint, y = dt_agg$median_abs, 
            name = paste0(dt, " (Abs)"),
            line = list(color = color, dash = "solid"),
            legendgroup = dt, visible = TRUE,
            hovertemplate = paste0(dt, "<br>Year: %{x}<br>Absolute Gini: ", 
                                   round(dt_agg$median_gini_x_mean, 2),
                                   "<br>Mean/Median: ", round(dt_agg$median_mean_wealth, 2), "/", round(dt_agg$median_median_wealth, 2),
                                   "<br>Normalized: %{y:.2f}<extra></extra>")
          )
          
          # Scatter points
          p <- p %>% add_trace(
            data = dt_data, x = ~midpoint, y = ~norm_abs, 
            type = 'scatter', mode = 'markers',
            hoverinfo = 'text', 
            text = ~paste("Culture:", culture, "<br>Data type:", data_type,
                          "<br>Gini:", round(gini, 2),
                          "<br>Absolute Gini:", round(mean_x_gini, 2),
                          bias_iqr_text,
                          "<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2),
                          "<br>Normalized:", round(norm_abs, 2),
                          "<br>Year:", midpoint),
            marker = list(size = 6, color = color),
            legendgroup = dt, showlegend = FALSE, visible = TRUE,
            name = paste0(dt, " points")
          )
          
# Relative Gini IQR ribbon - HIDDEN by default
          dt_data_with_rel_iqr <- dt_data %>% filter(!is.na(bias_iqr_low) & !is.na(bias_iqr_high))
          if (nrow(dt_data_with_rel_iqr) > 0) {
            dt_rel_iqr_agg <- dt_data_with_rel_iqr %>%
              group_by(midpoint) %>%
              summarise(
                median_rel_iqr_low = median(bias_iqr_low, na.rm = TRUE),
                median_rel_iqr_high = median(bias_iqr_high, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              arrange(midpoint)
            
            # Add ribbon as filled area
            p <- p %>% add_trace(
              x = c(dt_rel_iqr_agg$midpoint, rev(dt_rel_iqr_agg$midpoint)),
              y = c(dt_rel_iqr_agg$median_rel_iqr_low, rev(dt_rel_iqr_agg$median_rel_iqr_high)),
              type = 'scatter', mode = 'none',
              fill = 'toself',
              fillcolor = paste0(color, "33"),  # 20% opacity
              line = list(color = 'transparent'),
              legendgroup = paste0(dt, "_rel"), showlegend = FALSE, visible = "legendonly",
              hoverinfo = 'skip',
              name = paste0(dt, " Rel IQR")
            )
          }
          
          # Relative Gini line - HIDDEN by default
          p <- p %>% add_lines(
            x = dt_agg$midpoint, y = dt_agg$median_gini, 
            name = paste0(dt, " (Rel)"),
            line = list(color = color, dash = "dash"),
            legendgroup = paste0(dt, "_rel"), visible = "legendonly",
            hovertemplate = paste0(dt, " (relative)<br>Year: %{x}<br>Gini: %{y:.2f}<extra></extra>")
          )
          
          # --- SUBADULT traces (red, hidden by default) ---
          dt_subadult <- subadult_data[subadult_data$data_type == dt, ]
          
          if (nrow(dt_subadult) > 1) {
            # Aggregate subadult data by midpoint
            dt_subadult_agg <- dt_subadult %>%
              group_by(midpoint) %>%
              summarise(
                median_abs_subadult = median(norm_abs_subadult, na.rm = TRUE),
                median_gini_subadult = median(gini_subadult, na.rm = TRUE),
                median_gini_x_mean_subadult = median(mean_x_gini_subadult, na.rm = TRUE),
                .groups = "drop"
              )
            
            # Subadult Absolute Gini line - HIDDEN by default
            p <- p %>% add_lines(
              x = dt_subadult_agg$midpoint, y = dt_subadult_agg$median_abs_subadult, 
              name = paste0(dt, " Subadult (Abs)"),
              line = list(color = "#e31a1c", dash = "solid", width = 1.5),
              legendgroup = paste0(dt, "_subadult"), visible = "legendonly",
              hovertemplate = paste0(dt, " Subadult<br>Year: %{x}<br>Absolute Gini: ", 
                                     round(dt_subadult_agg$median_gini_x_mean_subadult, 2),
                                     "<br>Normalized: %{y:.2f}<extra></extra>")
            )
            
            # Subadult scatter points - HIDDEN by default
            p <- p %>% add_trace(
              data = dt_subadult, x = ~midpoint, y = ~norm_abs_subadult, 
              type = 'scatter', mode = 'markers',
              hoverinfo = 'text', 
              text = ~paste("Culture:", culture, "<br>Data type:", data_type, "(Subadult)",
                            "<br>Subadult Gini:", round(gini_subadult, 2),
                            "<br>Subadult Abs Gini:", round(mean_x_gini_subadult, 2),
                            #"<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2)),
                            "<br>Normalized:", round(norm_abs_subadult, 2),
                            "<br>Year:", midpoint),
              marker = list(size = 5, color = "#e31a1c", symbol = "diamond"),
              legendgroup = paste0(dt, "_subadult"), showlegend = FALSE, visible = "legendonly",
              name = paste0(dt, " Subadult points")
            )
            
            # Subadult Relative Gini line - HIDDEN by default
            p <- p %>% add_lines(
              x = dt_subadult_agg$midpoint, y = dt_subadult_agg$median_gini_subadult, 
              name = paste0(dt, " Subadult (Rel)"),
              line = list(color = "#e31a1c", dash = "dash", width = 1.5),
              legendgroup = paste0(dt, "_subadult_rel"), visible = "legendonly",
              hovertemplate = paste0(dt, " Subadult (relative)<br>Year: %{x}<br>Gini: %{y:.2f}<extra></extra>")
            )
            
          } else if (nrow(dt_subadult) == 1) {
            # Single subadult point
            p <- p %>% add_trace(
              data = dt_subadult, x = ~midpoint, y = 0.5, 
              type = 'scatter', mode = 'markers',
              hoverinfo = 'text',
              text = ~paste("Culture:", culture, "<br>Data type:", data_type, "(Subadult)",
                            "<br>Subadult Abs Gini:", round(mean_x_gini_subadult, 2)),
                            #"<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2)),
              marker = list(size = 7, color = "#e31a1c", symbol = "diamond"),
              legendgroup = paste0(dt, "_subadult"), visible = "legendonly",
              showlegend = TRUE, name = paste0(dt, " Subadult")
            )
          }
          
        } else if (nrow(dt_data) == 1) {
          # Single point
          p <- p %>% add_trace(
            data = dt_data, x = ~midpoint, y = 0.5, 
            type = 'scatter', mode = 'markers',
            hoverinfo = 'text',
            text = ~paste("Culture:", culture, "<br>Data type:", data_type,
                           "<br>Gini:", round(gini, 2),
                          "<br>Absolute Gini:", round(mean_x_gini, 2),
                          "<br>Normalized Abs Gini:", round(norm_abs, 2),
                          "<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2)),
            marker = list(size = 8, color = color),
            showlegend = TRUE, name = dt
          )
        }
      }
      
      # --- PHYSICAL MEASURE DATA TYPES (purples) ---
      for (i in seq_along(present_physical_types)) {
        dt <- present_physical_types[i]
        dt_data <- all_results[all_results$data_type == dt, ]
        color <- purple_colors[min(i, length(purple_colors))]
        
        if (nrow(dt_data) > 1) {
          # Aggregate by midpoint
          dt_agg <- dt_data %>%
            group_by(midpoint) %>%
            summarise(
              median_abs = median(norm_abs, na.rm = TRUE),
              median_gini = median(gini, na.rm = TRUE),
              median_gini_x_mean = median(mean_x_gini, na.rm = TRUE),
              median_mean_wealth = median(mean_wealth, na.rm = TRUE),
              median_median_wealth = median(median_wealth, na.rm = TRUE),
              .groups = "drop"
            )
          
          # Absolute Gini line - VISIBLE by default
          p <- p %>% add_lines(
            x = dt_agg$midpoint, y = dt_agg$median_abs, 
            name = paste0(dt, " (Abs)"),
            line = list(color = color, dash = "solid"),
            legendgroup = dt, visible = TRUE,
            hovertemplate = paste0(dt, "<br>Year: %{x}<br>Absolute Gini: ", 
                                   round(dt_agg$median_gini_x_mean, 2),
                                   "<br>Normalized: %{y:.2f}<extra></extra>")
          )
          
          # Scatter points
          p <- p %>% add_trace(
            data = dt_data, x = ~midpoint, y = ~norm_abs, 
            type = 'scatter', mode = 'markers',
            hoverinfo = 'text', 
            text = ~paste("Culture:", culture, "<br>Data type:", data_type,
                          "<br>Gini:", round(gini, 2),
                          "<br>Absolute Gini:", round(mean_x_gini, 2),
                          "<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2),
                          "<br>Normalized:", round(norm_abs, 2),
                          "<br>Year:", midpoint),
            marker = list(size = 6, color = color),
            legendgroup = dt, showlegend = FALSE, visible = TRUE,
            name = paste0(dt, " points")
          )
          
          # Relative Gini line - HIDDEN by default
          p <- p %>% add_lines(
            x = dt_agg$midpoint, y = dt_agg$median_gini, 
            name = paste0(dt, " (Rel)"),
            line = list(color = color, dash = "dash"),
            legendgroup = paste0(dt, "_rel"), visible = "legendonly",
            hovertemplate = paste0(dt, " (relative)<br>Year: %{x}<br>Gini: %{y:.2f}<extra></extra>")
          )
          
        } else if (nrow(dt_data) == 1) {
          # Single point
          p <- p %>% add_trace(
            data = dt_data, x = ~midpoint, y = 0.5, 
            type = 'scatter', mode = 'markers',
            hoverinfo = 'text',
            text = ~paste("Culture:", culture, "<br>Data type:", data_type,
                          "<br>Gini:", round(gini, 2),
                          "<br>Absolute Gini:", round(mean_x_gini, 2),
                          "<br>Mean/Median:", round(mean_wealth, 2), "/", round(median_wealth, 2)),
            marker = list(size = 8, color = color),
            showlegend = TRUE, name = dt
          )
        }
      }
      
      # --- CAI CALCULATION ---
      # Uses geometric mean of all available Ginis within 300-year window
      # --- CAI CALCULATION ---
      # Uses geometric mean of all available Ginis within 300-year bins
      calculate_CAI <- function(all_data, min_year, max_year) {
        BIN_WIDTH <- 200
        
        # Create fixed bins from min to max year
        bin_starts <- seq(floor(min_year / BIN_WIDTH) * BIN_WIDTH, 
                          max_year, 
                          by = BIN_WIDTH)
        
        cai_results <- lapply(bin_starts, function(bin_start) {
          bin_end <- bin_start + BIN_WIDTH
          bin_mid <- bin_start + BIN_WIDTH / 2
          
          # Find all data points whose midpoint falls in this bin
          in_bin <- all_data[all_data$midpoint >= bin_start & all_data$midpoint < bin_end, ]
          
          if (nrow(in_bin) < 2) return(NULL)
          
          # Get Ginis from different categories
          count_ginis <- in_bin$gini[in_bin$category == "count" & !is.na(in_bin$gini)]
          physical_ginis <- in_bin$gini[in_bin$category == "physical" & !is.na(in_bin$gini)]
          
          # Need at least one from each category for CAI
          if (length(count_ginis) == 0 || length(physical_ginis) == 0) return(NULL)
          
          # Geometric mean of all available Ginis
          all_ginis <- c(count_ginis, physical_ginis)
          all_ginis <- all_ginis[all_ginis > 0]
          if (length(all_ginis) == 0) return(NULL)
          
          cai <- exp(mean(log(all_ginis)))
          
          data.frame(
            midpoint = bin_mid,
            CAI = cai,
            n_count = length(count_ginis),
            n_physical = length(physical_ginis),
            bin_range = paste0(bin_start, " to ", bin_end)
          )
        })
        
        cai_df <- do.call(rbind, cai_results)
        if (is.null(cai_df) || nrow(cai_df) < 1) return(NULL)
        cai_df
      }
      
      cai_data <- calculate_CAI(all_results, min_year, max_year)
      
      # Add CAI trace - HIDDEN by default
      if (!is.null(cai_data) && nrow(cai_data) >= 1) {
        p <- p %>% add_lines(
          x = cai_data$midpoint, y = cai_data$CAI, 
          name = "CAI (Composite)",
          line = list(color = "orange", dash = "solid", width = 2.5),
          legendgroup = "cai", visible = "legendonly",
          hovertemplate = paste0("CAI (Geometric Mean)<br>Bin: ", cai_data$bin_range,
                                 "<br>CAI: %{y:.3f}",
                                 "<br>Count sources: ", cai_data$n_count,
                                 "<br>Physical sources: ", cai_data$n_physical,
                                 "<extra></extra>")
        )
        
        p <- p %>% add_trace(
          data = cai_data, x = ~midpoint, y = ~CAI, 
          type = 'scatter', mode = 'markers',
          hoverinfo = 'text', 
          text = ~paste("CAI:", round(CAI, 3), 
                        "\nBin:", bin_range,
                        "\nCount sources:", n_count, 
                        "\nPhysical sources:", n_physical),
          marker = list(size = 8, color = "orange", symbol = "diamond"),
          legendgroup = "cai", showlegend = FALSE, visible = "legendonly",
          name = "CAI Points"
        )
      }
      
      # Layout
      p %>% layout(
        xaxis = list(title = "Year (midpoint)"),
        yaxis = list(title = "Normalized Abs. G (0-1) / Gini", range = c(-0.05, 1.05)),
        legend = list(orientation = "h", x = 0, y = -0.30),
        margin = list(b = 200),
        annotations = list(list(
          x = 0.5, y = 1.10, xref = "paper", yref = "paper",
          text = paste0("Time range: ", min_year, " to ", max_year, " | Data points: ", nrow(all_results),
                        if (!is.null(cai_data) && nrow(cai_data) > 0) paste0(" | CAI points: ", nrow(cai_data)) else ""),
          showarrow = FALSE, font = list(size = 11, color = "gray")
        ))
      )
    }
    
    #Observe only Trend plot button
    observeEvent(input$showTrendPlot, {
      bounds <- drawnBounds()
      if (is.null(bounds)) {
        showModal(
          modalDialog(
            title = "Draw an Area First",
            "Draw a rectangle or polygon on the map to generate the trend plot.",
            easyClose = TRUE,
            footer = modalButton("OK")
          )
        )
      } else {
        removeModal()               # if a "draw" modal is sitting around
        # inject the UI slot
        updatePlotUI(plotlyOutput("inequalityTrendPlot", height = "450px"))
        output$inequalityTrendPlot <- renderPlotly({
          makeTrendPlot(bounds, input$timeSliderFinal)
        })
        trendPlotGenerated(TRUE)    # flag if you still need it elsewhere
      }
    })
    
    #Simple polygon drawing observer
    observeEvent(drawnBounds(), {
      bounds <- drawnBounds()
      if (is.null(bounds)) {
        return()  # nothing to do until the user draws
      }
      # On first draw: inject the UI slot
      if (!trendPlotGenerated()) {
        updatePlotUI(plotlyOutput("inequalityTrendPlot", height = "450px"))
        trendPlotGenerated(TRUE)
      }
      output$inequalityTrendPlot <- renderPlotly({
        # Use timeSliderFinal if available, otherwise fall back to timeSlider
        timeRange <- if (!is.null(input$timeSliderFinal)) input$timeSliderFinal else input$timeSlider
        makeTrendPlot(bounds, timeRange)
      })
      # whenever the user draws/edits the polygon, you might
      #   1) update a status text, or
      #   2) clear the "please draw" modal if it's open
      output$selectedArea <- renderText({
        paste0("Selected area bounds: ", toString(drawnBounds()))
      })
    })
    
    # Update trend plot when time slider changes (if area is already drawn)
    # Update trend plot when time slider is released (not during drag)
    observeEvent(input$timeSliderFinal, {
      bounds <- drawnBounds()
      if (!is.null(bounds) && trendPlotGenerated()) {
        output$inequalityTrendPlot <- renderPlotly({
          makeTrendPlot(bounds, input$timeSliderFinal)
        })
      }
    }, ignoreInit = TRUE)
    
    output$tablesUI <- renderUI({
      tables_list <- list(
        DTOutput("originalTable"),
        DTOutput("updatedTable"),
        DTOutput("sqrtTable")
      )
      
      # Add house table if house data is processed
      if (houseDataUploaded()) {
        tables_list <- c(tables_list, list(DTOutput("houseTable")))
      }
      
      do.call(tagList, tables_list)
    })
    
    output$mymap <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Esri)") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light (CartoDB)") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark (CartoDB)") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic (Esri)") %>%
        addLayersControl(
          baseGroups = c("Satellite (Esri)", "OpenStreetMap", "Light (CartoDB)", "Dark (CartoDB)", 
                         "Topographic (Esri)"),
          overlayGroups = c(
            # Count-based (green)
            "grave goods/features (counts)",
            "hoards (counts)", 
            "house objects (counts)",
            # Physical measures (purple)
            "house size (m2)",
            "storage area (m2)",
            "mound area (m2)",
            "mound volume (m3)",
            "grave pit depth (cm)"
          ),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%

        addDrawToolbar(
          editOptions = editToolbarOptions(
            remove = FALSE, # Enables the trash icon for deleting drawn items
            edit = FALSE # Disables editing of drawn shapes
          ), 
          polygonOptions = drawPolygonOptions(
            showArea = TRUE, 
            shapeOptions = list(color = 'grey', interactive = FALSE)
          ),
          rectangleOptions = drawRectangleOptions(
            showArea = TRUE, 
            shapeOptions = list(color = 'grey', interactive = FALSE)
          ),
          circleOptions = FALSE, # Explicitly disable circles
          markerOptions = FALSE, # Explicitly disable markers
          circleMarkerOptions = FALSE,
          polylineOptions = FALSE, # Explicitly disable polylines
          # Disable both edit *and* remove → no trash can
          singleFeature = TRUE # Don't allow multiple features to be drawn
        ) %>% setView(lng = 8.796, lat = 49.776, zoom = 5) # Set a default view
    })
    
    observeEvent(input$addResult, {
      formSubmitted(FALSE)
      
      # If data has coordinates, require map click to verify location
      if (input$graveHasCoords && !is.null(graveUploadCoords())) {
        graveVerifyingCoords(TRUE)
        addResultsClicked(TRUE)
        coords <- graveUploadCoords()
        showNotification(
          paste0("Please click on the map near your data location to verify coordinates. ",
                 "Data centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
          type = "message", duration = 10
        )
      } else {
        # No coordinates in data - standard map click flow
        graveVerifyingCoords(FALSE)
        addResultsClicked(TRUE)
      }
    })
    
    #House add results observer
    observeEvent(input$addHouseResult, {
      # Validate that house culture and time data are filled out
      if (is.null(input$houseCulture) || input$houseCulture == "" || 
          is.null(input$houseStartYear) || is.null(input$houseEndYear)) {
        showNotification("Please fill out Culture/Period, Start Year, and End Year before adding results to map.", 
                         type = "warning")
        return()
      }
      
      # Validate that house Gini has been calculated
      if (is.null(houseGiniCoeff()) || is.null(meanHouseSize()) || is.null(houseSampleSize())) {
        showNotification("Please calculate Gini first before adding results to map.", 
                         type = "warning")
        return()
      }
      
      houseFormSubmitted(FALSE)
      
            # If data has coordinates, require map click to verify location
      if (input$houseHasCoords && !is.null(houseUploadCoords())) {
        houseVerifyingCoords(TRUE)
        addHouseResultsClicked(TRUE)
        coords <- houseUploadCoords()
        showNotification(
          paste0("Please click on the map near your data location to verify coordinates. ",
                 "Data centroid: ", round(coords$lat, 2), "°, ", round(coords$lng, 2), "°"),
          type = "message", duration = 10
        )
      } else {
        # No coordinates in data - standard map click flow
        houseVerifyingCoords(FALSE)
        addHouseResultsClicked(TRUE)
      }
    })
    
    observeEvent(input$mymap_click, {
      # Handle grave results submission
      if (!formSubmitted() && addResultsClicked()) {
        userClick <- list(lat = input$mymap_click$lat, lng = input$mymap_click$lng)
        
        # Check if we're verifying coordinates from data
        if (graveVerifyingCoords() && !is.null(graveUploadCoords())) {
          dataCoords <- graveUploadCoords()
          
          # Check if coordinates might be flipped
          if (check_coordinates_flipped(dataCoords, userClick)) {
            # Show modal offering to swap
            showModal(modalDialog(
              title = "Possible Coordinate Swap Detected",
              p(paste0("Your data coordinates (", round(dataCoords$lat, 2), "°, ", round(dataCoords$lng, 2), 
                       "°) appear to be far from where you clicked.")),
              p("This often happens when lat and lng columns are swapped in the source data."),
              p(tags$strong("Would you like to swap the lat/lng values?")),
              p(paste0("Swapped coordinates would be: ", round(dataCoords$lng, 2), "°, ", round(dataCoords$lat, 2), "°"),
                style = "color: #666;"),
              footer = tagList(
                #modalButton("Cancel"),
                actionButton("cancelGraveSubmit", "Cancel"),
                actionButton("keepGraveCoords", "Keep Original"),
                actionButton("swapGraveCoords", "Swap Lat/Lng", class = "btn-primary")
              )
            ))
            return()  # Wait for user decision
          }
          
          # Coordinates verified - use data coordinates
          clickPosition$lat <- dataCoords$lat
          clickPosition$lng <- dataCoords$lng
          graveVerifyingCoords(FALSE)
        } else {
          # Using click position directly
          clickPosition$lat <- userClick$lat
          clickPosition$lng <- userClick$lng
        }
        
        # Show submission form
        showModal(modalDialog(
          title = "Add Grave Data to Map",
          if (graveVerifyingCoords() || !is.null(graveUploadCoords())) {
            p(paste0("Using coordinates: ", round(clickPosition$lat, 4), ", ", round(clickPosition$lng, 4)),
              style = "color: #666; font-style: italic;")
          },
          textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
          selectInput("dataType", "Data Type",
              choices = c("grave goods/features (counts)", 
                          #"grave features (counts)",
                          "hoards (counts)", 
                          "house objects (counts)"),
              selected = "grave goods (counts)"),      
          br(),
          h4("Optional File Attachments"),
          checkboxInput("submitGravePlots", "Submit plots and derived table with this submission", value = TRUE),
          checkboxInput("submitGraveData", "Submit input data with this submission", value = FALSE),
          p("Note: Plots and data will be publicly accessible via map links", 
            style = "font-size: 0.8em; color: #666;"),
          footer = tagList(
            #modalButton("Cancel"), 
            actionButton("cancelGraveSubmit", "Cancel"),
            actionButton("submitData", "Submit"))
        ))
      }
      
      # Handle house results submission
      if (!houseFormSubmitted() && addHouseResultsClicked()) {
        userClick <- list(lat = input$mymap_click$lat, lng = input$mymap_click$lng)
        
        # Check if we're verifying coordinates from data
        if (houseVerifyingCoords() && !is.null(houseUploadCoords())) {
          dataCoords <- houseUploadCoords()
          
          # Check if coordinates might be flipped
          if (check_coordinates_flipped(dataCoords, userClick)) {
            # Show modal offering to swap
            showModal(modalDialog(
              title = "Possible Coordinate Swap Detected",
              p(paste0("Your data coordinates (", round(dataCoords$lat, 2), "°, ", round(dataCoords$lng, 2), 
                       "°) appear to be far from where you clicked.")),
              p("This often happens when lat and lng columns are swapped in the source data."),
              p(tags$strong("Would you like to swap the lat/lng values?")),
              p(paste0("Swapped coordinates would be: ", round(dataCoords$lng, 2), "°, ", round(dataCoords$lat, 2), "°"),
                style = "color: #666;"),
              footer = tagList(
                modalButton("cancelHouseSubmit", "Cancel"),
                actionButton("keepHouseCoords", "Keep Original"),
                actionButton("swapHouseCoords", "Swap Lat/Lng", class = "btn-primary")
              )
            ))
            return()  # Wait for user decision
          }
          
          # Coordinates verified - use data coordinates
          houseClickPosition$lat <- dataCoords$lat
          houseClickPosition$lng <- dataCoords$lng
          houseVerifyingCoords(FALSE)
        } else {
          # Using click position directly
          houseClickPosition$lat <- userClick$lat
          houseClickPosition$lng <- userClick$lng
        }
        
        # Show submission form
        showModal(modalDialog(
          title = "Add House Data to Map",
          if (houseVerifyingCoords() || !is.null(houseUploadCoords())) {
            p(paste0("Using coordinates: ", round(houseClickPosition$lat, 4), ", ", round(houseClickPosition$lng, 4)),
              style = "color: #666; font-style: italic;")
          },
          textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
          selectInput("dataType", "Data Type",
              choices = c("house size (m2)",
                          "storage area (m2)", 
                          "mound area (m2)",
                          "mound volume (m3)",
                          "grave pit depth (cm)"),
              selected = "house size (m2)"),
          br(),
          h4("Optional File Attachments"),
          checkboxInput("submitHousePlots", "Submit plots with this submission", value = TRUE),
          checkboxInput("submitHouseData", "Submit input data with this submission", value = FALSE),
          p("Note: Plots and data will be publicly accessible via map links", 
            style = "font-size: 0.8em; color: #666;"),
          footer = tagList(
            modalButton("cancelHouseSubmit", "Cancel"), 
            actionButton("submitHouseDataBtn", "Submit"))
        ))
      }
    })

    # Reset grave submission state on cancel
    observeEvent(input$cancelGraveSubmit, {
      removeModal()
      addResultsClicked(FALSE)
      formSubmitted(FALSE)
    })
    # Reset house submission state on cancel
    observeEvent(input$cancelHouseSubmit, {
      removeModal()
      addHouseResultsClicked(FALSE)
      houseFormSubmitted(FALSE)
    })

    # Handle grave coordinate swap decision
    observeEvent(input$swapGraveCoords, {
      removeModal()
      coords <- graveUploadCoords()
      # Swap the coordinates
      swapped <- list(lat = coords$lng, lng = coords$lat)
      graveUploadCoords(swapped)
      clickPosition$lat <- swapped$lat
      clickPosition$lng <- swapped$lng
      graveVerifyingCoords(FALSE)
      
      showNotification("Coordinates swapped successfully!", type = "message")
      
      # Now show the submission form
      showModal(modalDialog(
        title = "Add Grave Data to Map",
        p(paste0("Using swapped coordinates: ", round(swapped$lat, 4), ", ", round(swapped$lng, 4)),
          style = "color: #666; font-style: italic;"),
        textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
          selectInput("dataType", "Data Type",
              choices = c("grave goods/features (counts)", 
                          "hoards (counts)", 
                          "house objects (counts)"),
              selected = "grave goods (counts)"),
        br(),
        h4("Optional File Attachments"),
        checkboxInput("submitGravePlots", "Submit plots and derived table with this submission", value = TRUE),
        checkboxInput("submitGraveData", "Submit input data with this submission", value = FALSE),
        p("Note: Plots and data will be publicly accessible via map links", 
          style = "font-size: 0.8em; color: #666;"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("submitData", "Submit"))
      ))
    })
    
    observeEvent(input$keepGraveCoords, {
      removeModal()
      coords <- graveUploadCoords()
      clickPosition$lat <- coords$lat
      clickPosition$lng <- coords$lng
      graveVerifyingCoords(FALSE)
      
      # Show the submission form with original coordinates
      showModal(modalDialog(
        title = "Add Grave Data to Map",
        p(paste0("Using original coordinates: ", round(coords$lat, 4), ", ", round(coords$lng, 4)),
          style = "color: #666; font-style: italic;"),
        textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
        selectInput("dataType", "Data Type",
              choices = c("grave goods/features (counts)", 
                          #"grave features (counts)",
                          "hoards (counts)", 
                          "house objects (counts)"),
              selected = "grave goods (counts)"),
        br(),
        h4("Optional File Attachments"),
        checkboxInput("submitGravePlots", "Submit plots and derived table with this submission", value = TRUE),
        checkboxInput("submitGraveData", "Submit input data with this submission", value = FALSE),
        p("Note: Plots and data will be publicly accessible via map links", 
          style = "font-size: 0.8em; color: #666;"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("submitData", "Submit"))
      ))
    })
    
    # Handle house coordinate swap decision
    observeEvent(input$swapHouseCoords, {
      removeModal()
      coords <- houseUploadCoords()
      # Swap the coordinates
      swapped <- list(lat = coords$lng, lng = coords$lat)
      houseUploadCoords(swapped)
      houseClickPosition$lat <- swapped$lat
      houseClickPosition$lng <- swapped$lng
      houseVerifyingCoords(FALSE)
      
      showNotification("Coordinates swapped successfully!", type = "message")
      
      # Now show the submission form
      showModal(modalDialog(
        title = "Add House Data to Map",
        p(paste0("Using swapped coordinates: ", round(swapped$lat, 4), ", ", round(swapped$lng, 4)),
          style = "color: #666; font-style: italic;"),
        textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
          selectInput("dataType", "Data Type",
              choices = c("house size (m2)",
                          "storage area (m2)", 
                          "mound area (m2)",
                          "mound volume (m3)",
                          "grave pit depth (cm)"),
              selected = "house size (m2)"),
        br(),
        h4("Optional File Attachments"),
        checkboxInput("submitHousePlots", "Submit plots with this submission", value = TRUE),
        checkboxInput("submitHouseData", "Submit input data with this submission", value = FALSE),
        p("Note: Plots and data will be publicly accessible via map links", 
          style = "font-size: 0.8em; color: #666;"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("submitHouseDataBtn", "Submit"))
      ))
    })
    
    observeEvent(input$keepHouseCoords, {
      removeModal()
      coords <- houseUploadCoords()
      houseClickPosition$lat <- coords$lat
      houseClickPosition$lng <- coords$lng
      houseVerifyingCoords(FALSE)
      
      # Show the submission form with original coordinates
      showModal(modalDialog(
        title = "Add House Data to Map",
        p(paste0("Using original coordinates: ", round(coords$lat, 4), ", ", round(coords$lng, 4)),
          style = "color: #666; font-style: italic;"),
        textInput("publicationRef", "Publication Reference(s)", 
            placeholder = "Separate multiple with ;"),
          textInput("publicationDOI", "Publication DOI(s) or ISBN(s)",
            placeholder = "Separate multiple with ;"),
          textInput("contributorName", "Contributor Name"),
          textInput("contributorInstitution", "Contributor Institution"),
          textInput("contactEmail", "Contact Email"),
          textAreaInput("submissionComments", "Comments", 
                placeholder = "Any information about the data, collection methods, etc.",
                rows = 3),
        selectInput("dataType", "Data Type",
              choices = c("house size (m2)",
                          "storage area (m2)", 
                          "mound area (m2)",
                          "mound volume (m3)",
                          "grave pit depth (cm)"),
              selected = "house size (m2)"),
        br(),
        h4("Optional File Attachments"),
        checkboxInput("submitHousePlots", "Submit plots with this submission", value = TRUE),
        checkboxInput("submitHouseData", "Submit input data with this submission", value = FALSE),
        p("Note: Plots and data will be publicly accessible via map links", 
          style = "font-size: 0.8em; color: #666;"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("submitHouseDataBtn", "Submit"))
      ))
    })

    observe({
  if (graveFilesReady()) safe_log("Grave files ready for saving")
  if (houseFilesReady()) safe_log("House files ready for saving")
})

output$fileReadyStatus <- renderText({
  paste("Grave files ready:", graveFilesReady(), 
        "House files ready:", houseFilesReady())
})    
    
  }, ####end of whole server block ####
  
  error = function(e) {
    safe_log(paste("UNCAUGHT ERROR in server():", e$message))
    stop(e)                   # let Shiny show its error page
  })
}

safe_log("Checkpoint: launching shinyApp()")

app <- shinyApp(ui, server)   # create the Shiny app object

# ---- OPTIONAL global guard --------------------------------------------
app <- tryCatch(
  app,
  error = function(e) {
    write(paste("Shiny app crash:", e$message),
          log_path, append = TRUE)
    stop(e)                    # keep the standard error page
  }
)
# -----------------------------------------------------------------------

app                             # <-- this **must** be the last line
