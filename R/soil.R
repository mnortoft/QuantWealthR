# ============================================================================
# Soil preservation scores (Kibblewhite et al. 2015, ESDAC)
# ============================================================================

.soil_citation <- paste0(
  "Soil preservation scores from: Kibblewhite, M., T\u00f3th, G. & Hermann, T. (2015). ",
  "Predicting the preservation of cultural artefacts and buried materials in soil. ",
  "Science of the Total Environment 529, 249-263. https://doi.org/10.1016/j.scitotenv.2015.04.036 ",
  "Data: European Soil Data Centre (ESDAC), European Commission, Joint Research Centre.")

#' Add soil preservation scores to graves
#'
#' Looks up, for every grave, how well the soil at its location preserves
#' buried materials, using the European maps of Kibblewhite, T\enc{ó}{o}th
#' and Hermann (2015). The scores are added as new columns, so you can check
#' whether preservation could explain differences in grave goods, e.g.
#' whether bone and shell ornaments are rarer where bone preserves badly.
#'
#' **The map is not included in QuantWealthR.** It is distributed by the
#' European Soil Data Centre (ESDAC) of the European Commission's Joint
#' Research Centre, which asks users to register before downloading.
#' Download it yourself from the ESDAC page "Maps of preservation capacity
#' of cultural artefacts and buried materials in soils in the EU"
#' (<https://esdac.jrc.ec.europa.eu/content/maps-related-predicting-preservation-cultural-artefacts-and-buried-materials-soils-eu-0>)
#' and give the path to the shapefile (`SF_cultural.shp`). Every call prints
#' the reference to cite; it is also stored in the attribute `"citation"`.
#'
#' **The scores.** Each soil type was assigned to a preservation category
#' for each material: 1 = poor (destroyed within about 100 years), 2 = fair,
#' 3 = good (still present after about 2000 years). A map unit's score is
#' the area-weighted mean of its soil types, so it lies between 1 and 3.
#' **Higher means better preservation.** The layers are:
#' * `bones`: bones, teeth and shells: the relevant one for bone tools,
#'   tooth pendants and shell beads, and for skeletons;
#' * `organics`: other organic materials (wood, textiles, leather), which
#'   survive mainly in waterlogged, oxygen-free soils;
#' * `metals`: copper, bronze and iron;
#' * `strati`: stratigraphic evidence.
#'
#' Map units with the value 0 have no assessment (towns, water bodies, ice);
#' these graves get `NA`, as do graves outside the map.
#'
#' **Caveats.** The map is at 1:1,000,000 scale, so a score describes the
#' typical soils of a map unit several kilometres across, not the soil of
#' the grave itself. Site coordinates are often approximate too. ESDAC notes
#' that these maps were not themselves peer-reviewed as data. All graves of
#' a site share one score, so in statistical tests the site, not the grave,
#' is the unit of observation.
#'
#' @param df A data frame with one row per grave and coordinates in decimal
#'   degrees (WGS84).
#' @param shapefile Path to the downloaded `SF_cultural.shp` (the
#'   accompanying `.shx`, `.dbf` and `.prj` files must be in the same
#'   folder), or the map already read with [sf::st_read()] (faster when you
#'   call this several times).
#' @param layers Which scores to add (default all four:
#'   `c("bones", "organics", "metals", "strati")`).
#' @param lat_col,lng_col Names of the latitude and longitude columns
#'   (default `"lat"`, `"lng"`).
#' @param prefix Prefix of the new column names (default `"soil_"`, giving
#'   e.g. `soil_bones`).
#' @return `df` with one new column per layer (numeric, 1-3, `NA` where there
#'   is no score), in the same row order, with attribute `"citation"`.
#'   Requires the \pkg{sf} package.
#' @examples
#' \dontrun{
#' shp <- "C:/data/soil/SF_cultural.shp"          # your downloaded copy
#' SK  <- add_soil_preservation(SK, shp)
#' summary(SK$soil_bones)
#'
#' # Reading the map once is faster when you annotate several datasets
#' soil <- sf::st_read(shp, quiet = TRUE)
#' SK  <- add_soil_preservation(SK, soil)
#' GAC <- add_soil_preservation(GAC, soil)
#' }
#'
#' # A runnable illustration with a made-up two-polygon map
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   sq <- function(x0, y0) sf::st_polygon(list(cbind(c(x0, x0 + 1, x0 + 1, x0, x0),
#'                                                    c(y0, y0, y0 + 1, y0 + 1, y0))))
#'   toy <- sf::st_sf(bones = c(1.1, 2.4), organics = c(1.3, 1.2),
#'                    metals = c(1.0, 1.6), strati = c(2.1, 2.0),
#'                    geometry = sf::st_sfc(sq(8, 55), sq(14, 50), crs = 4326))
#'   graves <- data.frame(id = 1:3, lat = c(55.5, 50.4, 45), lng = c(8.5, 14.4, 5))
#'   add_soil_preservation(graves, toy)   # the third grave lies outside: NA
#' }
#' @export
add_soil_preservation <- function(df, shapefile,
                                  layers = c("bones", "organics", "metals", "strati"),
                                  lat_col = "lat", lng_col = "lng", prefix = "soil_") {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("add_soil_preservation() needs the 'sf' package: install.packages(\"sf\").")
  layers <- match.arg(layers, c("bones", "organics", "metals", "strati"), several.ok = TRUE)
  for (cl in c(lat_col, lng_col))
    if (!cl %in% names(df)) stop(sprintf("Column '%s' not found in df.", cl))
  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  lng <- suppressWarnings(as.numeric(df[[lng_col]]))
  ok  <- !is.na(lat) & !is.na(lng)
  if (!any(ok)) stop("No rows with numeric coordinates.")

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  pts_ll <- sf::st_as_sf(data.frame(.row = which(ok), lng = lng[ok], lat = lat[ok]),
                         coords = c("lng", "lat"), crs = 4326)

  if (inherits(shapefile, "sf")) {
    soil <- shapefile
    pts  <- sf::st_transform(pts_ll, sf::st_crs(soil))
  } else {
    if (!is.character(shapefile) || length(shapefile) != 1 || !file.exists(shapefile))
      stop("`shapefile` must be the path to SF_cultural.shp (or an sf object). ",
           "Download the map from ESDAC; see ?add_soil_preservation.")
    # Read only the part of the map around the graves (much faster)
    crs  <- sf::st_layers(shapefile)$crs[[1]]
    pts  <- sf::st_transform(pts_ll, crs)
    box  <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(sf::st_as_sfc(sf::st_bbox(pts)), 20000)))
    soil <- sf::st_read(shapefile, quiet = TRUE, wkt_filter = sf::st_as_text(box))
  }
  miss <- setdiff(layers, names(soil))
  if (length(miss)) stop("The map has no column(s) ", paste(miss, collapse = ", "),
                         ". Is this the Kibblewhite et al. (2015) shapefile?")

  j <- suppressMessages(sf::st_join(pts, soil[, layers], join = sf::st_intersects, left = TRUE))
  j <- sf::st_drop_geometry(j)
  j <- j[!duplicated(j$.row), , drop = FALSE]    # a point on a shared border: first polygon

  for (ly in layers) {
    v <- rep(NA_real_, nrow(df))
    v[j$.row] <- as.numeric(j[[ly]])
    v[!is.na(v) & v == 0] <- NA                  # 0 = not assessed (towns, water)
    df[[paste0(prefix, ly)]] <- v
  }
  n_na <- sum(is.na(df[[paste0(prefix, layers[1])]]))
  if (n_na > 0)
    message(n_na, " of ", nrow(df), " rows have no score (no coordinates, outside the map, ",
            "or an unassessed map unit such as a town).")
  message(.soil_citation)
  attr(df, "citation") <- .soil_citation
  df
}
