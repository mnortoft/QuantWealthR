# ============================================================================
# Maps of the cases
# ============================================================================

#' Map the graves or sites of each case
#'
#' Plots where the graves of each case (phase / dataset) lie, using the `lat`
#' and `lng` columns of the data (decimal degrees, WGS84). All cases share
#' one map and are told apart by colour.
#'
#' Two levels of detail:
#' * `level = "site"` (default): graves with the same coordinates (usually
#'   one cemetery or site) are combined into one point per site and case.
#'   Point size shows `size_by`: the number of graves, or a wealth
#'   statistic of the site's graves. This keeps the map readable when many
#'   graves share coordinates.
#' * `level = "grave"`: one point per grave, sized by its wealth. Graves with
#'   identical coordinates are spread out slightly (`jitter`, in degrees), so
#'   their number is visible; the spread is not their real position.
#'
#' Country borders come from the \pkg{maps} package if it is installed
#' (otherwise the points are drawn without a background, with a message).
#' The map is cropped to the points plus a margin. Longitude and latitude are
#' drawn with the correct proportions for the map's latitude
#' ([ggplot2::coord_quickmap()]).
#'
#' Site statistics are only as good as the number of graves behind them: a
#' site Gini needs at least 3 graves (sites with fewer get `NA` and are drawn
#' small and grey when `size_by = "gini"`), and the median of 2 graves says
#' little. The table behind the map is returned as the attribute `"sites"`
#' (or `"graves"`), with the number of graves per site, so you can check.
#'
#' @param pipeline_result A [count_pipeline()] result whose data had `lat`
#'   and `lng` columns.
#' @param level `"site"` (default) or `"grave"`; see Details.
#' @param size_by For `level = "site"`: what point size shows. `"n"` (number
#'   of graves, default), `"median_wealth"`, `"mean_wealth"`,
#'   `"max_wealth"`, `"gini"` or `"empty_share"` (share of graves without
#'   grave goods). Ignored for `level = "grave"` (size is the grave's
#'   wealth).
#' @param cases Optional character vector of case names (keys of
#'   `pipeline_result$by_group`, e.g. `"A: A2"`) to show. Default: all.
#' @param graves `"adults"` (default), `"all"` or `"subadults"`: which
#'   graves to include, as in [plot_gini_comparison()].
#' @param measure `"combined"` (default), `"prestige"` or `"tot"`: the
#'   wealth measure used for sizes and statistics.
#' @param borders Logical. Draw country borders (needs the \pkg{maps}
#'   package; default `TRUE`).
#' @param margin Margin around the points, as a share of their extent
#'   (default 0.15).
#' @param jitter For `level = "grave"`: maximum spread, in degrees, of
#'   graves that share coordinates. Default (`NULL`): 1.5% of the map's
#'   extent, enough to see how many graves a site has. `0` turns it off.
#' @param alpha Point transparency (default 0.7), so overlapping points stay
#'   visible.
#' @param title Optional plot title. The age group is added.
#' @return A [ggplot2::ggplot] object, with attribute `"sites"` (for
#'   `level = "site"`: one row per site and case with `lat`, `lng`, `n` and
#'   all site statistics) or `"graves"` (for `level = "grave"`).
#' @examples
#' ph  <- read_count_data(example_data_path("phases"))
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE", progress = FALSE)
#'
#' # One point per site and phase, sized by the number of graves
#' plot_case_map(res)
#'
#' # Sized by the median wealth of the site's graves; only two phases
#' p <- plot_case_map(res, size_by = "median_wealth", cases = c("A: A3", "B: B3"))
#' p
#' attr(p, "sites")            # the numbers behind the map
#'
#' # Every grave, sized by its wealth
#' plot_case_map(res, level = "grave", cases = c("A: A3", "B: B3"))
#' @export
plot_case_map <- function(pipeline_result,
                          level   = c("site", "grave"),
                          size_by = c("n", "median_wealth", "mean_wealth", "max_wealth",
                                      "gini", "empty_share"),
                          cases   = NULL,
                          graves  = c("adults", "all", "subadults"),
                          measure = c("combined", "prestige", "tot"),
                          borders = TRUE, margin = 0.15, jitter = NULL,
                          alpha = 0.7, title = NULL) {
  level   <- match.arg(level)
  size_by <- match.arg(size_by)
  graves  <- match.arg(graves)
  measure <- match.arg(measure)
  bg <- pipeline_result$by_group
  if (is.null(bg) || length(bg) == 0) stop("pipeline_result has no cases.")
  if (!is.null(cases)) {
    bad <- setdiff(cases, names(bg))
    if (length(bad)) stop("Unknown case(s): ", paste(bad, collapse = ", "),
                          ". Available: ", paste(names(bg), collapse = ", "))
    bg <- bg[cases]
  }
  wcol <- switch(measure, combined = "combined_total", prestige = "prestige_total",
                 tot = "raw_TOT")

  # One row per grave: case, coordinates, wealth, empty or not
  rows <- lapply(names(bg), function(cs) {
    pd <- bg[[cs]]$prestige_df
    if (is.null(pd) || !all(c("lat", "lng") %in% names(pd))) return(NULL)
    keep <- rep(TRUE, nrow(pd))
    if ("is_adult_grave" %in% names(pd)) {
      if (graves == "adults")    keep <- pd$is_adult_grave
      if (graves == "subadults") keep <- pd$is_subadult_only
    }
    pd <- pd[keep, , drop = FALSE]
    if (nrow(pd) == 0) return(NULL)
    w <- as.numeric(pd[[wcol]])
    if (measure == "tot") w <- w + 1
    data.frame(case = cs, lat = suppressWarnings(as.numeric(pd$lat)),
               lng = suppressWarnings(as.numeric(pd$lng)), wealth = w,
               empty = as.numeric(pd$raw_TOT %||% NA) == 0, stringsAsFactors = FALSE)
  })
  g <- do.call(rbind, rows)
  if (is.null(g) || nrow(g) == 0)
    stop("No graves with `lat` and `lng` found. The data given to count_pipeline() ",
         "needs numeric `lat` and `lng` columns (decimal degrees).")
  miss <- is.na(g$lat) | is.na(g$lng)
  if (any(miss)) {
    message(sum(miss), " grave(s) without coordinates are left out.")
    g <- g[!miss, , drop = FALSE]
  }
  g$case <- factor(g$case, levels = names(bg)[names(bg) %in% g$case])

  age_lab <- .age_label(graves)
  ttl <- paste0(title %||% if (level == "site") "Sites by case" else "Graves by case", age_lab)

  if (is.null(jitter))
    jitter <- 0.015 * max(diff(range(g$lng)), diff(range(g$lat)), 0.2)

  if (level == "site") {
    key <- paste(g$case, g$lat, g$lng, sep = "|")
    pts <- do.call(rbind, lapply(split(g, key), function(d) {
      w <- d$wealth[!is.na(d$wealth)]
      data.frame(case = d$case[1], lat = d$lat[1], lng = d$lng[1], n = nrow(d),
                 median_wealth = if (length(w)) stats::median(w) else NA_real_,
                 mean_wealth   = if (length(w)) mean(w) else NA_real_,
                 max_wealth    = if (length(w)) max(w) else NA_real_,
                 gini = if (length(w) >= 3 && sum(w) > 0)
                   DescTools::Gini(w, unbiased = TRUE) else NA_real_,
                 empty_share = mean(d$empty, na.rm = TRUE))
    }))
    rownames(pts) <- NULL
    pts$size_val <- pts[[size_by]]
    pts <- pts[order(-pts$n), , drop = FALSE]      # big points first, small on top
    size_lab <- switch(size_by, n = "Graves", median_wealth = "Median wealth",
                       mean_wealth = "Mean wealth", max_wealth = "Max wealth",
                       gini = "Gini (sites with\n>= 3 graves)",
                       empty_share = "Share without\ngrave goods")
  } else {
    pts <- g
    if (jitter > 0) {
      dup <- duplicated(paste(pts$lat, pts$lng)) |
             duplicated(paste(pts$lat, pts$lng), fromLast = TRUE)
      pts$lat[dup] <- pts$lat[dup] + stats::runif(sum(dup), -jitter, jitter)
      pts$lng[dup] <- pts$lng[dup] + stats::runif(sum(dup), -jitter, jitter)
    }
    pts$size_val <- pts$wealth
    size_lab <- "Wealth"
  }

  # Extent: points plus margin (at least 0.2 degrees across)
  rx <- range(pts$lng); ry <- range(pts$lat)
  dx <- max(diff(rx), 0.2); dy <- max(diff(ry), 0.2)
  xlim <- c(rx[1] - margin * dx, rx[2] + margin * dx)
  ylim <- c(ry[1] - margin * dy, ry[2] + margin * dy)

  p <- ggplot2::ggplot()
  if (isTRUE(borders)) {
    if (requireNamespace("maps", quietly = TRUE)) {
      world <- ggplot2::map_data("world", xlim = xlim + c(-5, 5), ylim = ylim + c(-5, 5))
      p <- p + ggplot2::geom_polygon(data = world,
                                     ggplot2::aes(x = long, y = lat, group = group),
                                     fill = "grey95", colour = "grey60", linewidth = 0.3)
    } else {
      message("Install the 'maps' package to draw country borders.")
    }
  }
  na_pts <- is.na(pts$size_val)
  if (any(na_pts) && level == "site")
    p <- p + ggplot2::geom_point(data = pts[na_pts, , drop = FALSE],
                                 ggplot2::aes(x = lng, y = lat), colour = "grey50",
                                 size = 1.2, shape = 4)
  p <- p +
    ggplot2::geom_point(data = pts[!na_pts, , drop = FALSE],
                        ggplot2::aes(x = lng, y = lat, colour = case, size = size_val),
                        alpha = alpha) +
    ggplot2::scale_size_area(max_size = if (level == "site") 10 else 4, name = size_lab) +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"),
                   panel.grid = ggplot2::element_line(colour = "white", linewidth = 0.3)) +
    ggplot2::labs(title = ttl, x = "Longitude", y = "Latitude", colour = "Case",
                  caption = if (level == "grave" && jitter > 0)
                    "Graves with identical coordinates are spread slightly for visibility." else
                    if (level == "site" && any(na_pts))
                    "x = site with no value for this statistic (e.g. fewer than 3 graves for a Gini)." else NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3, alpha = 1)))
  attr(p, if (level == "site") "sites" else "graves") <- pts[, setdiff(names(pts), "size_val")]
  p
}
