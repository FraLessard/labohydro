#' sample_farthest_point
#'
#' @param nb number of point to sample
#' @param res resolution of raster to use for distance matrix
#' @param sampling_area sampling area to cover
#' @param seed seed for reproducibility
#'
#' @returns
#' nb number of point placed as far apart from each other as possible
#' @export
#'
#' @examples
#' # no example
sample_farthest_point <- function(nb,
                                  res,
                                  sampling_area,
                                  seed) {

  # ------------------------------------------------------------
  # 1. Validate inputs
  # ------------------------------------------------------------
  if (!inherits(sampling_area, "sf")) {
    stop("Sampling_area must be sf objects.")
  }

  # ------------------------------------------------------------
  # 2. Prepare a permanent empty raster template (created ONCE)
  # ------------------------------------------------------------
  bbox <- sf::st_bbox(sampling_area)

  rast_template <- terra::rast(
    xmin  = bbox["xmin"],
    xmax  = bbox["xmax"],
    ymin  = bbox["ymin"],
    ymax  = bbox["ymax"],
    resolution = res,
    crs = sf::st_crs(sampling_area)$wkt
  )

  terra::values(rast_template) <- 1

  # ------------------------------------------------------------
  # 3. Select the first point randomly
  # ------------------------------------------------------------
  set.seed(seed)

  pt <- sf::st_sample(sampling_area, 1, type = "random")

  # ------------------------------------------------------------
  # 4. Iteratively select farthest points
  # ------------------------------------------------------------
  for (i in 1:(nb-1)) {

    # Compute distance raster FROM currently selected points
    dist_raster <- terra::distance(rast_template, terra::vect(pt))

    # Mask raster
    dist_raster_masked <- terra::mask(dist_raster, sampling_area)

    # Plot results
    terra::plot(dist_raster_masked)
    sf::plot(pt, add = T, col = "red", pch = 16)

    # Locate farthest cell location within raster
    max_cell <- which.max(terra::values(dist_raster_masked))
    farthest_cell <- terra::rast(dist_raster_masked)
    terra::values(farthest_cell) <- NA
    terra::values(farthest_cell)[max_cell] <- 1

    # Randomly sample one point inside this cell
    farthest_cell_poly <- terra::as.polygons(farthest_cell)
    farthest_cell_poly <- sf::st_as_sf(farthest_cell_poly)
    set.seed(seed)
    new_pt <- sf::st_sample(farthest_cell_poly, 1, type = "random")

    # Plot results
    sf::plot(new_pt, add = T, col = "blue", pch = 16)

    pt <- dplyr::bind_rows(pt, new_pt)

  }

  return(pt)
}
