#' crop_mask
#'
#' @param raster A `SpatRaster`. Raster to crop and mask.
#' @param polygon An `sf` object. Polygon used to crop and mask `raster`.
#' @param transform Logical. If `TRUE`, the output is projected to the CRS of `polygon`.
#' @param method Character. Resampling method used when `transform = TRUE`: "bilinear" for continuous rasters or "near" for categorical rasters.
#'
#' @returns
#' a raster with extent and mask of polygon
#' @export
#'
#' @examples
#' # no example
crop_mask <- function(raster,
                      polygon,
                      transform = TRUE,
                      method = "bilinear"){

  polygon_crs <- sf::st_transform(polygon, sf::st_crs(raster))

  raster <- terra::crop(raster, polygon_crs)
  raster <- terra::mask(raster, polygon_crs)

  if(transform == TRUE){
    raster <- terra::project(raster, sf::st_crs(polygon)$wkt, method = method)
  }

  return(raster)
}
