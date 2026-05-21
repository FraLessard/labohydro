#' crop_mask
#'
#' @param raster raster to clip
#' @param polygon reference polygon
#' @param transform transform crs of final raster TRUE/FALSE
#' @param method transformation method : "bilinear" for numeric raster and "near" for categorical raster
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
