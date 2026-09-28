#' topographic_wetness_index
#'
#' @param drainage_area A `SpatRaster`. Drainage area, usually from flow accumulation.
#' @param slope A `SpatRaster`. Slope, in radians.
#' @param drainage_area_weight Numeric. Weight of the drainage area.
#' @param slope_weight Numeric. Weight of the slope.
#'
#' @returns
#' topographic wetness index
#' @export
#'
#' @examples
#' # no example
topographic_wetness_index <- function(drainage_area,
                                      slope,
                                      drainage_area_weight = 1,
                                      slope_weight = 1) {

  # Éviter les divisions par zéro
  slope[slope <= 0] <- NA
  drainage_area[drainage_area <= 0] <- NA

  # Calcul du TWI
  twi <- log((drainage_area^drainage_area_weight) / (tan(slope)^slope_weight))

  return(twi)

}
