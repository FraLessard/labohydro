#' topographic_wetness_index
#'
#' @param drainage_area drainage area defined usyally by flow accumulation
#' @param slope slope
#' @param drainage_area_weight weight of drainage area
#' @param slope_weight weight of slope
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
