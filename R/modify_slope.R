#' modify_slope
#'
#' @param slope A `SpatRaster`. Slope.
#' @param slope_weight Numeric. Weight of the slope.
#' @param suction Numeric. Suction parameter.
#'
#' @returns
#' modified slope to modify flow accumulation
#' @export
#'
#' @examples
#' # no example
modify_slope <- function(slope,
                         slope_weight,
                         suction){

  t_param <- suction^(slope_weight*slope)
  m_slope <- (1/t_param)^exp(t_param)
  return(m_slope)

}
