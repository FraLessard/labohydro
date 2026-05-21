#' modify_slope
#'
#' @param slope slope
#' @param slope_weight slope weight
#' @param suction suction
#'
#' @returns
#' modified slope to modify flow accumulation
#' @export
#'
#' @examples
#' no example
modify_slope <- function(slope,
                         slope_weight,
                         suction){

  t_param <- suction^(slope_weight*slope)
  m_slope <- (1/t_param)^exp(t_param)
  return(m_slope)

}
