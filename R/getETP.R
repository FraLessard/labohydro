#' getETP
#'
#' @param tmin Numeric vector. Daily minimum temperature, in °C.
#' @param tmax Numeric vector. Daily maximum temperature, in °C.
#' @param ATnor Numeric vector. Normal daily temperature range, from [getATnor()].
#' @param RE Numeric vector. Extraterrestrial radiation, from [getRE()].
#'
#' @returns
#' ETP
#' @export
#'
#' @examples
#' # no example
getETP <- function(tmin,
                   tmax,
                   ATnor,
                   RE){

  AT <- tmax - tmin
  ATx <- AT - ATnor

  ETP <- -1.75 + 0.0646*tmax + 0.0975*ATx + 0.00448*RE

  return(ETP)

}
