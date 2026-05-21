#' getETP
#'
#' @param tmin tmin
#' @param tmax tmax
#' @param ATnor ATnor
#' @param RE RE
#'
#' @returns
#' ETP
#' @export
#'
#' @examples
#' no example
getETP <- function(tmin,
                   tmax,
                   ATnor,
                   RE){

  AT <- tmax - tmin
  ATx <- AT - ATnor

  ETP <- -1.75 + 0.0646*tmax + 0.0975*ATx + 0.00448*RE

  return(ETP)

}
