#' getATnor
#'
#' @param date Date vector. Dates of the daily series.
#' @param tmin Numeric vector. Daily minimum temperature, in °C.
#' @param tmax Numeric vector. Daily maximum temperature, in °C.
#' @param years Integer. Number of previous years used to compute the normal.
#'
#' @returns
#' ATnor
#' @export
#'
#' @examples
#' # no example
getATnor <- function(date,
                     tmin,
                     tmax,
                     years = 30){

  ATnor <- rep(NA,
               times = length(date))
  AT <- tmax - tmin

  for(i in seq_along(date)){

    m <- lubridate::month(date[i])
    y <- lubridate::year(date[i])-years

    AT_m <- AT[lubridate::year(date) > y & lubridate::month(date) == m]

    ATnor[i] <- mean(AT_m,
                     na.rm = TRUE)

  }

  return(ATnor)

}
