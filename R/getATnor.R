#' getATnor
#'
#' @param date date
#' @param tmin tmin
#' @param tmax tmax
#' @param years
#'
#' @returns
#' ATnor
#' @export
#'
#' @examples
#' no example
getATnor <- function(date,
                     tmin,
                     tmax,
                     years = 30){

  ATnor <- rep(NA,
               times = length(date))
  AT <- tmax - tmin

  for(i in seq_along(date)){

    m <- month(date[i])
    y <- year(date[i])-years

    AT_m <- AT[year(date) > y & month(date) == m]

    ATnor[i] <- mean(AT_m,
                     na.rm = TRUE)

  }

  return(ATnor)

}
