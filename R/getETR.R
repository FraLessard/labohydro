#' getETR
#'
#' @param ETP ETP
#' @param rain rain
#'
#' @returns
#' ETR
#' @export
#'
#' @examples
#' # no example
getETR <- function(ETP,
                   rain){

  ETR <- rep(NA,
             times = length(ETP))

  # 0 < rain < 5
  ETR[which(rain > 0 & rain < 5)] <- ETP[which(rain > 0 & rain < 5)]

  # rain > 5
  ETR[which(rain > 5)] <- 0

  # rain == 0
  ETR[which(rain == 0)] <- 0.5 * ETP[which(rain == 0)]

  # rain[i-1] > 5
  rain_5mm <- rain > 5
  yesterday_rain_5mm <- logical(length(rain))
  yesterday_rain_5mm[-1] <- rain_5mm[-length(rain_5mm)]
  ETR[which(yesterday_rain_5mm)] <- ETP[which(yesterday_rain_5mm)]

  return(ETR)

}
