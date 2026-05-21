#' getRE
#'
#' @param date date
#' @param latitude latitude
#'
#' @returns
#' RE
#' @export
#'
#' @examples
#' no example
getRE <- function(date,
                  latitude) {

  # constantes
  rad <- pi / 180
  fac <- 1440 / pi                         # = 24*60/pi (minutes/jour/rad)

  # conversions
  J <- as.POSIXlt(date, format = "%d%b%y")$yday
  phi <- latitude * rad                     # latitude en radians
  dr  <- 1 + 0.033 * cos(2 * pi * J / 365) # distance Terre-Soleil
  delta <- 0.409 * sin(2 * pi * J / 365 - 1.39) # déclinaison en radians

  # argument pour acos peut dépasser [-1,1] numériquement -> clamp
  arg <- -tan(phi) * tan(delta)
  arg[arg >  1] <-  1
  arg[arg < -1] <- -1
  omega_s <- acos(arg)                      # angle horaire coucher (rad)

  # formule principale (cal/cm2/day)
  RE <- fac * 1.94 * dr * ( omega_s * sin(phi) * sin(delta) +
                              cos(phi) * cos(delta) * sin(omega_s) )

  return(RE)
}
