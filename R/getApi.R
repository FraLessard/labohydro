#' getApi
#'
#' @param x seq
#' @param k k
#' @param n number of data to use
#' @param finite TRUE/FALSE
#'
#' @returns
#' Api
#' @export
#'
#' @examples
#' no example
getApi <- function(x,
                   k = 0.9,
                   n = 5,
                   finite = TRUE) {

  l <- length(x)
  Api <- rep(NA,times=l)

  if(finite) {

    if(length(k)==1) {
      kn <- rep(NA,times=n)
      for(i in 1:n) kn[i] <- k^(n-i)
    } else {
      n <- length(k)
      kn <- sort(k)
    }

    for(i in (n+1):l) {
      Api[i] <- t(kn)%*%x[(i-n):(i-1)]
    }

  } else {

    k <- max(k)
    Api[2] <- x[1]
    for(i in 3:l) {Api[i] <- k*Api[i-1]+x[i-1]}

  }

  return(Api)

}



# # Allow neg values
# getApi <- function(x, k = 0.9, n = 5, finite = TRUE) {
#
#   l <- length(x)
#   Api <- rep(NA_real_, l)
#
#   # Vérifier qu'on a assez de données
#   if (l <= 1) return(Api)
#
#   if (finite) {
#
#     # Calcul des pondérations
#     if (length(k) == 1) {
#       kn <- k ^ ((n - 1):0)
#     } else {
#       n <- length(k)
#       kn <- sort(k, decreasing = FALSE)
#     }
#
#     # Normaliser les poids si besoin
#     kn <- kn / sum(kn)
#
#     # Calcul glissant
#     if (l > n) {
#       for (i in seq(n + 1, l)) {
#         Api[i] <- sum(kn * x[(i - n):(i - 1)], na.rm = TRUE)
#       }
#     }
#
#   } else {
#     # Cas infini (récursif)
#     k <- max(k)
#     Api[2] <- x[1]
#     for (i in 3:l) {
#       Api[i] <- k * Api[i - 1] + x[i - 1]
#     }
#   }
#
#   return(Api)
# }
