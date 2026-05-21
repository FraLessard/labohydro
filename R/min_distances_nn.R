#' min_distances_nn
#'
#' @param sf_object an sf object
#'
#' @returns
#' distance between an sf object and the closet one
#' @export
#'
#' @examples
#' # no example
min_distances_nn <- function(sf_object) {
  # k = 2 because the first neighbor returned is the feature itself
  nn <- nngeo::st_nn(sf_object, sf_object, k = 2)

  # Extract the index of the true nearest neighbor for each geometry
  idx <- sapply(nn, function(x) x[2])

  # Compute the distance to the nearest neighbor
  distances <- sf::st_distance(sf_object, sf_object[idx, ], by_element = TRUE)

  # Return a numeric vector
  return(as.numeric(distances))
}
