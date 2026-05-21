#' annulus_focal_window
#'
#' @param inner_radius inner radius of annulus in number of cells
#' @param outer_radius outer radius of annulus in number of cells
#'
#' @returns
#' a matrix of annulus focal window
#' @export
#'
#' @examples
#' no example
annulus_focal_window <- function(inner_radius,
                                 outer_radius){
  # Define the radius of the focal window (it must cover the outer radius)
  window_radius <- outer_radius * 2 + 1  # total window radius is twice the outer radius + 1
  diameter <- outer_radius + 1

  # Create an empty matrix for the focal window
  focal_window <- matrix(0, nrow = window_radius, ncol = window_radius)

  # Fill the focal window to create a donut-shaped matrix
  # Mark cells within the outer radius but excluding the inner radius
  for (i in 1:window_radius) {
    for (j in 1:window_radius) {
      distance <- sqrt((i - diameter)^2 + (j - diameter)^2) # Compute the distance from the center cell
      if (distance >= inner_radius && distance <= outer_radius) {
        focal_window[i, j] <- 1  # Set to 1 where the distance is within the annulus
      }
    }
  }
  return(focal_window)
}
