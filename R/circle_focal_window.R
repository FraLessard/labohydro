#' circle_focal_window
#'
#' @param radius radius of window
#'
#' @returns
#' a matrix of circle focal window
#' @export
#'
#' @examples
#' no example
circle_focal_window <- function(radius){
  # Define the radius of the focal window (it must cover the outer radius)
  window_radius <- radius * 2 + 1  # total window radius is twice the outer radius + 1
  diameter <- radius + 1

  # Create an empty matrix for the focal window
  focal_window <- matrix(0, nrow = window_radius, ncol = window_radius)

  # Fill the focal window to create a donut-shaped matrix
  # Mark cells within the outer radius but excluding the inner radius
  for (i in 1:window_radius) {
    for (j in 1:window_radius) {
      distance <- sqrt((i - diameter)^2 + (j - diameter)^2) # Compute the distance from the center cell
      if (distance <= radius) {
        focal_window[i, j] <- 1  # Set to 1 where the distance is within the radius
      }
    }
  }
  return(focal_window)
}
