#' ceiling_odd
#'
#' @param x a number
#'
#' @returns
#' ceiling_odd
#' @export
#'
#' @examples
#' no example
ceiling_odd <- function(x) {
  # Apply ceiling to the value
  xc <- ceiling(x)

  # If the result is even, add 1 to make it odd
  if (xc %% 2 == 0) {
    xc <- xc + 1
  }

  return(xc)
}
