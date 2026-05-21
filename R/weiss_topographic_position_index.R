#' Topographic position index from Weiss poster : https://www.jennessent.com/downloads/TPI-poster-TNC_18x22.pdf
#'
#' @param dem digital elevation model
#' @param inner_radius inner radius
#' @param outer_radius outer radius
#'
#' @returns
#' Topographic position index from Weiss poster
#' @export
#'
#' @examples
#' # no example
weiss_topographic_position_index <- function(dem,
                                             inner_radius,
                                             outer_radius){

  # Get the resolution of the DEM (cell size in meters)
  resolution <- terra::res(dem)[1]  # assuming square cells

  # Convert radii from meters to number of cells
  inner_size <- round((inner_radius+resolution/2) / resolution)  # inner radius in number of cells
  outer_size <- round((outer_radius-resolution/2) / resolution)  # outer radius in number of cells

  # Apply focal mean with the donut-shaped window
  focal_mean <- terra::focal(dem,
                             w = annulus_focal_windows(inner_size, outer_size),
                             fun = "mean",
                             na.policy = 'omit')

  # Calculate TPI: TPI = DEM - focal_mean + 0.5
  tpi <- dem - focal_mean + 0.5

  # Apply rounding to integer
  tpi_int <- round(tpi)

  return(tpi_int)
}
