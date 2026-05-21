#' random_raster
#'
#' @param raster reference raster
#' @param error rmse of error to infer into raster
#' @param seed seed for reproducibility
#'
#' @returns
#' a random raster
#' @export
#'
#' @examples
#' # no example
random_raster <- function(raster,
                          error,
                          seed = 1){

  # Generate a new raster with the same extent and resolution as the input DEM
  raster_random <- terra::rast(raster)  # Copy properties from DEM raster
  # Generate random values from a normal distribution using the specified mean and standard deviation
  set.seed(seed) # Set a random seed for reproducibility
  terra::values(raster_random) <- stats::rnorm(terra::ncell(raster_random), mean = 0, sd = 1)

  terra::mask(raster_random, raster) -> raster_random_mask

  terra::focal(raster_random_mask, w = 3, fun = "mean") -> raster_random_foc

  raster_random_foc/terra::global(raster_random_foc, "sd", na.rm = T)$sd -> raster_random_foc_corr

  raster_random_foc_corr*error -> raster_random_foc_corr_err

  return(raster_random_foc_corr_err)

}
