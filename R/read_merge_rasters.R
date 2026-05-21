#' read_merge_rasters
#'
#' @param path path to rasters
#' @param pattern pattern of rasters to merge
#'
#' @returns
#' a mutilayer spatraster
#' @export
#'
#' @examples
#' # no example
read_merge_rasters <- function(path = getwd(),
                               pattern){

  files <- list.files(path = path,
                      pattern = "\\.tif$",
                      full.names = T)
  files <- stringr::str_subset(files, pattern)

  rasters <- purrr::map(files, terra::rast)
  rasters <- terra::rast

  return(rasters)
}
