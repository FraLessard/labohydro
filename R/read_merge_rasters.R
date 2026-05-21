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
#' no example
read_merge_rasters <- function(path = getwd(),
                               pattern){

  list.files(path = path,
             pattern = "\\.tif$",
             full.names = T) |>
    stringr::str_subset(pattern) |>
    map(terra::rast) %>%
    terra::rast -> rasters

  return(rasters)
}
