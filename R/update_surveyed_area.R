#' update_surveyed_area
#'
#' @param surveyed_area surveyed area in spatrast
#' @param update_area update area in sf feature
#' @param task task : "add" of "remove" area
#'
#' @returns
#' update surveyed area with 1 refer to surveyed area and 0 to non-surveyed area
#' @export
#'
#' @examples
#' no example
update_surveyed_area <- function(surveyed_area,
                                 update_area,
                                 task){

  update_area |>
    sf::st_transform(st_crs(surveyed_area)) |>
    sf::st_as_sf() -> update_area_crs

  surveyed_area_temp <- surveyed_area

  if(task == "add"){
  values(surveyed_area_temp) <- 1

  surveyed_area_temp |>
    terra::mask(update_area_crs) -> surveyed_area_temp

  c(surveyed_area,
    surveyed_area_temp) |>
    terra::app(fun = "max",
               na.rm = TRUE) -> surveyed_area_updated
  }

  if(task == "remove"){
    values(surveyed_area_temp) <- 0

    surveyed_area_temp |>
      terra::mask(update_area_crs) -> surveyed_area_temp

    c(surveyed_area,
      surveyed_area_temp) |>
      terra::app(fun = "min",
                 na.rm = TRUE) -> surveyed_area_updated
  }

  return(surveyed_area_updated)

}
