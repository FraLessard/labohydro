#' assign_date
#'
#' @param x An `sf` object. Features to which a date is assigned.
#' @param y An `sf` object. Features with known dates, from which the nearest date is taken.
#' @param date_col Character. Name of the date column in `y`.
#'
#' @returns
#' compute the "new_date" column with a "date_distance" column
#' @export
#'
#' @examples
#' # no example
assign_date <- function(x,
                        y,
                        date_col){

  # Compute nearest id
  y_drop_na <- tidyr::drop_na(y, !!rlang::sym(date_col))
  y_nearest_id <- sf::st_nearest_feature(x, y_drop_na)
  new_date <- dplyr::pull(y_drop_na[y_nearest_id, ], !!rlang::sym(date_col))

  # Compute distance
  date_distance <- purrr::map2_dbl(sf::st_geometry(x),
                            sf::st_geometry(y_drop_na[y_nearest_id, ]),
                            sf::st_distance)

  x_new_date <- dplyr::mutate(x,
                              new_date = new_date,
                              date_distance = date_distance)

  return(x_new_date)

}
