#' extract_channel_head
#'
#' @param channel An `sf` object. Channel lines.
#' @param surveyed_area An `sf` object. Surveyed area polygon.
#' @param tolerance Numeric. Distance tolerance used to detect connected channels, in map units.
#'
#' @returns
#' channel head
#' @export
#'
#' @examples
#' # no example
extract_channel_head <- function(channel,
                                 surveyed_area,
                                 tolerance){

  channel_head <- dplyr::bind_rows(sf::st_as_sf(lwgeom::st_startpoint(channel)),
                                   sf::st_as_sf(lwgeom::st_endpoint(channel)))

  channel_head_distances <- sf::st_distance(channel_head,
                                            channel)

  apply(channel_head_distances,
        MARGIN = 1,
        function(x){sum(x < tolerance) == 1}) -> keep

  channel_head_keeped <- channel_head[keep,]

  channel_head_keeped <- sf::st_filter(channel_head_keeped,
                                       surveyed_area)

  return(channel_head_keeped)

}
