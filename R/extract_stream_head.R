#' extract_stream_head
#'
#' @param channel channels
#' @param stream_field stream field
#' @param stream_class stream class
#' @param surveyed_area surveyed area
#' @param tolerance tolerance distance
#'
#' @returns
#' stream head
#' @export
#'
#' @examples
#' # no example
extract_stream_head <- function(channel,
                                stream_field,
                                stream_class,
                                surveyed_area,
                                tolerance){

  stream_channel <- channel[channel[[stream_field]] == stream_class,]

  stream_head <- dplyr::bind_rows(sf::st_as_sf(lwgeom::st_startpoint(stream_channel)),
                            sf::st_as_sf(lwgeom::st_endpoint(stream_channel)))

  # Remove stream head that touche two stream channel
  stream_head_distances <- sf::st_distance(stream_head,
                                           stream_channel)

  apply(stream_head_distances,
        MARGIN = 1,
        function(x){sum(x < tolerance) == 1}) -> keep

  stream_head_keeped <- stream_head[keep,]

  # Remove stream head outside of the surveyed area
  stream_head_keeped <- sf::st_filter(stream_head_keeped,
                                      surveyed_area)

  # Remove stream head that touche other stream two times
  other_channel <- channel[channel[[stream_field]] != stream_class,]
  other_channel_buffer <- sf::st_buffer(other_channel, tolerance)
  other_channel_buffer <- sf::st_union(other_channel_buffer)
  other_channel_buffer <- sf::st_cast(other_channel_buffer, "POLYGON")
  other_channel_buffer <- sf::st_as_sf(other_channel_buffer)

  stream_head_distances <- sf::st_distance(stream_head_keeped,
                                           other_channel_buffer)

  stream_head_distances <- apply(stream_head_distances,
                                 MARGIN = 2,
                                 function(x){as.numeric(x)})

  apply(stream_head_distances,
        MARGIN = 1,
        function(x){

          !any(((colSums(stream_head_distances == 0) >= 2) + (x == 0)) == 2)

        }) -> keep

  stream_head_keeped <- stream_head_keeped[keep,]

  return(stream_head_keeped)

}
