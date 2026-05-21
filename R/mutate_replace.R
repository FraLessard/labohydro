#' mutate_replace
#'
#' @param x a dataframe
#' @param col_name column name to replace value
#' @param old_value old value
#' @param new_value new value
#'
#' @returns
#' a dataframe with replaced values inside the column
#' @export
#'
#' @examples
#' no example
mutate_replace <- function(x,
                           col_name,
                           old_value,
                           new_value) {

    dplyr::mutate(x, {{ col_name }} := ifelse({{ col_name }} %in% old_value, new_value, {{ col_name }})) -> x_replaced

  return(x_replaced)

}
