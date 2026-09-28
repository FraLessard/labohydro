#' mutate_replace
#'
#' @param x A `data.frame`. Data in which values are replaced.
#' @param col_name Column name (unquoted) in which values are replaced.
#' @param old_value Vector. Values to replace.
#' @param new_value Replacement value.
#'
#' @returns
#' a dataframe with replaced values inside the column
#' @export
#'
#' @examples
#' # no example
mutate_replace <- function(x,
                           col_name,
                           old_value,
                           new_value) {

    dplyr::mutate(x, {{ col_name }} := ifelse({{ col_name }} %in% old_value, new_value, {{ col_name }})) -> x_replaced

  return(x_replaced)

}
