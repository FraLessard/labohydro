#' display_pca
#'
#' @param data A `data.frame`. Numeric variables and the coloring variable.
#' @param color_variable Character. Name of the column used to color the points.
#' @param point_size Numeric. Size of the points.
#' @param label_size Numeric. Size of the loading labels.
#'
#' @returns
#' a nice and clean pca
#' @export
#'
#' @examples
#' # no example
display_pca <- function(data,
                        color_variable,
                        point_size,
                        label_size){
  data |>
    dplyr::select(-!!dplyr::sym(color_variable)) |>
    stats::prcomp(scale = TRUE) -> pca

  tidyterra::autoplot(pca,
                      data = data,
                      colour = color_variable,
                      size = point_size,
                      loadings = TRUE,
                      loadings.colour = 'black',
                      loadings.label = TRUE,
                      loadings.label.size = label_size,
                      loadings.label.colour = 'black',
                      frame = TRUE,
                      frame.type = 'norm',
                      loadings.label.repel=T,
                      variance_percentage = FALSE) -> pca_plot

  return(pca_plot)

}
