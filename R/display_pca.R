#' display_pca
#'
#' @param data data frame
#' @param color_variable variable to use for coloring
#' @param point_size size of point
#' @param label_size size of label
#'
#' @returns
#' a nice and clean pca
#' @export
#'
#' @examples
#' no example
display_pca <- function(data,
                        color_variable,
                        point_size,
                        label_size){
  data |>
    dplyr::select(-!!sym(color_variable)) |>
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
