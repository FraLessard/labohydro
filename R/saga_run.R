#' Run a SAGA-GIS tool
#'
#' @param library library of SAGA-GIS / ex : "ta_morphometry"
#' @param tool tool number / ex : "9"
#' @param ... arguments / ex : "-DEM", paste0(wd, "/dem_filtered_breached_crop.sgrd"),
#' @param saga_cmd SAGA-GIS path
#'
#' @returns
#' tool ouput
#' @export
#'
#' @examples
#' no example
saga_run <- function(library,
                     tool,
                     ...,
                     saga_cmd = "C:/Logiciels/saga-8.3.0_x64/saga_cmd.exe"){

  args <- c(library, tool, ...)

  system2(saga_cmd, args = shQuote(args), stdout = TRUE, stderr = TRUE)

}