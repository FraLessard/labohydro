#' Run a SAGA-GIS tool
#'
#' @param library Character. SAGA-GIS library (e.g. "ta_morphometry").
#' @param tool Character. Tool number in the library (e.g. "9").
#' @param ... Character. Tool arguments (e.g. "-DEM", "dem.sgrd").
#' @param saga_cmd Character. Path to the SAGA-GIS command-line executable.
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
