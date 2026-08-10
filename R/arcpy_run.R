#' Run a arcpy tool
#'
#' @param script path of script
#' @param ... arguments
#' @param python_exe arcpy path
#'
#' @returns
#' tool output
#' @export
#'
#' @examples
#' no example
arcpy_run <- function(script,
                      ...,
                      python_exe = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe"){
  
  args <- c(script, ...)
  
  system2(python_exe, args = shQuote(args), stdout = TRUE, stderr = TRUE)
  
}