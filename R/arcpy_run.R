#' Run a arcpy tool
#'
#' @param script Character. Path to the Python script to run.
#' @param ... Character. Arguments passed to the script.
#' @param python_exe Character. Path to the ArcGIS Pro Python executable.
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
