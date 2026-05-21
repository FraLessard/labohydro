#' download_lidar_raster
#'
#' @param index index in sf with download path : "lidar_url"
#' @param tile tile number
#' @param raster raster product : "MHC", "MNT", "MNT_Ombre", "Pentes"
#' @param destination destination of raster
#'
#' @returns
#' path of downloaded raster
#' @export
#'
#' @examples
#' # no example
download_lidar_raster <- function(index,
                                  tile,
                                  raster = "MNT", # Choose one : "MHC", "MNT", "MNT_Ombre", "Pentes"
                                  destination = getwd()) {

  # ------------------------------------------------------------
  # 1. Temporarily increase the download timeout
  #
  #    CRAN policy prohibits changing global options permanently.
  #    Therefore:
  #    - Save the current timeout value
  #    - Set a high timeout only for the duration of this function
  #    - Automatically restore the user's original settings on exit
  # ------------------------------------------------------------
  old_timeout <- getOption("timeout")
  options(timeout = 10^10)
  on.exit(options(timeout = old_timeout), add = TRUE)


  # ------------------------------------------------------------
  # 2. Select the correct row in the index based on tile
  #
  #    The 'index' table must include:
  #       - a column named 'feuillet' (tile)
  #       - a column named 'lidar_url' (base download URL)
  #
  #    If the requested sheet does not exist in the index,
  #    the function stops with a clear error message.
  # ------------------------------------------------------------
  index_select <- index[index$feuillet == tile, ]

  if (nrow(index_select) == 0) {
    stop("The specified tile does not exist in the index.")
  }


  # ------------------------------------------------------------
  # 3. Build the product file name
  #
  #    Example:
  #        raster = "MNT"
  #        tile = "32B11NE"
  #
  #    Result:
  #        "MNT_32B11NE.tif"
  # ------------------------------------------------------------
  filename <- paste0(raster, "_", index_select$feuillet, ".tif")


  # ------------------------------------------------------------
  # 4. Construct the full download URL
  #
  #    Some URLs in metadata do not end with "/", so we ensure
  #    that the final URL is valid by appending "/" if needed.
  #
  #    Example:
  #       lidar_url = "https://example.com/path"
  #       filename = "MNT_32B11NE.tif"
  #
  #    Final URL:
  #       "https://example.com/path/MNT_32B11NE.tif"
  # ------------------------------------------------------------
  base_url <- index_select$lidar_url

  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  file_url <- paste0(base_url, filename)


  # ------------------------------------------------------------
  # 5. Build the destination path (local file)
  #
  #    Combines the user-provided output directory with the
  #    file name determined earlier.
  # ------------------------------------------------------------
  dest_path <- file.path(destination, filename)


  # ------------------------------------------------------------
  # 6. Download the product using base R
  #
  #    - utils::download.file() is the canonical and CRAN-safe
  #      method for downloading files in R packages.
  #    - mode = "wb" ensures binary mode (important for TIFFs).
  # ------------------------------------------------------------
  utils::download.file(
    url      = file_url,
    destfile = dest_path,
    mode     = "wb",
    quiet    = FALSE
  )


  # ------------------------------------------------------------
  # 7. Return value
  #
  #    For convenience, the function returns the full path to
  #    the downloaded file, allowing direct use in pipelines.
  # ------------------------------------------------------------
  return(dest_path)
}
