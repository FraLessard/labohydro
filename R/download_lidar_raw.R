#' download_lidar_raw
#'
#' @param index index in sf with download path : "file_url"
#' @param tile tile number
#' @param destination destination of lidar data
#'
#' @returns
#' path of downloaded lidar data
#' @export
#'
#' @examples
#' no example
download_lidar_raw <- function(index,
                               tile,
                               destination = getwd()) {

  # ------------------------------------------------------------
  # 1. Temporarily increase download timeout
  #
  #    This is CRAN‑safe:
  #    - It does NOT permanently change user's settings
  #    - It restores the previous value automatically on exit
  # ------------------------------------------------------------
  old_timeout <- getOption("timeout")
  options(timeout = 10^10)
  on.exit(options(timeout = old_timeout), add = TRUE)


  # ------------------------------------------------------------
  # 2. Select the appropriate LiDAR tile from the index
  #
  #    Required index columns:
  #      - "tile" (e.g., "32B11NE")
  #      - "TELECHARGEMENT_TUILE" (full download URL)
  #
  #    If the tile cannot be found, stop with a clear error.
  # ------------------------------------------------------------
  index_select <- index[index$NOM_TUILE == tile, ]

  if (nrow(index_select) == 0) {
    stop("The specified tile does not exist in the index.")
  }

  file_url <- index_select$TELECHARGEMENT_TUILE

  if (is.na(file_url) || file_url == "") {
    stop("Missing download URL in the 'TELECHARGEMENT_TUILE' column.")
  }


  # ------------------------------------------------------------
  # 3. Determine destination filename
  #
  #    Uses the filename provided by the URL:
  #      basename("https://.../Tile_32B11NE.zip") → "Tile_32B11NE.zip"
  #
  #    This ensures full compatibility with any LiDAR raw format:
  #      - .laz
  #      - .las
  #      - .zip
  #      - .7z
  # ------------------------------------------------------------
  filename <- basename(file_url)
  dest_path <- file.path(destination, filename)


  # ------------------------------------------------------------
  # 4. Download the LiDAR raw file
  #
  #    utils::download.file() is the official base‑R method
  #    recommended for R packages.
  #
  #    mode = "wb" ensures correct writing of binary files.
  # ------------------------------------------------------------
  utils::download.file(
    url      = file_url,
    destfile = dest_path,
    mode     = "wb",
    quiet    = FALSE
  )


  # ------------------------------------------------------------
  # 5. Return the full path to the downloaded file
  #
  #    Returning the path makes this function easy to integrate
  #    into larger workflows or pipelines.
  # ------------------------------------------------------------
  return(dest_path)
}
