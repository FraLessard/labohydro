#' fill_dem_holes_idw
#'
#' @param dem path to digital elevation model with extension
#' @param output path to digital elevation model filled with extension
#' @param working_directory working directory
#' @param idw_power idw power
#' @param remove_temp remove temporary data : dem file
#'
#' @returns
#' a dem with no data filled
#' @export
#'
#' @examples
#' no example
fill_dem_holes_idw <- function(dem,
                               output,
                               working_directory = getwd(),
                               idw_power = 2,
                               remove_temp = TRUE) {

  # ------------------------------------------------------------
  # 1. Save and switch working directory
  # ------------------------------------------------------------
  original_wd <- getwd()
  setwd(working_directory)

  # ------------------------------------------------------------
  # 2. Create temporary filenames
  # ------------------------------------------------------------
  dem_file        <- "dem_input_temp.tif"
  inner_mask_file <- "inner_holes_mask.tif"
  idw_filled_file <- "dem_inner_idw_fill_temp.tif"

  # ------------------------------------------------------------
  # 3. Identify NA voids in DEM
  # ------------------------------------------------------------
  na_mask <- is.na(dem)

  # Connected components (void regions)
  patches <- terra::patches(na_mask, directions = 8)
  vals <- values(patches)

  region_ids <- sort(unique(vals))
  region_ids <- region_ids[!is.na(region_ids)]

  # Initialize empty mask for INTERNAL holes only
  inner_mask <- terra::rast(dem)
  values(inner_mask) <- 0

  # ------------------------------------------------------------
  # 4. Detect internal voids based on raster boundaries
  #
  # A void is "internal" if:
  # - It does NOT touch the DEM border
  # - It is entirely surrounded by non-NA DEM values
  # ------------------------------------------------------------
  ncol_dem <- ncol(dem)
  nrow_dem <- nrow(dem)

  for (pid in region_ids) {

    # all cells belonging to this void
    region_cells <- which(vals == pid)

    # convert cell numbers to row/col
    rc <- terra::rowColFromCell(dem, region_cells)
    rows <- rc[,1]
    cols <- rc[,2]

    # check if void touches raster edges → EXTERNAL void
    if (any(rows == 1) || any(rows == nrow_dem) ||
        any(cols == 1) || any(cols == ncol_dem)) {
      next  # skip external voids
    }

    # This is an internal hole → mark for IDW fill
    inner_mask[region_cells] <- 1
  }

  # If no internal voids → return the input DEM unchanged
  if (all(values(inner_mask) == 0, na.rm = TRUE)) {
    message("No internal voids detected.")
    terra::writeRaster(dem, output, overwrite = TRUE)
    setwd(original_wd)
    return(dem)
  }

  # ------------------------------------------------------------
  # 5. Write DEM + mask to disk
  # ------------------------------------------------------------
  terra::writeRaster(dem, dem_file, overwrite = TRUE)
  terra::writeRaster(inner_mask, inner_mask_file, overwrite = TRUE)

  # ------------------------------------------------------------
  # 6. Perform WhiteboxTools IDW interpolation ONLY on inner voids
  # ------------------------------------------------------------
  whitebox::wbt_idw_interpolation(
    i      = dem_file,
    output = idw_filled_file,
    mask   = inner_mask_file,
    power  = idw_power,
    verbose = FALSE
  )

  # ------------------------------------------------------------
  # 7. Load output and preserve NA outside DEM mask
  # ------------------------------------------------------------
  dem_filled <- terra::rast(idw_filled_file)
  dem_filled <- terra::mask(dem_filled, !is.na(dem), maskvalues = FALSE)

  # ------------------------------------------------------------
  # 8. Save final result
  # ------------------------------------------------------------
  terra::writeRaster(dem_filled, output, overwrite = TRUE)

  # ------------------------------------------------------------
  # 9. Cleanup
  # ------------------------------------------------------------
  if (remove_temp) {
    file.remove(dem_file,
                inner_mask_file,
                idw_filled_file)
  }

  # ------------------------------------------------------------
  # 10. Restore working directory
  # ------------------------------------------------------------
  setwd(original_wd)

  return(dem_filled)
}
