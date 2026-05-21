#' breach_depression
#'
#' @param input path to digital elevation model with extension
#' @param output path to digital elevation model breached with extension
#' @param working_directory working directory
#' @param remove_temp remove temporary data : fill scp/breach scp/breach lc
#'
#' @returns
#' a dem with depressions breached
#' @export
#'
#' @examples
#' # no example
breach_depression <- function(input,
                              output,
                              working_directory = getwd(),
                              remove_temp = TRUE) {

  # ------------------------------------------------------------
  # 1. Save current working directory and switch to target folder
  # ------------------------------------------------------------
  original_wd <- getwd()
  setwd(working_directory)

  # ------------------------------------------------------------
  # 2. Define clear temporary filenames
  #
  #    These will be created inside working_directory and can
  #    be deleted later if remove_temp = TRUE.
  # ------------------------------------------------------------
  fill_scp_file   <- "fill_scp.tif"
  breach_scp_file <- "breach_scp.tif"
  breach_lc_file  <- "breach_lc.tif"

  # ------------------------------------------------------------
  # 3. Step 1 — Fill single-cell pits
  #
  #    Removes isolated sinks made from floating-point noise.
  # ------------------------------------------------------------
  whitebox::wbt_fill_single_cell_pits(
    dem = input,
    output = fill_scp_file,
    verbose_mode = FALSE
  )

  # ------------------------------------------------------------
  # 4. Step 2 — Breach single-cell pits (simple carving)
  #
  #    Ensures that small pits are carved instead of filled,
  #    preserving hydrological realism.
  # ------------------------------------------------------------
  whitebox::wbt_breach_single_cell_pits(
    dem = fill_scp_file,
    output = breach_scp_file
  )

  # ------------------------------------------------------------
  # 5. Step 3 — Breach depressions using least-cost path carving
  #
  #    - "flat_increment" ensures flow paths are not flat.
  #    - "dist" controls maximum carving distance.
  #    - "fill = FALSE" avoids automatic elevation filling.
  # ------------------------------------------------------------
  whitebox::wbt_breach_depressions_least_cost(
    dem = breach_scp_file,
    output = breach_lc_file,
    flat_increment = 0.0001,
    dist = 50,
    fill = FALSE
  )

  # ------------------------------------------------------------
  # 6. Step 4 — Final breaching of all remaining depressions
  #
  #    The output is written to the user-provided path.
  # ------------------------------------------------------------
  whitebox::wbt_breach_depressions(
    dem = breach_lc_file,
    output = output,
    flat_increment = 0.0001
  )

  # ------------------------------------------------------------
  # 7. Optional cleanup of temporary intermediate files
  # ------------------------------------------------------------
  if (remove_temp) {
    files_to_remove <- c(fill_scp_file,
                         breach_scp_file,
                         breach_lc_file)

    exists_idx <- file.exists(files_to_remove)
    if (any(exists_idx)) {
      file.remove(files_to_remove[exists_idx])
    }
  }

  # ------------------------------------------------------------
  # 8. Restore original working directory
  # ------------------------------------------------------------
  setwd(original_wd)
}
