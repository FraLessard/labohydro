#' Landform from Weiss poster : https://www.jennessent.com/downloads/TPI-poster-TNC_18x22.pdf
#'
#' @param tpi300 A `SpatRaster`. Topographic position index at the 300 m scale (annulus of 150 m to 300 m, i.e. 15 to 30 cells at 30 m resolution).
#' @param tpi2000 A `SpatRaster`. Topographic position index at the 2000 m scale (annulus of 1860 m to 2010 m, i.e. 62 to 67 cells at 30 m resolution).
#' @param scale_tpi300 Numeric. Threshold applied to `tpi300`.
#' @param scale_tpi2000 Numeric. Threshold applied to `tpi2000`.
#' @param slope A `SpatRaster`. Slope, in degrees.
#'
#' @returns
#' Landform from Weiss poster
#' @export
#'
#' @examples
#' # no example
weiss_landform <- function(tpi300,
                           tpi2000,
                           scale_tpi300 = 100,
                           scale_tpi2000 = 100,
                           slope) {

  cond1 <- tpi300 <= -(scale_tpi300) & tpi2000 <= -(scale_tpi2000)
  cond2 <- tpi300 <= -(scale_tpi300) & tpi2000 > -(scale_tpi2000) & tpi2000 < (scale_tpi2000)
  cond3 <- tpi300 <= -(scale_tpi300) & tpi2000 >= (scale_tpi2000)
  cond4 <- tpi300 > -(scale_tpi300) & tpi300 < (scale_tpi300) & tpi2000 <= -(scale_tpi2000)
  cond5 <- tpi300 > -(scale_tpi300) & tpi300 < (scale_tpi300) & tpi2000 > -(scale_tpi2000) & tpi2000 < (scale_tpi2000) & slope <= 15
  cond6 <- tpi300 > -(scale_tpi300) & tpi300 < (scale_tpi300) & tpi2000 > -(scale_tpi2000) & tpi2000 < (scale_tpi2000) & slope > 15
  cond7 <- tpi300 > -(scale_tpi300) & tpi300 < (scale_tpi300) & tpi2000 >= (scale_tpi2000)
  cond8 <- tpi300 >= (scale_tpi300) & tpi2000 <= -(scale_tpi2000)
  cond9 <- tpi300 >= (scale_tpi300) & tpi2000 > -(scale_tpi2000) & tpi2000 < (scale_tpi2000)
  cond10 <- tpi300 >= (scale_tpi300) & tpi2000 >= (scale_tpi2000)

  landform <-
    cond1 * 1 +
    cond2 * 2 +
    cond3 * 3 +
    cond4 * 4 +
    cond5 * 5 +
    cond6 * 6 +
    cond7 * 7 +
    cond8 * 8 +
    cond9 * 9 +
    cond10 * 10

  landform_labels <- c("1" = "V-shape river valleys / Deep narrow cayons",
                       "2" = "Lateral midslope incised drainages / Local valleys in plains",
                       "3" = "Upland incised drainages / Stream headwaters",
                       "4" = "U-shape valleys",
                       "5" = "Broad flat areas (slope <= 15 %)",
                       "6" = "Broad open slopes (slope > 15 %)",
                       "7" = "Flat ridge tops / Mesa tops",
                       "8" = "Local ridge / Hilltops within broad valleys",
                       "9" = "Lateral midslope drainage divides / Local ridges in plains",
                       "10" = "Mountain tops / High narrow ridges")

  landform <- as.factor(landform)  # Convert raster to a factor
  levels(landform) <- data.frame(ID = 1:10, Class = landform_labels)  # Attach labels

  # Return the classified landforms
  return(landform)
}
