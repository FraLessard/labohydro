#' burn_dem
#'
#' @param dem digital elevation model
#' @param vect_sf vector to be burned into dem, usually culvert
#'
#' @returns
#' burned dem
#' @export
#'
#' @examples
#' # no example
burn_dem <- function(dem, vect_sf) {

  # ------------------------------------------------------------
  # 1. Ensure the input vector is an sf object
  # ------------------------------------------------------------
  if (!inherits(vect_sf, "sf")) {
    stop("vect_sf must be an sf object.")
  }

  # ------------------------------------------------------------
  # 2. Cast all geometries to MULTILINESTRING
  #    (prevents mixed geometry issues during rasterization)
  # ------------------------------------------------------------
  vect_cast <- sf::st_cast(vect_sf, "MULTILINESTRING")

  # ------------------------------------------------------------
  # 3. Clip the features to the DEM bounding box
  # ------------------------------------------------------------
  dem_bbox <- sf::st_as_sfc(sf::st_bbox(dem))
  vect_clip <- sf::st_filter(vect_cast, dem_bbox)

  # If no features intersect the raster, return the original DEM
  if (nrow(vect_clip) == 0) {
    return(dem)
  }

  # ------------------------------------------------------------
  # 4. Add a unique ID column for rasterization
  # ------------------------------------------------------------
  vect_clip$id <- seq_len(nrow(vect_clip))

  # ------------------------------------------------------------
  # 5. Rasterize vector IDs onto the dem
  # ------------------------------------------------------------
  id_raster <- terra::rasterize(
    vect_clip,
    dem,
    field = "id"
  )

  # ------------------------------------------------------------
  # 6. For each ID, compute the minimum elevation in the DEM
  # ------------------------------------------------------------
  raster_min <- terra::zonal(
    x = dem,
    z = id_raster,
    fun = "min",
    as.raster = TRUE
  )

  # ------------------------------------------------------------
  # 7. Burn the minimum DEM value into the raster wherever vector
  #    features were rasterized
  # ------------------------------------------------------------
  dem_burned <- terra::ifel(
    !is.nan(id_raster),
    raster_min,
    dem
  )

  return(dem_burned)
}



# # OLD
# brulage.mnt <- function(mnt,
#                         liste_vecteurs){
#
#   liste_vecteurs %>%
#     lapply(st_cast, "MULTILINESTRING") %>%
#     bind_rows() %>%
#     st_filter(st_bbox(mnt) %>% st_as_sfc()) %>%
#     rowid_to_column("id") -> vecteur_brulage_id
#
#   if(nrow(vecteur_brulage_id) == 0){
#     mnt -> mnt_brule
#   } else {
#     vecteur_brulage_id %>%
#       terra::rasterize(mnt, "id") %>%
#       terra::zonal(mnt, ., fun = "min", as.raster = TRUE) -> ponceaux_raster_min
#     ifel(!is.nan(mnt) & !is.nan(ponceaux_raster_min), ponceaux_raster_min, mnt) -> mnt_brule
#   }
#   return(mnt_brule)
# }
