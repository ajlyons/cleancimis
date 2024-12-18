#' Find the closest CIMIS station to a point
#'
#' Find the closest CIMIS station to a point
#'
#' @param pt either be a sf object or a numeric vector of length 2 containing longitude and latitude coordinates
#' @param cimis_sf A sf point object with CIMIS station locations

#' @seealso [cc_stn_active]
#' @importFrom sf st_as_sf st_transform st_crs st_nearest_feature
#' @importFrom dplyr pull slice
#' @importFrom conflicted conflicts_prefer
#'
#' @export

cc_stn_closest <- function(pt, cimis_sf) {

  if(!inherits(cimis_sf, "sf")) stop("cimis_sf must be a point sf object")
  if (!"stid" %in% names(cimis_sf)) stop("cimis_sf must have a column called 'stid'")

  conflicted::conflicts_prefer(dplyr::filter, dplyr::pull, .quiet = TRUE)

  if (is.numeric(pt)) {
    if (length(pt) != 2) stop("Point should be a vector of longitude-latitude coordinates, or a sf object")

    pt_sf <- data.frame(id = 1,
                        lon = pt[1],
                        lat = pt[2]) |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      st_transform(st_crs(cimis_sf))

  } else if (inherits(pt, "sf")) {
    pt_sf <- st_transform(pt, st_crs(cimis_sf))
  }

  ## Get the index of the closest station
  closest_station_idx <- st_nearest_feature(pt_sf, cimis_sf)

  ## Return the stid of the closest station
  cimis_sf |>
    slice(closest_station_idx) |>
    pull(stid)

}
