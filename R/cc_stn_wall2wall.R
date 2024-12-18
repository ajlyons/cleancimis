#' Identify CIMIS stations for an area
#'
#' Identify CIMIS stations to get wall-to-wall coverage for an area
#'
#' @param poly A sf polygon object
#' @param cimis_sf A sf point object with CIMIS station locations
#' @param nn Number of nearest neighbors
#'
#' @details
#' This function will identify the nearest \code{nn} CIMIS stations for every inch within poly.
#' @returns A vector of CIMIS station IDs
#'
#' @seealso [cc_stn_active]
#'
#' @importFrom conflicted conflicts_prefer
#' @importFrom sf st_coordinates st_drop_geometry st_transform st_as_sf
#' @importFrom dplyr select filter as_tibble
#' @importFrom FNN get.knnx
#' @importFrom terra rast mask values<- as.points
#' @export

## for any location in the polygon we have the N nearest active CIMIS stations
## (including those that may fall outside the polygon)

cc_stn_wall2wall <- function(poly, cimis_sf, nn = 1) {

  if(!inherits(poly, "sf")) stop("poly must be a point sf object")
  if (nrow(poly) != 1) stop("poly must contain one and only one polygon")

  if(!inherits(cimis_sf, "sf")) stop("cimis_sf must be a point sf object")
  if (!"stid" %in% names(cimis_sf)) stop("cimis_sf must have a column called 'stid'")

  conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)

  #epsg_caalbers <- 3310

  ## Get the CIMIS active stations
  # stn_sf <- cc_stns_active(crs = epsg_caalbers)

  ## Create a data frame with just the stid column (will need it below when we convert indices to stids)
  stn_tbl <- cimis_sf |>
    st_drop_geometry() |>
    select(stid) |>
    as_tibble()

  ## Extract the coordinates for the stations
  stn_mat <- st_coordinates(cimis_sf)

  ## Create the query points

  ## Make sure the polygon is also in CA Albers
  poly_prj_sf <- st_transform(poly, st_crs(cimis_sf))

  ## Create a blank raster with 250m pixels
  poly_ext_rst <- terra::rast(poly_prj_sf, res = 250)

  ## Assign the pixels a value (will be overwritten)
  values(poly_ext_rst) <- 1

  ## Mask it to the polygon boundary
  poly_bnd_rst <- terra::mask(poly_ext_rst, poly_prj_sf)

  ## Convert the raster to points (cell centroids)
  poly_pts_sf <- st_as_sf(as.points(poly_bnd_rst))
  ## plot(poly_pts_sf$geometry, axes = TRUE)

  ## Get the coordinates of the query points
  qry_mat <- st_coordinates(poly_pts_sf)

  ## For each cell centroid, find the nearest CIMIS station(s)
  qry_knn_lst <- get.knnx(data = stn_mat, query = qry_mat, k = nn)

  ## Unlist and remove duplicates
  all_idx <- unique(as.vector(unlist(qry_knn_lst$nn.index)))

  ## Return a vector of the station ids
  sort(stn_tbl[all_idx, "stid", drop = TRUE])

}

