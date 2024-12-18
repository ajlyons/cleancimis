#' Return a list of CIMIS Station Nearest Neighbors
#'
#' Return a list of CIMIS Station Nearest Neighbors
#'
#' @param cimis_sf A simple feature point object of CIMIS stations
#' @param nn number of nearest neighbors (integer)
#'
#' @return A list
#'
#' @importFrom conflicted conflicts_prefer
#' @importFrom sf st_drop_geometry st_coordinates
#' @importFrom dplyr select as_tibble
#' @importFrom FNN get.knnx
#' @export

## Create a list of nearest neighbors for all CIMIS stations

cc_stn_nn <- function(cimis_sf, nn = 4) {

  if(!inherits(cimis_sf, "sf")) stop("cimis_sf must be a point sf object")
  if (!"stid" %in% names(cimis_sf)) stop("cimis_sf must have a column called 'stid'")

  conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)

  ## Create a data frame with just the stid column (will need it below)
  stn_tbl <- cimis_sf |>
    st_drop_geometry() |>
    select(stid) |>
    as_tibble()

  ## Extract the coordinates for the stations
  stn_mat <- st_coordinates(cimis_sf)

  ## Define the query points
  qry_sf <- cimis_sf
  qry_mat <- st_coordinates(qry_sf)

  ## Find the (indices) of the nn nearest neighbors
  stn_knn_lst <- FNN::get.knnx(data = stn_mat, query = qry_mat, k = nn)

  ## Replace index values in $nn.index values with stid values
  stid_nn_mat <- stn_knn_lst$nn.index
  stid_int <- stn_tbl[as.vector( stn_knn_lst$nn.index), "stid", drop = TRUE]
  stid_nn_mat[] <- stid_int

  ## Return a list
  lapply(1:nrow(stid_nn_mat), function(i)
    list(stid = stid_nn_mat[i,1],
         nn = stid_nn_mat[i,2:nn])) |>
    setNames(as.character(stid_nn_mat[,1]))

}






