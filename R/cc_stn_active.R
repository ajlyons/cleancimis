#' Return active CIMIS Stations
#'
#' Return active CIMIS Stations
#'
#' @param data_dir The cache directory
#' @param crs The EPSG ID for the returned simple feature point layer
#' @param overwrite Overwrite the saved active stations file
#'
#' @details
#' The default value for \code{crs} = 3310 is California Teale Albers.
#'
#' @return A sf point layer containing the active CIMIS stations
#'
#' @importFrom httr2 request resp_body_json req_perform
#' @importFrom purrr map_chr
#' @importFrom conflicted conflicts_prefer
#' @import dplyr
#' @importFrom sf st_read st_as_sf st_transform st_write
#' @import crayon
#' @importFrom stringr str_extract
#' @export

cc_stn_active <- function(data_dir = Sys.getenv("CC_DATADIR"), crs = 3310, overwrite = FALSE) {

  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory"))
  stn_geojson <- file.path(data_dir, "cimis_stn.geojson")

  if (file.exists(stn_geojson) && !overwrite) {
    message(crayon::green(" - loading stored copy of cimis stations"))
    ## Read the geojson file and return
    sf::st_read(stn_geojson, quiet=TRUE)

  } else {
    message(crayon::green(" - downloading active cimis stations"))

    stn_resp <- req_perform(request("https://et.water.ca.gov/api/station"))

    if (resp_status(stn_resp) != 200) stop("CIMIS API error")

    stn_lst <- stn_resp |> resp_body_json()

    stn_tbl <- tibble(
      stid = as.integer(purrr::map_chr(stn_lst$Stations, "StationNbr")),
      name = purrr::map_chr(stn_lst$Stations, "Name"),
      city = purrr::map_chr(stn_lst$Stations, "City"),
      county = purrr::map_chr(stn_lst$Stations, "County"),
      hmslon_chr = purrr::map_chr(stn_lst$Stations, "HmsLongitude"),
      hmslat_chr = purrr::map_chr(stn_lst$Stations, "HmsLatitude"),
      is_active_chr = purrr::map_chr(stn_lst$Stations, "IsActive")) |>

      mutate(active = (is_active_chr == "True")) |>

      mutate(lon = as.numeric(stringr::str_extract(hmslon_chr, "(?<=/ ).*")),
             lat = as.numeric(stringr::str_extract(hmslat_chr, "(?<=/ ).*"))) |>

      select(-hmslon_chr, -hmslat_chr, -is_active_chr) |>
      filter(active)

    # stn_tbl |> head()

    ## Create a projected sf object
    stn_sf <- stn_tbl |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      st_transform(crs = crs)

    ## Save it to the temp folder
    if (use_cache) st_write(stn_sf, dsn = stn_geojson, quiet = TRUE)

    ## Return the sf object
    stn_sf
  }

}






