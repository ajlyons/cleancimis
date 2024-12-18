#' Predict tree temperature
#'
#' Predict tree temperature
#'
#' @param station Station id, integer of length 1
#' @param nn_list List of nearest neighbors (see \link{cc_stn_nn})
#' @param nn Number of nearest neighbors
#' @param start_dt Start date
#' @param end_dt End date
#' @param pt Orchard point
#' @param treetempgam_rds Path to the Rds file for the GAM model
#' @param data_dir The data directory
#' @param proj_subdir Project subdirectory
#' @param all_cols Return all columns

#' @details
#'
#' This takes the gap filled data, creates additional variables for the tree temperature gam,
#' and uses the gam to predict tree temperature (i.e., bark temperature)
#'
#' This function should be relatively fast (1-2 seconds). It requires that GAP filled data for the
#' station of interest and the period of interest already be available as a RDS file. See also
#' cc_fillgaps.
#'
#' Unlike many of the other functions, cc_treetemp accepts 1 and only 1 station. You must also
#' pass coordinates of the orchard of interest. Presumably, the station passed is the closest
#' CIMIS station to
#'
#' It returns the predicted bark temperature, as well as the chill portions
#' (in a future version, the chill portions could be made optional).
#'
#' @import dplyr
#' @importFrom lubridate yday year date
#' @importFrom tidyr fill replace_na
#' @importFrom conflicted conflict_prefer
#' @importFrom tidyselect all_of
#' @importFrom magrittr set_names
#' @importFrom crayon green red magenta silver
#' @importFrom sf st_transform st_crs st_coordinates
#' @import stringr
#' @import zoo
#' @import oce
#'
#' @export

cc_treetemp <- function(station,
                        nn_list,
                        nn = 3,
                        start_dt,
                        end_dt,
                        pt,
                        treetempgam_rds,
                        data_dir = Sys.getenv("CC_DATADIR"),
                        proj_subdir = "my_project_area",
                        all_cols = FALSE) {

  # library(dplyr)
  # library(lubridate)
  # library(tidyr)
  # library(stringr)
  # library(zoo)
  # library(oce)
  # library(conflicted)

  conflicted::conflicts_prefer(dplyr::filter, dplyr::select, lubridate::yday, .quiet = TRUE)

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  gapfilldata_dir <- file.path(data_dir, proj_subdir, "03_gap_filled")
  if (!dir.exists(gapfilldata_dir)) stop(paste0(gapfilldata_dir, " does not exist"))

  ## Error checking
  if (length(station) > 1) stop("`station` should contain only one station id")
  if (FALSE %in% (station %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  if (start_dt >= end_dt) stop("start_dt must come before end_dt")

  ## Load the GAM model (saved as a RDS file)
  if (file.exists(treetempgam_rds)) {
    gam_T_tree_0 <- readRDS(treetempgam_rds)
  } else {
    stop(paste0("Can't find ", basename(treetempgam_rds)))
  }

  if (is.numeric(pt)) {
    if (length(pt) != 2) stop("Point should be a vector of longitude-latitude coordinates, or a sf object")
    orch_coords <- pt

    # pt_sf <- data.frame(id = 1,
    #                     lon = pt[1],
    #                     lat = pt[2]) |>
    #   st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    #   st_transform(st_crs(poly))

  } else if (inherits(pt, "sf")) {
    if (nrow(pt) != 1) stop("pt should contain one and only one point")
    orch_coords <- st_transform(pt, crs = 4326) |> st_coordinates() |> as.vector()
  }

  ## Check that we have enough nearest neighbors station ids (if nn=3, we need 2)
  stid_nn <- nn_list[[as.character(station)]]$nn
  if (length(stid_nn) < nn - 1) stop("Insufficient number of nearest neighbors identified")

  ## Get only as many as needed
  stid_nn <- stid_nn[1:(nn-1)]

  ## Create a named vector of the station ids
  gap_fill_stns <- c(station, stid_nn) |>
    setNames(LETTERS[1:(length(stid_nn) + 1)])

  ## Construct a RDS file name for this station and its neighbors and years
  ## TODO: We may also want to create meta data file containing the actual date ranges

  ## Find a RDS file with gap-filled data for these stations and the years needed
  gapfilled_rds <- tibble(
    rds_fn = list.files(gapfilldata_dir,
                        pattern = paste0("^",
                                         paste(gap_fill_stns, collapse = "-")))) |>
    mutate(start_year = as.integer(str_split_i(rds_fn, "_", 2)),
           end_year = as.integer(str_split_i(rds_fn, "_", 3)),
           num_years = end_year - start_year) |>
    filter(start_year <= lubridate::year(start_dt),
           end_year >= lubridate::year(end_dt)) |>
    slice_min(num_years, n = 1) |>
    pull(rds_fn)

  if (length(gapfilled_rds) == 0) {
    stop(crayon::red("Could not find an RDS file with gap filled data for the stations and time period required."))

  } else {

    hourly_tbl <- readRDS(file.path(gapfilldata_dir, gapfilled_rds))
    # dim(hourly_tbl)

    ## Construct a 30 minute time series
    complete_dt <- tibble(
      date_time = c(
        seq(
          from = min(hourly_tbl$date_time),
          to = max(hourly_tbl$date_time),
          by = 1800
        )
      )
    )
    # complete_dt

    ## Define the columns (i.e., variables) where the 30-minute values will
    ## be filled in using divide (??). There are the columns for et and ppt
    cols_2_divide <- names(hourly_tbl)  |>
      grep("_ppt_|_et_", x = _, value = TRUE)

    ## Define the columns (i.e., variables) where the 30-minute values will
    ## be estimated using a spline. There are the columns for et and ppt
    ## these include all the columns not in cols_2_divide
    ## (i.e., solar radiation, air temp, relative humidity, wind, soil temp)
    cols_2_spline <- names(hourly_tbl)  |>
      grep("^st_", x = _, value = TRUE)  |>
      dplyr::setdiff(cols_2_divide)

    ## Full-join the half hour values, and then fill (interpolate) the
    ## missing vaules (at the 0:30) using the divide method or spline method

    half_hourly_tbl <- full_join(
      complete_dt,
      hourly_tbl,
      by = join_by(date_time)
    ) |>
      tidyr::fill(                                    ## for cols_2_divide, fill in the
        tidyselect::all_of(cols_2_divide),            ##  --:30 rows with the value from
        .direction = "updown"                         ## the following row
      ) |>
      mutate(
        across(
          .cols = tidyselect::all_of(cols_2_spline),  ## for cols_2_spline, use a spline
          .fns = na.spline,                           ## function to interpolate the NA
          .names = "{.col}"                           ## values
        ),
        across(
          .cols = tidyselect::all_of(cols_2_divide),  ## for cols_2_divide (which are defined
          .fns = ~.x/2,                               ## as hourly accumulations), divide everything
          .names = "{.col}"                           ## in half
        )
      )

    ## Identify elements of gap_fill_stns (a named vector) to drop (i.e., 4th onward)
    drop_stations <- gap_fill_stns[4:length(gap_fill_stns)]

    ## Create a named character vector A:C
    stn_letter4number <- LETTERS[1:3] |>
      magrittr::set_names(
        paste0("stn", gap_fill_stns[1:3])
      )

    ## For the time period of interest, compute all the variables needed
    ## for tree temperature GAM
    half_hourly_4pred_tbl <- half_hourly_tbl |>
      dplyr::filter(
        # date_time %within% crop_intrvl
        date_time >= start_dt, date_time <= end_dt
      ) |>
      dplyr::select(
        -contains(
          paste0(
            "stn",
            drop_stations
          )
        )
      ) |>
      mutate(
        date_time4sun = date_time + 8 * 3600,
        orch_sun_azmth = sunAngle(
          t = date_time4sun,
          latitude = orch_coords[2], #orch_Lat,
          longitude = orch_coords[1], ##orch_Long,
          useRefraction = TRUE
        )$azimuth |> unname(),
        orch_sun_alt = sunAngle(
          t = date_time4sun,
          latitude = orch_coords[2], #orch_Lat,
          longitude = orch_coords[1], ##orch_Long,
          useRefraction = TRUE
        )$altitude |> unname(),
        orch_sun_alt_day = ifelse(
          orch_sun_alt < 0,
          0,
          orch_sun_alt
        ),
        doy = yday(date_time),
        date = lubridate::date(date_time)
      ) %>%
      magrittr::set_names( ### 7. Put stations into A, B, C column names.===
        names(.) |>
          str_replace_all(stn_letter4number)
      ) |>
      arrange(date_time) %>%
      mutate( # 6. Calculate moving averages and lagged variables, etc. ===
        dec_hour = hour(date_time) / 24,
        date = date(date_time),
        st_ppt_ma_A = (dplyr::lag(st_ppt_mm_A) +
                         dplyr::lag(st_ppt_mm_A, 2) +
                         dplyr::lag(st_ppt_mm_A, 3) +
                         dplyr::lag(st_ppt_mm_A, 4)) / 4,
        st_ppt_ma_B = (dplyr::lag(st_ppt_mm_B) +
                         dplyr::lag(st_ppt_mm_B, 2) +
                         dplyr::lag(st_ppt_mm_B, 3) +
                         dplyr::lag(st_ppt_mm_B, 4)) / 4,
        st_ppt_ma_C = (dplyr::lag(st_ppt_mm_C) +
                         dplyr::lag(st_ppt_mm_C, 2) +
                         dplyr::lag(st_ppt_mm_C, 3) +
                         dplyr::lag(st_ppt_mm_C, 4)) / 4,
        st_et_mm_A_lag1 = dplyr::lag(st_et_mm_A, 1),
        st_et_mm_A_lag2 = dplyr::lag(st_et_mm_A, 2),
        st_et_mm_A_lag3 = dplyr::lag(st_et_mm_A, 3),
        st_slr_A_lag1 = dplyr::lag(st_slr_w_m2_A, 1),
        st_slr_A_lag2 = dplyr::lag(st_slr_w_m2_A, 2),
        st_slr_A_lag3 = dplyr::lag(st_slr_w_m2_A, 3),
        st_t_air_A_lag1 = dplyr::lag(st_t_air_C_A, 1),
        st_t_air_A_lag2 = dplyr::lag(st_t_air_C_A, 2),
        st_t_air_A_lag3 = dplyr::lag(st_t_air_C_A, 3),
        st_t_soil_A_lag1 = dplyr::lag(st_t_soil_C_A, 1),
        st_t_soil_A_lag2 = dplyr::lag(st_t_soil_C_A, 2),
        st_t_soil_A_lag3 = dplyr::lag(st_t_soil_C_A, 3),
        st_wind_A_lag1 = dplyr::lag(st_wind_m_s_A, 1),
        st_wind_A_lag2 = dplyr::lag(st_wind_m_s_A, 2),
        st_wind_A_lag3 = dplyr::lag(st_wind_m_s_A, 3),
        orch = factor("any_orch"),                       # for gam pred function to work
        crop_year = factor("any_year")                   # ask Emlio about this. If crop_year is simply
                                                         # for the purposes of aggregated computed chill portions,
                                                         # it could be computed in a later step
      ) |>
      group_by(doy) %>%
      mutate(
        cum_slr_A = cumsum(
          replace_na(st_slr_w_m2_A, 0)
        )
      ) %>%
      ungroup() %>%
      stats::na.omit()

    # class(half_hourly_4pred_tbl)
    # ## Expected number of rows:
    # half_hourly_4pred_tbl$date_time |> range() |> diff() |> as.numeric() * 48
    # ## Actual number of rows:
    # dim(half_hourly_4pred_tbl)     ## num rows for one year of data is 365 * 48 = 17520
    # names(half_hourly_4pred_tbl)

    # Predict half-hourly bark temperature
    half_hourly_treetemp_tbl <- half_hourly_4pred_tbl %>%
      mutate(
        pred_avg_t_tree = predict(
          gam_T_tree_0,
          newdata = .,
          exclude = c(
            "s(orch,crop_year)"
          )
        ) %>%
          as.numeric()
      ) %>%
      arrange(
        orch,
        crop_year,
        date_time
      ) %>%
      group_by(
        orch,
        crop_year
      ) %>%
      mutate(
        chillp_pred_Ttree = cc_chill_portion(pred_avg_t_tree, delta_t_h = 0.5),
        cum_chill_pTtree = cumsum(replace_na(chillp_pred_Ttree, 0)),
      ) |>
      ungroup()           ## I added this - check with Emilio

    # half_hourly_treetemp_tbl |> slice(1:100) |> View()

    if (all_cols) {
      half_hourly_treetemp_tbl
    } else {
      #names(half_hourly_treetemp_tbl )

      half_hourly_treetemp_tbl |>
        mutate(station = .env$station) |>
        select(station, crop_year, date_time, pred_avg_t_tree, chillp_pred_Ttree, cum_chill_pTtree)

    }

  }




}






