#' Fill missing values in CIMIS stations
#'
#' Fill missing values from CIMIS station data
#'
#' @param stations description
#' @param nn_list list of nearest neighbors (see \link{cc_stn_nn})
#' @param nn Number of nearest neighbors
#' @param start_year Start year, integer
#' @param end_year End year, integer
#' @param data_dir The data directory
#' @param proj_subdir Project subdirectory
#' @param save_forest Whether to save the random forest model as a Rds file
#' @param overwrite Overwrite existing files, logical
#' @param processing_csv A csv file to write the processing time to (for testing)
#'
#' @details
#' This will impute NA values in CIMIS time series data using RandomForests.
#' It does using the data from a CIMIS station and its 3 nearest neighbors.
#' The outputs are saved as RDS files.
#' It processes the 'gappy' data output from cc_csv2rds
#' - these are usually annual collections of data for a subset of stations, although they could be incomplete
#' If save_forest = TRUE, the model is saved (as well)
#'
#' @seealso [cc_stn_nn()]
#' @importFrom dplyr filter select tibble bind_rows
#' @importFrom lubridate hour as_date
#' @importFrom tictoc tic toc
#' @importFrom readr write_csv read_rds
#' @importFrom conflicted conflicts_prefer
#' @importFrom crayon green red magenta silver
#' @importFrom tidyr pivot_wider starts_with
#' @importFrom tsibble as_tsibble fill_gaps
#' @importFrom purrr map
#' @import missRanger
#'
#' @export

cc_fillgaps <- function(stations,
                        nn_list,
                        nn = 4,
                        start_year,
                        end_year,
                        data_dir = Sys.getenv("CC_DATADIR"),
                        proj_subdir = "my_project_area",
                        save_forest = FALSE,
                        overwrite = FALSE,
                        processing_csv = NULL) {

  # library(dplyr)
  # library(lubridate)
  # library(tidyr)
  # library(tsibble)
  # library(tictoc)
  # library(readr)
  #library(zoo)
  # library(missRanger)
  #library(oce)
  #library(conflicted)

  conflicted::conflicts_prefer(dplyr::filter, dplyr::select, lubridate::hour, .quiet = TRUE)

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  gappydata_dir <- file.path(data_dir, proj_subdir, "02_gappy")
  if (!dir.exists(gappydata_dir)) stop(paste0(gappydata_dir, " does not exist"))

  gapfilldata_dir <- file.path(data_dir, proj_subdir, "03_gap_filled")
  if (!dir.exists(gapfilldata_dir)) dir.create(gapfilldata_dir, recursive = TRUE)
  if (!dir.exists(gapfilldata_dir)) stop(paste0(gapfilldata_dir, " does not exist"))

  if (save_forest) {
    forests_dir <- file.path(data_dir, proj_subdir, "04_forests")
    if (!dir.exists(forests_dir)) dir.create(forests_dir, recursive = TRUE)
    if (!dir.exists(forests_dir)) stop(paste0(forests_dir, " does not exist"))
  }

  if (FALSE %in% (stations %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  if (start_year > end_year) stop("start_year must be equal to or less than end_year")
  if (FALSE %in% c(start_year, end_year) %in% 1982:2024) stop("valid years are 1982 - 2024")

  ## Initialize the object that will eventually be returned
  ## (a list containing the file names of rds files found or created)
  res <- list(gapfilled = character(0), forests = character(0))

  years_needed <- start_year:end_year

  rds_needed_fn <- file.path(gappydata_dir, paste0("hourly", years_needed, ".rds"))
  if (FALSE %in% file.exists(rds_needed_fn)) {
    stop("Can't find one or more gappy data rds files")
  }

  ## Loop thru the stations
  for (i in 1:length(stations)) {

    start_time <- Sys.time()

    stid_this <- stations[i]
    message(crayon::green(paste0("\n - Gap filling the data for CIMIS station #", stid_this)))

    ## Check that we have enough nearest neighbors station ids (if nn=4, we need 3)
    stid_nn <- nn_list[[as.character(stations[i])]]$nn
    if (length(stid_nn) < nn - 1) stop("Insufficient number of nearest neighbors provided")

    ## Get only as many as needed
    stid_nn <- stid_nn[1:(nn-1)]

    ## Create a named vector of the station ids
    gap_fill_stns <- c(stid_this, stid_nn) |>
      setNames(LETTERS[1:(length(stid_nn) + 1)])

    ## Construct RDS filenames for this station and its neighbors and years
    ## TODO: We may also want to create meta data file containing the actual date ranges
    stids_years_chr <- paste0(paste(gap_fill_stns, collapse = "-"), "_",
                              paste(range(years_needed), collapse = "_"))

    gapfilled_rds <- file.path(gapfilldata_dir,
                               paste0(stids_years_chr, "_gapfilled.rds"))

    if (save_forest) {
      forests_rds <- file.path(forests_dir,
                                 paste0(stids_years_chr, "_forests.rds"))
      } else {
      forests_rds <- NA
    }

    # treetempgam_rds <- file.path(treetempgam_dir,
    #                            paste0(stids_years_chr, "_treetempgam.rds"))


    all_files_found_yn <- file.exists(gapfilled_rds) &&
      ifelse(save_forest, file.exists(forests_rds), TRUE)

    if (all_files_found_yn && !overwrite) {

      message(crayon::green(paste0(" - found gap filled data for #", stid_this, ". Skipping")))
      res$gapfilled <- c(res$gapfilled, gapfilled_rds)

      if (save_forest) {
        message(crayon::green(paste0(" - found forests for #", stid_this, ". Skipping")))
        res$forests <- c(res$forests, forests_rds)
      }

      # message(crayon::green(paste0(" - found tree temp gam data for #", stid_this, ". Skipping")))
      # res$treetempgam <- c(res$treetempgam, treetempgam_rds)

    } else {

      ## First, we:
      ## 1. import the rds data for individual years (i.e., files are output by cc_csv2rds)
      ## 2. keep only the rows for this set of stations
      ## 3. append them into a single tibble,
      ## 4. mutate a couple of columns

      years_comb_tbl <- purrr::map(rds_needed_fn,
                               ~read_rds(.x)) |> bind_rows()

      if (FALSE %in% (gap_fill_stns %in% unique(years_comb_tbl$station_n))) {
        stop("One or more stations are not available in the gappy data")
      }

      hourly_tbl <- years_comb_tbl |>
        bind_rows() |>
        dplyr::filter(station_n %in% gap_fill_stns) |>
        dplyr::select(
          -date,
          -hour_pst,
          -st_dew_point_C,
          -wind_dir,
          -vp
        )  |>
        unique() |>
        mutate(
          station_n = factor(station_n),
          fyear = factor(year(date_time)),
          hod = hour(date_time)
        )

      cat("RIGHT HERE, WE NEED TO CHECK THAT THE TIME SERIES FOR THE 3 NEAREST
          STATIONS DOESN'T EXTEND BEYOND THE STATION OF INTEREST. OTHERWISE
          WE'LL BE INTERPOLATING MISSING VALUES BEYOND THE DATA RECORD.
          ASK EMILIO IF THIS IS REASONABLE. CAN RANDOM FORESTS INTERPOLATE
          GAPS IN THE DATA? \n")

      ################################
      ## Go to a wide format, with columns for each station-variable
      ## Then convert it to a tsibble, and "fill gaps" (??)

      hourly_w_tbl <- hourly_tbl |>
        pivot_wider(
          names_from = station_n,
          values_from = starts_with("st_"),
          names_prefix = "stn",
          values_fill = NA
        ) |>
        as_tsibble(
          index = date_time,
          key = NULL
        ) |>
        fill_gaps()

      # dim(hourly_w_tbl)

      # Impute missing values with missRanger (3 iterations). For 3 years of data,
      ## data_only - takes ~8-10 minutes
      ## save forests - 30 minutes

      if (save_forest) {

        tictoc::tic(msg = "Going to run missRanger and keep the forests")
        hourly_missrngr_msrg <- missRanger(
          data = dplyr::select(hourly_w_tbl, starts_with("st_")),
          data_only = FALSE,
          keep_forests = TRUE)
        #toc()
        tictoc::toc(func.toc = toc.outmsg_min)

        ## Save the gapfilled data (tbl only) as rds
        hourly_tbl <- hourly_missrngr_msrg$data

        saveRDS(hourly_tbl, gapfilled_rds)

        ## For testing purposes, try different levels of compression
        saveRDS(hourly_tbl, gsub(".rds$", ".bzip2.rds", gapfilled_rds), compress = "bzip2")
        saveRDS(hourly_tbl, gsub(".rds$", ".xz.rds", gapfilled_rds), compress = "xz")

        res$gapfilled <- c(res$gapfilled, gapfilled_rds)
        message(crayon::green(paste0(" - saved gapfilled data: ", basename(gapfilled_rds))))

        ## Save the missRanger object including the forests as rds
        saveRDS(hourly_missrngr_msrg, forests_rds)
        saveRDS(hourly_missrngr_msrg, gsub(".rds$", ".bzip2.rds", forests_rds), compress = "bzip2")
        saveRDS(hourly_missrngr_msrg, gsub(".rds$", ".xz.rds", forests_rds), compress = "xz")

        res$forests <- c(res$forests, forests_rds)
        message(crayon::green(paste0(" - saved forests: ", basename(forests_rds))))

      } else {

        ## Data only
        tic(msg = "Going to run missRanger (data_only = TRUE)")
        hourly_tbl <- missRanger(
          data = dplyr::select(hourly_w_tbl, starts_with("st_")),
          data_only = TRUE)

        # toc()
        toc(func.toc = toc.outmsg_min)

        saveRDS(hourly_tbl, gapfilled_rds)
        saveRDS(hourly_tbl, gsub(".rds$", ".bzip2.rds", gapfilled_rds), compress = "bzip2")
        saveRDS(hourly_tbl, gsub(".rds$", ".xz.rds", gapfilled_rds), compress = "xz")

        res$gapfilled <- c(res$gapfilled, gapfilled_rds)
        message(crayon::green(paste0(" - saved ", basename(gapfilled_rds))))
      }

      # hourly_missrngr_msrg$data$date_time |> range()
      # hourly_missrngr_msrg |> object.size() |> format(units="Mb")
      # hourly_missrngr_msrg$data |> object.size() |> format(units="Mb")
      # hourly_missrngr_tbl <- hourly_missrngr_msrg$data
      # format(object.size(hourly_missrngr_tbl), units="Mb")    ## data_only = TRUE, 5.6 Mb

      message(crayon::green(paste0(" - DONE WITH CIMIS station #", stid_this, ". Summary:")))

      ## Compute a tibble with the processing times and outputs
      processing_tbl <- tibble(
        dt = format(Sys.time(), "%Y-%m-%d %H:%M"),
        stid = stid_this,
        nn = paste(stid_nn, collapse = ","),
        start_year = start_year,
        end_year = end_year,
        start_date = as_date(min(hourly_tbl$date_time)),    ## annual collections many be incomplete
        end_date = as_date(max(hourly_tbl$date_time)),
        num_recs = nrow(hourly_tbl),
        save_forest = save_forest,
        processing_time_min = round(difftime(Sys.time(), start_time, units="mins"), 1),
        gapfilled_fn = basename(gapfilled_rds),
        gapfill_size_mb = round(file.size(gapfilled_rds) / 1048576, 1),
        forests_fn = ifelse(save_forest, basename(forests_rds), ""),
        forests_size_mb = ifelse(save_forest, round(file.size(forests_rds) / 1048576, 1), "")
      )

      ## Print it to the console
      for (i in 1:ncol(processing_tbl)) {
        cat(crayon::silver("    ", names(processing_tbl)[i], ": ", processing_tbl[1,i,drop=T], "\n", sep=""))
      }

      ## Enter a row to the CSV of processing times
      if (!is.null(processing_csv)) {
        trytosave <- try({
          readr::write_csv(processing_tbl, processing_csv, append = TRUE,
                           col_names = !file.exists(processing_csv))}, silent = TRUE)
        if (inherits(trytosave, "try-error")) {
          message(crayon::red(paste0(" - Unable to append a row to ", basename(processing_csv),
                                     ". Is it open in Excel?")))
        }
      }

      #########################################

    }

  }   ## loop thru stations

  message(crayon::green(" - DONE"))

  cat("TODO: return a ggplot or some other summary of the input data availability \n")

  invisible(res)

}

## Custom output function for tictoc
toc.outmsg_min <- function (tic, toc, msg) {

  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste0(round((toc - tic) / 60, 1), " min elapsed")
  } else {
    outmsg <- paste0(msg, ": ", round((toc - tic) / 60, 1), " min elapsed")
  }

  outmsg
}





