#' Download annual compilations of hourly data from CIMIS
#'
#' Download annual compilations of hourly data from CIMIS
#'
#' @param year year(s), integer
#' @param data_dir Data directory
#' @param overwrite Overwrite files already downloaded
#' @param sftp_user A username for the CIMIS SFTP server
#' @param sftp_pwd A password for the CIMIS SFTP server
#' @param unzip Unzip archives when needed, logical
#' @param stations The station IDs to unzip
#' @param keep_zip Keep the zip file, logical
#'
#' @details
#' This will download yearly aggregations of hourly data for all CIMIS stations from the CIMIS SFTP server. As of December 2024,
#' yearly aggregations are available for 1982-2023. To get a username and password for the CIMIS sftp server, contact CIMIS (see their website).
#'
#' The files come down as zips (20-25MB) (1982-2022), or annual CSV files (2023). Files will be saved to the data directory.
#'
#' @seealso [cc_dwnhly_mth()]
#' @importFrom zip unzip zip_list
#' @importFrom crayon green red magenta silver
#'
#' @export

# FileZilla settings:
#   Protocol: SFTP
#   Host: sftpcimis.water.ca.gov
#   Port: leave blank
#   Logon type: Normal (or Ask for Password)
#   User: sftpcimis
#   Password: email CIMIS

## Directory on server:
## Hourly Data - Annual compilations (as CSV)
## /pub2/annualMetric
## example: hourlyStns2012.zip

## sftp://sftpcimis@sftpcimis.water.ca.gov/pub2/annualMetric/hourlyStns2014.zip

cc_dwnhly_yr <- function(year,
                         data_dir = Sys.getenv("CC_DATADIR"),
                         overwrite = FALSE,
                         sftp_user = Sys.getenv("CIMIS_SFPT_USR"),
                         sftp_pwd = Sys.getenv("CIMIS_SFPT_PWD"),
                         unzip = FALSE,
                         stations = NULL,
                         keep_zip = TRUE) {

  if (FALSE %in% (year %in% 1982:2023)) stop("year must be one or more integers between 1982-2022")

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  zip_dir <- file.path(data_dir, "zips", "annualMetric")
  if (!dir.exists(zip_dir)) dir.create(zip_dir, recursive = TRUE)

  csv_dir <- file.path(data_dir, "csvs", "annualMetric")
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
  if (unzip && !dir.exists(csv_dir)) stop(paste0(csv_dir, " does not exist"))

  if (!requireNamespace("sftp", quietly = TRUE)) stop("sftp is a required package")
  ## remotes::install_github("stenevang/sftp")

  if (sftp_user == "") stop("You must provide a username for the sftp server")

  if (!is.null(stations)) {
    if (FALSE %in% (stations %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  }

  res <- list(zip = character(0), csv = character(0))

  ## 2009-2020: 2020hourly088.csv
  ## 2021-2022: hlymet035.csv
  ## This is not going to cut it:

  # csv_base <- list(
  #   `2022` = "hlymet",
  #   `2021` = "hlymet",
  #   `2020` = "hourly",   ## 2020hourly088.csv
  # )

  ## Download the zip file (which contains hourly data for one year for all CIMIS stations)
  cimis_sftp <- sftp::sftp_connect(server = "sftpcimis.water.ca.gov",
                                   folder = "pub2/annualMetric",
                                   username = Sys.getenv("CIMIS_SFPT_USR"),
                                   password = Sys.getenv("CIMIS_SFPT_PWD"))

  ## Get a FTP directory listing
  annual_metric_ftp_fn <- cc_sftp_files(cimis_sftp)

  for (i in 1:length(year)) {

    if (year[i] <= 2022) {

      ## There should be a zip file
      zip_fn <- paste0("hourlyStns", year[i], ".zip")
      local_zip_fn <- file.path(zip_dir, zip_fn)

      if (file.exists(local_zip_fn) && !overwrite) {
        message(crayon::green(paste0(" - Zip file found: ", zip_fn)))
        ok_to_unzip <- TRUE
        res$zip <- c(res$zip, zip_fn)

      } else {
        message(crayon::green(paste0(" - Going to try to download ", zip_fn)))

        download_successful <- sftp::sftp_download(file = zip_fn, tofolder = zip_dir,
                                             sftp_connection = cimis_sftp)

        if (as.logical(download_successful)) {
          message(crayon::green(paste0(" - downloaded ", zip_fn)))
          ok_to_unzip <- TRUE
          res$zip <- c(res$zip, zip_fn)

        } else {
          warning(crayon::red(paste0(" - failed to download ", zip_fn)))
          ok_to_unzip <- FALSE
        }
      }

      if (unzip && ok_to_unzip) {

        filesinzip <- zip::zip_list(local_zip_fn)[["filename"]]

        if (is.null(stations)) {

          ## Will unzip all of them
          csvs2unzip <- filesinzip

          zip::unzip(zipfile = local_zip_fn,
                     exdir = csv_dir,
                     overwrite = TRUE)

          message(crayon::green(paste0(" - all csvs unzipped")))

          if (year[i] %in% c(2016, 2021, 2022)) {
            csvs_names_final <- paste0(year[i], "hourly", gsub("^hlymet", "", csvs2unzip))
          } else {
            csvs_names_final <- csvs2unzip
          }

        } else {

          ## Stations Not Null

          if (year[i] %in% c(2016, 2021, 2022)) {
            csvs2unzip <- paste0("hlymet", sprintf("%03d", stations), ".csv")
            csvs_names_final <- paste0(year[i], "hourly", sprintf("%03d", stations), ".csv")

          } else {
            csvs2unzip <- paste0(year[i], "hourly", sprintf("%03d", stations), ".csv")
            csvs_names_final <- csvs2unzip
          }

          csvs_available_yn <- csvs2unzip %in% filesinzip

          if (FALSE %in% (csvs_available_yn)) {
            message(crayon::magenta(paste0(" - stations missing from ", year[i], ": ",
                                           paste(csvs2unzip[!csvs_available_yn], collapse = ", "))))

            csvs2unzip <- csvs2unzip[csvs_available_yn]
            csvs_names_final <- csvs_names_final[csvs_available_yn]

          }

          zip::unzip(zipfile = local_zip_fn,
                     files = csvs2unzip,
                     exdir = csv_dir,
                     overwrite = TRUE)

          message(crayon::green(paste0(" - ", length(csvs2unzip), " csvs unzipped")))

        }

      }   ## if unzip and ok_to_unzip

      ## Have to rename files from 2021=2022 onward
      if (year[i] %in% c(2016, 2021, 2022)) {

        file.rename(from = file.path(csv_dir, csvs2unzip),
                    to = file.path(csv_dir, csvs_names_final))

        message(crayon::green(paste0(" - ", length(csvs2unzip), " csv files renamed for uniformity")))

      }


      res$csv <- c(res$csv, csvs_names_final)

      if (!keep_zip) {
        unlink(local_zip_fn)
      }

    } else if (year[i] == 2023) {

      ## No zip, these are just csvs

      if (is.null(stations)) {

        ## We want all 2023hourlys*.csv files from the directory listing
        csvs_needed <- grep(paste0(year[i], "hourly"), annual_metric_ftp_fn, value = TRUE)

      } else {

        ## Stations are provided. Construct a list of CSVs filenames
        csvs_needed <- paste0(year[i], "hourly", sprintf("%03d", stations), ".csv")

        ## If any are missing, show a message
        if (FALSE %in% (csvs_needed %in% annual_metric_ftp_fn)) {
          message(crayon::magenta(paste0(" - stations missing from ", year[i], ": ",
                                         paste(csvs_needed[!csvs_needed %in% annual_metric_ftp_fn],
                                               collapse = ", "))))
          csvs_needed <- csvs_needed[csvs_needed %in% annual_metric_ftp_fn]
        }

      }  ## stations not null

      ## See what has already been downloaded
      csvs_needed_alrdygot_yn <- csvs_needed %in% list.files(path = csv_dir, pattern = ".csv$")

      if (sum(csvs_needed_alrdygot_yn) > 0) {
        message(crayon::green(paste0(" - ", sum(csvs_needed_alrdygot_yn),
                                     " files already downloaded")))
      }

      if (FALSE %in% csvs_needed_alrdygot_yn) {
        download_successful <- sftp::sftp_download(file = csvs_needed[!csvs_needed_alrdygot_yn],
                                             tofolder = csv_dir,
                                             sftp_connection = cimis_sftp)
      }

      res$csv <- c(res$csv, csvs_needed)

    }


  }

  invisible(res)

}

#' List files
#' @param sftp_con A sftp connection object
#' @importFrom crayon green

cc_sftp_files <- function(sftp_con) {

  if (!requireNamespace("sftp")) stop("sftp is a required package")

  files_on_server_rds <- file.path(tempdir(),
                                     paste0(gsub("\\.", "-", sftp_con$server),
                                            "_",
                                            gsub("/", "-", sftp_con$folder),
                                            "_ls.Rds"))

  if (file.exists(files_on_server_rds)) {
    message(crayon::green(" - loading a saved FTP directory listing"))
    files_on_server_fn <- readRDS(files_on_server_rds)

  } else {
    message(crayon::green(" - reading the FTP directory"))
    files_on_server_fn <- sftp::sftp_listfiles(sftp_con, verbose = FALSE)[["name"]]
    saveRDS(files_on_server_fn, files_on_server_rds)

  }

  ## Return the list of files
  files_on_server_fn
}






