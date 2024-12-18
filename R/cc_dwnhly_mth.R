#' Download monthly compilations of hourly data from CIMIS
#'
#' Download monthly compilations of hourly data from CIMIS for the current calendar year
#'
#' @param month months(s), integer
#' @param data_dir Data directory
#' @param overwrite Overwrite files already downloaded
#' @param sftp_user A username for the CIMIS SFTP server
#' @param sftp_pwd A password for the CIMIS SFTP server
#' @param unzip Unzip archives when needed, logical
#' @param stations The station IDs to unzip
#' @param keep_zip Keep the zip file, logical
#'
#' @details
#' This will download hourly data for all CIMIS stations in one month chunks.
#' CIMIS provides monthly compilations for the last 12 months. These monthly archives are
#' typically uploaded to the server on the first of the month (for the previous month).
#' To get a username and password for the CIMIS sftp server, contact CIMIS (see their website).
#'
#' @seealso [cc_dwnhly_mth()]
#'
#' @importFrom zip unzip zip_list
#' @importFrom crayon green red magenta silver
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
## /pub2/monthlyMetric
## example: hourlyStns2012.zip

cc_dwnhly_mth <- function(month,
                         data_dir = Sys.getenv("CC_DATADIR"),
                         overwrite = FALSE,
                         sftp_user = Sys.getenv("CIMIS_SFPT_USR"),
                         sftp_pwd = Sys.getenv("CIMIS_SFPT_PWD"),
                         unzip = TRUE,
                         stations = NULL,
                         keep_zip = TRUE) {

  ## This will download hourly data for all CIMIS stations in one month chunks
  ## These monthly archives are typically posted on the first of the month
  ## The year will be inferred

  if (FALSE %in% (month %in% 1:12)) stop("month must be one or more integers between 1-12")

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  zip_dir <- file.path(data_dir, "zips", "monthlyMetric")
  if (!dir.exists(zip_dir)) dir.create(zip_dir, recursive = TRUE)

  csv_dir <- file.path(data_dir, "csvs", "monthlyMetric")
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
  if (unzip && !dir.exists(csv_dir)) stop(paste0(csv_dir, " does not exist"))

  if (!require(sftp, quietly = TRUE)) stop("sftp is a required package")
  ## remotes::install_github("stenevang/sftp")

  if (sftp_user == "") stop("You must provide a username for the sftp server")

  if (!is.null(stations)) {
    if (FALSE %in% (stations %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  }

  if (unzip && !dir.exists(csv_dir)) stop(paste0(csv_dir, " does not exist"))

  if (!is.null(stations)) {
    if (FALSE %in% (stations %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  }

  res <- list(zip = character(0), csv = character(0))

  ## Download the zip file (which contains hourly data for one year for all CIMIS stations)
  cimis_sftp <- sftp::sftp_connect(server = "sftpcimis.water.ca.gov",
                             folder = "pub2/monthlyMetric",
                             username = Sys.getenv("CIMIS_SFPT_USR"),
                             password = Sys.getenv("CIMIS_SFPT_PWD"))

  ## Get a FTP directory listing
  monthly_metric_ftp_fn <- cc_sftp_files(cimis_sftp)

  for (i in 1:length(month)) {
    month_abbrev <- tolower(month.abb[month[i]])

    month_year <- lubridate::year(Sys.Date()) - ifelse(month[i] < lubridate::month(Sys.Date()), 0, 1)

    zip_server_fn <- paste0("hourlyStns", month_abbrev, ".zip")
    local_zip_fn <- file.path(zip_dir, paste0(month_year, "_hourlyStns", month_abbrev, ".zip"))

    if (file.exists(local_zip_fn) && !overwrite) {
      message(crayon::green(paste0(" - Zip file found: ", local_zip_fn)))
      ok_to_unzip <- TRUE
      res$zip <- c(res$zip, local_zip_fn)

    } else {
      message(crayon::green(paste0(" - Going to try to download ", zip_server_fn)))

      download_successful <- sftp::sftp_download(file = zip_server_fn, tofolder = zip_dir,
                                           sftp_connection = cimis_sftp)

      if (as.logical(download_successful)) {
        file.rename(from = file.path(zip_dir, zip_server_fn),
                    to = local_zip_fn)
        message(crayon::green(paste0(" - downloaded ", local_zip_fn)))
        ok_to_unzip <- TRUE
        res$zip <- c(res$zip, local_zip_fn)

      } else {
        warning(crayon::red(paste0(" - failed to download ", zip_fn)))
        ok_to_unzip <- FALSE
      }
    }

    if (unzip && ok_to_unzip) {

      filesinzip <- zip_list(local_zip_fn)[["filename"]]

      if (is.null(stations)) {

        ## Will unzip all of them
        csvs2unzip <- filesinzip

        zip::unzip(zipfile = local_zip_fn,
                   exdir = csv_dir,
                   overwrite = TRUE)

        message(crayon::green(paste0(" - all csvs unzipped")))

        csvs_names_final <- paste0(month_year, "_", filesinzip)

      } else {

        ## Stations Not Null

        csvs2unzip <- paste0(month_abbrev, "hourly", sprintf("%03d", stations), ".csv")
        csvs_names_final <- paste0(month_year, "_", csvs2unzip)

        csvs_available_yn <- csvs2unzip %in% filesinzip

        if (FALSE %in% (csvs_available_yn)) {
          message(crayon::magenta(paste0(" - stations missing from ", month_abbrev, " zip file: ",
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

    file.rename(from = file.path(csv_dir, csvs2unzip),
                to = file.path(csv_dir, csvs_names_final))

    res$csv <- c(res$csv, csvs_names_final)

  }

  invisible(res)

}






