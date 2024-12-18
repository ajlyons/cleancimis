#' Stack hourly values for a set of stations that have been downloaded by year
#'
#' Stack hourly values for a set of stations that have been downloaded by year
#'
#' @param year Year(s) to stack, integer
#' @param stations The station IDs to stack, integers
#' @param data_dir Data directory
#' @param proj_subdir Subdirectory of the data directory where the stacked files should go
#' @param overwrite Overwrite files already stacked
#'
#' @details
#' This will import the hourly values for a set of stations that have been downloaded in yearly aggregations, and append them in
#' a single CSV file for the year.
#' This presumes you've already download the annual aggregations of CIMIS data, and unzipped the CSV files of the stations of interest.
#'
#' @importFrom readr read_csv write_csv cols col_character
#' @importFrom dplyr tibble filter pull
#' @importFrom conflicted conflict_prefer
#' @importFrom crayon green silver magenta
#'
#' @export

cc_stack_hly_yr <- function(year,
                            stations = NULL,
                            data_dir = Sys.getenv("CC_DATADIR"),
                            proj_subdir = "my_project_area",
                            overwrite = FALSE) {

  if (FALSE %in% (year %in% 1982:2023)) stop("year must be between 1982-2023")

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  csv_dir <- file.path(data_dir, "csvs", "annualMetric")
  if (!dir.exists(csv_dir)) stop(paste0(csv_dir, " does not exist"))

  out_dir <- file.path(data_dir, proj_subdir, "01_annual_csvs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  if (!dir.exists(out_dir)) stop(paste0(out_dir, " does not exist"))

  if (!is.null(stations)) {
    if (FALSE %in% (stations %in% 1:270)) stop("station must be one or more integers from 1 to 270")
  }

  conflicted::conflicts_prefer(dplyr::filter, dplyr::pull, .quiet = TRUE)

  res <- character(0)

  files_chr <- list.files(csv_dir, pattern = ".csv$")

  files_tbl <- tibble(
    filename = files_chr,
    year = as.integer(substr(files_chr, 1, 4)),
    stid = as.integer(substr(files_chr, 11, 14))
  )

  # i = 1
  for (i in 1:length(year)) {
    this_year <- year[i]

    files2stack_chr <- files_tbl |>
      filter(year == this_year, stid %in% .env$stations) |>
      pull(filename)

    csv_out_fn <- file.path(out_dir, paste0("hourly", this_year, ".csv"))

    proceed_yn <- TRUE

    if (file.exists(csv_out_fn)) {
      if (overwrite) {
        unlink(csv_out_fn)
      } else {
        proceed_yn <- FALSE
        message(crayon::green(paste0(" - ", csv_out_fn, " already exists, skipping")))
      }
    }

    if (proceed_yn) {
      message(crayon::green(paste0(" - concatenating ",
                                   length(files2stack_chr),
                                   " CSVs for ", this_year)))

      for (fn in files2stack_chr) {
        path_fn <- file.path(csv_dir, fn)

        if (file.exists(path_fn)) {

          message(crayon::silver(paste0("    - ", fn)))

          read_csv(path_fn, col_names = FALSE,
                   col_types = cols(.default = col_character())) |>
            write_csv(csv_out_fn, col_names = FALSE, na = "", append = TRUE, progress = FALSE)

        } else {
          message(crayon::magenta(paste0(" - can not find file: ", path_fn)))
        }


      }
    }

    res <- c(res, csv_out_fn)
  }

  invisible(res)

}






