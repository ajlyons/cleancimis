#' Produce time series statistics for gappy data
#'
#' Produce time series statistics for gappy data
#'
#' @param year Year(s), integer
#' @param data_dir The data directory
#' @param proj_subdir Project subdirectory
#' @param stations The station IDs to stack, integers
#'
#' @details
#' This will look at the gappy data and summarize the number of records per month.
#' This can help you determine how much missing data there are.
#'
#' @importFrom dplyr filter mutate group_by summarise n
#' @importFrom lubridate year month as_date floor_date
#' @importFrom conflicted conflicts_prefer
#' @importFrom data.table rbindlist
#' @export

cc_stn_stats <- function(year,
                         stations,
                         data_dir = Sys.getenv("CC_DATADIR"),
                         proj_subdir = "my_project_area") {

  # library(dplyr)
  # library(data.table)
  # library(lubridate)
  # library(conflicted)

  if (FALSE %in% (year %in% 1982:2024)) stop("year must be between 1982-2024")

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  rds_dir <- file.path(data_dir, proj_subdir, "02_gappy")
  if (!dir.exists(rds_dir)) dir.create(rds_dir, recursive = TRUE)
  if (!dir.exists(rds_dir)) stop(paste0(rds_dir, " does not exist"))

  conflicted::conflicts_prefer(dplyr::filter, lubridate::year, lubridate::month, .quiet = TRUE)

  res <- character(0)

  import_rds <- file.path(rds_dir, paste0("hourly", year, ".rds"))

  if (FALSE %in% file.exists(import_rds)) {
    stop(paste0("Missing file(s): ",
                paste(basename(import_rds)[!file.exists(import_rds)], collapse = ", ")))
  }

  allvals_tbl <- data.table::rbindlist(lapply(import_rds, readRDS))
  #allvals_tbl |> head()

  allvals_sum_tbl <- allvals_tbl |>
    mutate(station = as.integer(station_n)) |>
    filter(station %in% stations) |>
    group_by(station,
             year_month = as_date(floor_date(date_time, unit = "month"))) |>
    summarise(num_recs = n(), .groups = "drop")

  # allvals_sum_tbl <- allvals_tbl |>
  #   # select(station_n, date_time)
  #   mutate(station = as.integer(station_n)) |>
  #   filter(station %in% stations) |>
  #   group_by(station,
  #            year = as.integer(year(date_time)),
  #            month = as.integer(month(date_time))) |>
  #   summarise(num_recs = n(), .groups = "drop")

  # dim(allvals_sum_tbl)
  # allvals_sum_tbl |> View()

  allvals_sum_tbl


}






