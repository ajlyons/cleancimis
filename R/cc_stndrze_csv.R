#' Standardize CSVs and flag suspect values
#'
#' Standardize CSVs and flag suspect values
#'
#' @param year Year(s), integer
#' @param data_dir The data directory
#' @param proj_subdir Project subdirectory
#' @param overwrite Overwrite existing files, logical
#'
#' @details
#' This will import the stacked CSV files (created with cc_stack_hly_yr and cc_stack_hly_mth),
#' standardize the column names and data types, convert any values that CIMIS flagged with a quality code
#' to NAs, and save as RDS files. These "gappy" data will then be the input into the next function.
#'
#' @importFrom dplyr mutate across select starts_with
#' @importFrom readr read_csv write_rds
#' @importFrom crayon green silver magenta red
#' @importFrom lubridate mdy_hm
#' @export

cc_stndrze_csv <- function(year,
                           data_dir = Sys.getenv("CC_DATADIR"),
                           proj_subdir = "my_project_area",
                           overwrite = FALSE) {

  if (FALSE %in% (year %in% 1982:2024)) stop("year must be between 1982-2024")

  if(!dir.exists(data_dir)) stop(paste0("Can't find data directory. Please paass a valid directory, or create an environment variable called CC_DATADIR"))

  csv_dir <- file.path(data_dir, proj_subdir, "01_annual_csvs")
  if (!dir.exists(csv_dir)) stop(paste0(csv_dir, " does not exist"))

  rds_dir <- file.path(data_dir, proj_subdir, "02_gappy")
  if (!dir.exists(rds_dir)) dir.create(rds_dir, recursive = TRUE)
  if (!dir.exists(rds_dir)) stop(paste0(rds_dir, " does not exist"))

  ## Create a vector of column names
  hourly_col_names <- c(
    "station_n",
    "date",
    "hour_pst",
    "doy",
    "st_et_mm",
    "qc_et",
    "st_ppt_mm",
    "qc_ppt",
    "st_slr_w_m2",
    "qc_slr",
    "vp",
    "qc_vp",
    "st_t_air_C",
    "qc_tair",
    "st_rh_pcnt",
    "qc_rh",
    "st_dew_point_C",
    "qc_dew",
    "st_wind_m_s",
    "qc_wind",
    "wind_dir",
    "qc_windir",
    "st_t_soil_C",
    "qc_tsoil"
  )

  # Identify QC codes that invalidate data
  # Emilio read all the definitions of the CIMIS codes and made a decision about which
  # quality codes to remove. This included historical averages, simply because we
  # don't know how those were calculcated
  fail_qc_codes <- c("I", "M", "S", "R", "Y")

  for (i in 1:length(year)) {

    csv_fn <- file.path(csv_dir, paste0("hourly", year[i], ".csv"))

    # browser()

    if (file.exists(csv_fn)) {

      rds_fn <- file.path(rds_dir, gsub(".csv$", ".rds", basename(csv_fn)))

      if (file.exists(rds_fn) && !overwrite) {
        message(crayon::magenta(paste0(" - ", basename(rds_fn), " exists, skipping")))

      } else {

        message(crayon::green(paste0(" - processing ", basename(csv_fn))))

        ## Read in the file
        df <- read_csv(
          csv_fn,
          col_names = hourly_col_names,
          col_types = "nccnncncncncncncncncncnc"
        ) |>
          mutate(
            date_time = mdy_hm(paste(date,
                                     substr(hour_pst, 1, 2),
                                     ":",
                                     substr(hour_pst, 3, 4)
            )),
            across(
              c(qc_et, qc_ppt, qc_slr, qc_vp, qc_tair, qc_rh, qc_rh,
                qc_dew, qc_wind, qc_windir, qc_tsoil),
              ~ ifelse(.x %in% fail_qc_codes, NA, .x)
            )
          ) |>
          dplyr::select(
            -starts_with("qc")
          ) |>  unique()

        ##xx <- df |> unique()     ## `--` found in numeric colums > converted to NAs
        # problems(df)
        # df |> slice(886:900) |> View()
        # df |> View()

        write_rds(df, rds_fn)

      }

    } else {
      message(crayon::red(paste0(" - can't find ", basename(csv_fn))))

    }

  }


}






