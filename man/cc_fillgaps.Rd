% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_fillgaps.R
\name{cc_fillgaps}
\alias{cc_fillgaps}
\title{Fill missing values in CIMIS stations}
\usage{
cc_fillgaps(
  stations,
  nn_list,
  nn = 4,
  start_year,
  end_year,
  data_dir = Sys.getenv("CC_DATADIR"),
  proj_subdir = "my_project_area",
  save_forest = FALSE,
  overwrite = FALSE,
  processing_csv = NULL
)
}
\arguments{
\item{stations}{description}

\item{nn_list}{list of nearest neighbors (see \link{cc_stn_nn})}

\item{nn}{Number of nearest neighbors}

\item{start_year}{Start year, integer}

\item{end_year}{End year, integer}

\item{data_dir}{The data directory}

\item{proj_subdir}{Project subdirectory}

\item{save_forest}{Whether to save the random forest model as a Rds file}

\item{overwrite}{Overwrite existing files, logical}

\item{processing_csv}{A csv file to write the processing time to (for testing)}
}
\description{
Fill missing values from CIMIS station data
}
\details{
This will impute NA values in CIMIS time series data using RandomForests.
It does using the data from a CIMIS station and its 3 nearest neighbors.
The outputs are saved as RDS files.
It processes the 'gappy' data output from cc_csv2rds
\itemize{
\item these are usually annual collections of data for a subset of stations, although they could be incomplete
If save_forest = TRUE, the model is saved (as well)
}
}
\seealso{
\code{\link[=cc_stn_nn]{cc_stn_nn()}}
}
