% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_stn_stats.R
\name{cc_stn_stats}
\alias{cc_stn_stats}
\title{Produce time series statistics for gappy data}
\usage{
cc_stn_stats(
  year,
  stations,
  data_dir = Sys.getenv("CC_DATADIR"),
  proj_subdir = "my_project_area"
)
}
\arguments{
\item{year}{Year(s), integer}

\item{stations}{The station IDs to stack, integers}

\item{data_dir}{The data directory}

\item{proj_subdir}{Project subdirectory}
}
\description{
Produce time series statistics for gappy data
}
\details{
This will look at the gappy data and summarize the number of records per month.
This can help you determine how much missing data there are.
}
