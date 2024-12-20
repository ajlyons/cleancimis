% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_pointinpoly.R
\name{cc_pointinpoly}
\alias{cc_pointinpoly}
\title{Checks if a point falls within in a polygon}
\usage{
cc_pointinpoly(pt, poly)
}
\arguments{
\item{pt}{either be a sf object or a numeric vector of length 2 containing longitude and latitude coordinates}

\item{poly}{a sf polygon object}
}
\description{
Checks if a point falls within in a polygon
}
\details{
This function can be used on a web app to test whether a user-selected point falls within the coverage area.
pt and poly do not have to be in the same crs.
}
