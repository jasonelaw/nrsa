\name{nWadeableStationsPerTransect}
\alias{nWadeableStationsPerTransect}
\title{Estimates the intended number of wadeable thalweg stations to be visited at
an NRSA study site.}
\usage{
  nWadeableStationsPerTransect(uid, transect, station)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{transect}{a vector of transect identifiers}

  \item{station}{a vector of station identifiers}
}
\description{
  Estimates the intended number of wadeable thalweg
  stations at each transect which are considered sampled
  (even if there is no data) for the purposes of
  calculating residual pools and channel lengths.  The
  number of stations at a transect is calculated as the
  greater of either the number of stations occuring in the
  dataframe for that transect, or the most common count of
  stations (i.e. station mode) occuring at that site.
}
\details{
  It takes advantage of the fact that stations are numeric,
  and thus that the last station to be expected for a
  transect is the number of transects to be expected at
  that transect, adding 1 to the station to account for
  station numbering starting at 0.  Transect K always has 1
  station.
}
\examples{
d <- expand.grid(uid = 1:10, transects = LETTERS[1:11], station = 0:9)
nWadeableStationsPerTransect(d$uid, d$transects, d$station)
}

