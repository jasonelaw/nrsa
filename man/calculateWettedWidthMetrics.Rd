\name{calculateWettedWidthMetrics}
\alias{calculateWettedWidthMetrics}
\title{Calculates wetted width metrics}
\usage{
  calculateWettedWidthMetrics(uid, wetwid)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{wetwid}{a vector of wetted widths; uses the max of
  the transect - extra transect pair}
}
\value{
  a 'metric' data.frame
}
\description{
  Calculates wetted width metrics
}
\details{
  Uses the max wetted width of the transect - extra
  transect pair as returned by \link{joinExtraTransects}.
}

