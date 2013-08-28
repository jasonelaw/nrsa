\name{calculatePropTranInvPresent}
\alias{calculatePropTranInvPresent}
\title{Calculates the invasive species metrics.}
\usage{
  calculatePropTranInvPresent(uid, species, is.present)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{species}{a vector of species ids}

  \item{is.present}{a logical vector (or 0 and 1) which
  identifies whether that species was present at that
  transect}
}
\value{
  a 'metric' data.frame
}
\description{
  Calculates the proportion of transects at which the
  species was observed.  The species id is used in the
  metric name, except species on the original NRSA EPA list
  are mapped to the NRSA metric names.
}

