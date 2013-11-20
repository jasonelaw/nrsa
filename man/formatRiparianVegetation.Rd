\name{formatRiparianVegetation}
\alias{formatRiparianVegetation}
\title{Format Riparian Vegetation data for calculations}
\usage{
  formatRiparianVegetation(uid, transect, transdir,
    parameter, result)
}
\arguments{
  \item{uid}{a vector of site visit indicators}

  \item{transect}{a vector of transect indicators}

  \item{transdir}{a vector of bank indicators}

  \item{parameter}{the measured parameter code.  One of:
  "underveg", "canveg", "canbtre", "canstre", "undwdy",
  "undnwdy", "gcwdy", "gcnwdy", "bare".}

  \item{result}{the measured result for each parameter.  A
  vegetation class for "underveg" and "canveg" and 0-4 for
  the others.}
}
\value{
  a data frame in wide format with the parameters necessary
  for metric calculation
}
\description{
  In addition to transposing the data to a wide format,
  this function creates two sets of parameters: 'p_' and
  'r_'.  The former are 'presence' parameters and are TRUE
  whenever for any parameter with a nonzero vegetation
  cover class.  The latter parameters are the cover classes
  converted to the arithmetic means of end points of the
  cover classes to numerically characterize each class.
}

