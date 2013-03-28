\name{formatFishCover}
\alias{formatFishCover}
\title{Format fish cover data for calculations}
\usage{
  formatFishCover(uid, parameter, cover)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{parameter}{a vector parameter codes. These should
  be the original nrsa codes: ALGAE, BOULDR, LVTREE, etc}

  \item{cover}{a vector of cover values (integers 0-4)}
}
\description{
  \code{formatFishCover} formats the fish cover data for
  calculations.  It checks remaps parameter names, converts
  cover codes to proportions and returns a data frame that
  can be used as input to the other fish cover metric
  functions.
}

