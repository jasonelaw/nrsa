\name{calculateLWDSiteLength}
\alias{calculateLWDSiteLength}
\title{Calculate length of area sampled for LWD}
\usage{
  calculateLWDSiteLength(reachlen, numtran, is.wadeable)
}
\arguments{
  \item{reachlen}{a vector of reachlengths for the sites}

  \item{numtran}{a vector with the number of transects
  sampled}

  \item{protocol}{a logical vector; \code{TRUE} if the site
  was wadeable}
}
\description{
  \code{calculateLWDSiteLength} is used to calculate the
  length of the sampled area so that counts/length and
  counts/area can be calculated.
}

