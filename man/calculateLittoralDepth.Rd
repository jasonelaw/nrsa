\name{calculateLittoralDepth}
\alias{calculateLittoralDepth}
\alias{calculateLittoralDepth2}
\title{Calculate non-wadeable littoral depth metrics}
\usage{
  calculateLittoralDepth(uid, depth)

  calculateLittoralDepth2(uid, depth)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{depth}{a vector of depth measurements (with the
  same units)}
}
\description{
  \code{calculateLittoralDepth} calculates the non-wadeable
  littoral depth metrics.  There are two different
  implementations that vary primarily in speed.
  \code{calculateLittoralDepth} is faster although the
  implementation is somewhat more obtuse.
  \code{calculateLittoralDepth2} uses \link{ddply} and is
  fairly simple.
}
\examples{
calculateLittoralDepth(uid = rep(1:10, each = 10), depth = rnorm(100))
calculateLittoralDepth(uid = rep(1, 5), depth = rep(NA, 5))
}

