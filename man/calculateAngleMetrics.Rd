\name{calculateAngleMetrics}
\alias{calculateAngleMetrics}
\alias{calculateSlopeMetrics}
\title{Calculate slope and bearing metrics}
\usage{
  calculateAngleMetrics(uid, azimuth, distance)

  calculateSlopeMetrics()
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{transect}{a vector of transect identifiers}

  \item{azimuth}{a vector of bearings; more properly
  azimuths}

  \item{distance}{a vector of distances}
}
\description{
  Functions for calculating slope and bearing metrics from
  NRSA protocol channel geometry data.  The metrics
  calculated are: xbearing, sinu, xslope, vslope, nslope.
}
\details{
  The general strategy for calculationg slope and bearing
  metrics using these functions is: \enumerate{ \item
  Calculate wadeable transect spacing for wadeable as:
  \code{increment * nWadeableStationsPerTransect} \item
  Calculate wadeable distance for each slope/bearing as
  \code{transp * proportion} \item Calculate boatable
  transect spacing as \code{sum(distance)} and proportion
  as \code{distance / sum(distance)} \item Calculate grade
  (as %) from boatable elevation change measurements in
  centimeters, if applicable.  \item Calculate angle
  metrics using \code{calculateAngleMetrics} \item
  Calculate slope metrics using
  \code{calculateSlopeMetrics} } The overall working of
  \code{calculateAngleMetrics} differs somewhat than the
  EPA code.  EPA calculated total distances East and North
  at each transect, then summed to get the total distance
  East and North for the site.
  \code{calculateAngleMetrics} treats the channel geometry
  as a single line string and calculates the length of the
  line string and the distance from the first to last
  point.  It calculates xbearing as the azimuth between the
  first and last points.
}
\examples{
d <- data.frame(uid = c(1,1), azimuth = c(180,90), distance = c(1,1))
 calculateAngleMetrics(d$uid, d$azimuth, d$distance)
 d <- data.frame(uid = rep(1,6), transect = rep(1:2,each = 3),
                 slope = rep(c(10, 20, 30), 2), proportion = rep(1,6) / 3)
 with(d, calculateSlopeMetrics(uid, transect, slope, proportion))
}

