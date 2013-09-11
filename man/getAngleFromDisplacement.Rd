\name{getAngleFromDisplacement}
\alias{degToRad}
\alias{getAngleFromDisplacement}
\alias{getDisplacementFromDistAzimuth}
\alias{radToDeg}
\title{Returns the angle from the y axis for a vector}
\usage{
  getAngleFromDisplacement(x, y)

  degToRad(x)

  radToDeg(x)

  getDisplacementFromDistAzimuth(azimuth, distance,
    units = c("radian", "degrees"))
}
\arguments{
  \item{x}{the x coordinate}

  \item{y}{the y coordinate}

  \item{azimuth}{a vector of azimuths}

  \item{distance}{a vector of distances}

  \item{units}{the angle units of azimuths}
}
\value{
  the angle in radians for \code{getAngleFromDisplacement};
  angles in the appropriate units for \code{degToRad} and
  \code{radToDeg}; Displacement distances for each azimuth
  distance pair for \code{getDisplacementFromDistAzimuth}.
}
\description{
  Returns the angle from the y axis for a two dimenisonal
  vector (x,y).
}

