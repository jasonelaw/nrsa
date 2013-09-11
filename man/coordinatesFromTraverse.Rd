\name{coordinatesFromTraverse}
\alias{coordinatesFromTraverse}
\title{Computes coordinates for a traverse}
\usage{
  coordinatesFromTraverse(azimuth, distance,
    start = c(0, 0))
}
\arguments{
  \item{azimuth}{a vector of azimuth angles in degrees}

  \item{distance}{a vector of distances}

  \item{start}{the coordinates of the starting point for
  the traverse}
}
\value{
  the coordinates for the traverse
}
\description{
  Computes the coordinates for a two dimensional traverse
  with an arbitrary start point (assumed to be (0,0)).
}

