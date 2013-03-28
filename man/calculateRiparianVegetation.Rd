\name{calculateRiparianVegetation}
\alias{calculateRiparianVegetation}
\title{Calculates visible riparian vegetation metrics.}
\usage{
  calculateRiparianVegetation(uid, transect, transdir,
    parameter, result)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{transect}{a vector of transect indicators}

  \item{transdir}{a vector of transect direction indicators
  which identify the bank on which the visual riparian plot
  was located}

  \item{parameter}{a vector of parameter names (the left
  column on the form)}

  \item{result}{a vector of visual riparian estimates; 0-4
  for the cover estimates and (D, C, E, M, N) for the
  vegetation types.}
}
\value{
  a data frame of riparian vegetation metrics
}
\description{
  \code{calculateRiparianVegetation} is used to calculate
  riparian vegetation metrics from the visual riparian
  estimates section of the NRSA forms.
}

