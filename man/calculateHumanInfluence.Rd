\name{calculateHumanInfluence}
\alias{calculateHumanInfluence}
\title{Calculate human influence metrics}
\usage{
  calculateHumanInfluence(uid, parameter, result)
}
\arguments{
  \item{uid}{a vector that differentiates among sites or
  site visits.}

  \item{parameter}{a vector of parameter names for the 11
  human influence parameters}

  \item{result}{a vector of results 0 (not present), P
  (present > 10 m), C (present < 10 m), B (present on
  bank)}
}
\value{
  returns a dataframe with three columns: uid, metric,
  result
}
\description{
  This function calculates the human influence metrics.
  The parameter names are assumed to be in lower case
}

