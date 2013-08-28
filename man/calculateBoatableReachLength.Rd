\name{calculateBoatableReachLength}
\alias{calculateBoatableReachLength}
\title{Calculates the reach length for a boatable site}
\usage{
  calculateBoatableReachLength(uid, actransp)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{actransp}{the actual transect spacing}
}
\value{
  a 'metric' data.frame
}
\description{
  Calculates the reach length for a boatable site based on
  the actual transect spacing recorded on the transect
  form.  There should be one input value for each transect
  at the site.
}

