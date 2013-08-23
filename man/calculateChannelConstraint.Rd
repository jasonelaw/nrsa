\name{calculateChannelConstraint}
\alias{calculateChannelConstraint}
\title{Calculate the channel constraint metrics}
\usage{
  calculateChannelConstraint(uid, constraint)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{constraint}{a vector of channel constraint codes:
  'B', 'C', 'N', 'U'.}
}
\value{
  a 'metric' data.frame (i.e., one with uid, metric, result
  fields)
}
\description{
  Calculate the channel constaint metrics, pctch_b,
  pctch_c, pctch_n, and pctch_u.  These are the proprtion
  of the reach in each contraint category. For boatable
  sites only.
}
\examples{
uid <- rep(1:10, each = 11)
constraint <- sample(c('B', 'C', 'N', 'U'), 10*11, replace = T)
calculateChannelConstraint(uid, constraint)
}

