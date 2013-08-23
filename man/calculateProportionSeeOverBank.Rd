\name{calculateProportionSeeOverBank}
\alias{calculateProportionSeeOverBank}
\title{Calculates the proportion of the reach over which you can see over the bank}
\usage{
  calculateProportionSeeOverBank(uid, see.over.bank)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{see.over.bank}{a logical vector or vector
  containing the character string 'YES'.}
}
\value{
  a 'metric' data.frame (i.e., one with uid, metric, result
  fields)
}
\description{
  Calculates the metric pct_ovrb; the proportion of the
  reach over which an observer can see over the bank.  For
  boatable sites only.
}
\examples{
uid <- rep(1:10, each = 11)
}

