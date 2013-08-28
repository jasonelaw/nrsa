\name{calculateInvasiveScore}
\alias{calculateInvasiveScore}
\title{Calculate total invasive score}
\usage{
  calculateInvasiveScore(uid, metric, result, FUN = sum)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{result}{a vector of invasive metrics for each
  site.}
}
\value{
  a 'metric' data.frame
}
\description{
  Calculates the ip_score metric from the NRSA metrics.
  This is just the sum of the other metric scores for the
  site.  The metric is somewhat concerning if a survey
  doesn't use the stock EPA invasives list, because the
  metrics would be incomparable.  The mean might be a
  better metric, although not much better
}
\details{
  For the ip_score to be useful, each site must have the
  same number and identity of invasive species.  For this
  implementation, the metrics are subsetted so that only
  the species on the original EPA NRSA list (f_myrspi,
  f_hydver, f_eiccra, f_nympel, f_lytsal, f_arudon,
  f_butumb, f_tamspp, f_rosmul, f_eupesu) are included in
  the calculation.
}

