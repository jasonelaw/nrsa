\name{calculateThalwegRatios}
\alias{calculateThalwegRatios}
\title{Calculate thalweg width depth ratios}
\usage{
  calculateThalwegRatios(uid, wetwid, depth)
}
\arguments{
  \item{uid}{a vector site identifiers}

  \item{wetwid}{}

  \item{depth}{}
}
\value{
  a 'metric' data.frame
}
\description{
  Calculate metrics dealing with the product and ratio of
  the channel width and depth: xwxd, sdwxd, n_wd, xwd_rat,
  sdwd_rat, and n_wdr.
}
\details{
  These are station level data (i.e., from the thalweg
  form) and so do not need any special extra transect
  treatment.
}

