\name{calculateLWDCounts}
\alias{calculateLWDCounts}
\title{Calculate LWD counts within each size class}
\usage{
  calculateLWDCounts(uid, in.bankfull, length, diameter,
    count)
}
\arguments{
  \item{uid}{vector of site-visit indicators}

  \item{in.bankfull}{logical vector; true if the lwd was
  within bankfull}

  \item{length}{the length category of the lwd piece}

  \item{diameter}{the diamter cateogry of the lwd piece}

  \item{count}{vector of counts of LWD pieces}
}
\description{
  This function calculates LWD counts within each site,
  size class, and whether it is within the bankfull area
}

