\name{observedLWDTransects}
\alias{observedLWDTransects}
\title{Calculate number of LWD observed transects}
\usage{
  observedLWDTransects(uid, transect, count, in.bankfull)
}
\arguments{
  \item{count}{vector of counts of LWD pieces}

  \item{in.bankfull}{logical vector; true if the lwd was
  within bankfull}

  \item{uid}{vector of site-visit indicators}

  \item{transect}{vector of transect names}
}
\description{
  \code{observedLWDTransects} is used to calculate the
  number of transects that were observed for the LWD data.
}

