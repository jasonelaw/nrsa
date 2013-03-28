\name{calculateLWDSiteArea}
\alias{calculateLWDSiteArea}
\title{Calculate length of area sampled for LWD}
\usage{
  calculateLWDSiteArea(lwdlength, xbkf_w, is.wadeable)
}
\arguments{
  \item{lwdlength}{a vector of site lengths as returned by
  calculateLWDSiteLength}

  \item{xbkf_w}{a vector of average bankfull width}

  \item{protocol}{a logical vector; \code{TRUE} if the site
  was wadeable}
}
\description{
  \code{calculateLWDSiteLength} is used to calculate the
  area of the sampled lwd plots so that counts/area can be
  calculated.
}

