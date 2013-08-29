\name{calculateChannelMetrics}
\alias{calculateChannelMetrics}
\title{Calculate channel metrics}
\usage{
  calculateChannelMetrics(uid, bankwid, incishgt, bankhgt)
}
\arguments{
  \item{uid}{}

  \item{wetwid}{}

  \item{bankwid}{}

  \item{incishgt}{}

  \item{bankhgt}{}
}
\description{
  Calculate channel metrics
}
\examples{
d <- data.frame(uid = rep(1:10, 11), wetwid = runif(110), bankwid = runif(110), incishgt = runif(110), bankhgt = runif(110))
calculateChannelMetrics(d$uid, d$wetwid, d$bankwid, d$incishgt, d$bankhgt)
}

