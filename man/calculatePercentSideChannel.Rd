% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateGeneral.r
\name{calculatePercentSideChannel}
\alias{calculatePercentSideChannel}
\title{Proportion of side of reach with side channels.}
\usage{
calculatePercentSideChannel(uid, transect, is.sidechannel)
}
\arguments{
\item{uid}{a vector of site identifiers}

\item{transect}{a vector of transect identifiers}

\item{is.sidechannel}{a vector containing the 'SIDCHN' and 'OFF_CHAN' data, 
either \code{'Y'} or \code{'N'} or \code{NA}.}
}
\value{
a 'metric' data.frame
}
\description{
Calculates the proportion of the reach that has side channels.  The input data
is 'SIDCHN' for wadeable and OFF_CHAN for boatable sites.
}
