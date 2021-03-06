% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateGeneral.r
\name{isSiteSampled}
\alias{isSiteSampled}
\title{Determines whether a site was sampled from x site validation data}
\usage{
isSiteSampled(uid, valxsite)
}
\arguments{
\item{uid}{a vector of site identifiers}

\item{valxsite}{a vector of sampling methods.  The following ones indicate the
site was sampled: BOATABLE, PARBYBOAT, ALTERED, INTWADE, PARBYWADE, WADEABLE}
}
\value{
a 'metric' data.frame
}
\description{
Function returns the 'sampled' metric based on the data from the stream
verification form (field VALXSITE from the VERIFICATION table).
}
