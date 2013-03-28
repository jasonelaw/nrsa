\name{internal}
\alias{interpolatePercentile}
\alias{fetchNRSATable}
\alias{assignTaxCat}
\alias{summaryby}
\alias{siteProtocol}
\alias{quickUnitsCheck}
\alias{nWadeableStationsPerTransect}
\alias{neightborBasedValidation}
\alias{NA_filler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Various internal utility functions.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interpolatePercentile(df, classVar, percentile, pctlVar, classBounds)
fetchNRSATable(chan, tblName, at=NULL, where=NULL, filterSideChannels=TRUE)
assignTaxCat(lt)
summaryby(xxx, yyy, zzz)
siteProtocol(sites)
siteProtocol.1(sites, siteInfo)
siteProtocol.cleanup(chan)
quickUnitsCheck(df, meta.df)
nWadeableStationsPerTransect(thal)
neightborBasedValidation(df, keys, name, value, parameter, tbf, min, max)
NA_filler(alldat, metdat, metlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{classVar}{
%%     ~~Describe \code{classVar} here~~
}
  \item{percentile}{
%%     ~~Describe \code{percentile} here~~
}
  \item{pctlVar}{
%%     ~~Describe \code{pctlVar} here~~
}
  \item{classBounds}{
%%     ~~Describe \code{classBounds} here~~
}
}
\details{
Not really to be used by others
}

\author{
EPA
}




