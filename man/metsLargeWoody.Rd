\name{metsLargeWoody}
\alias{metsLargeWoody}
\alias{metsLgWoody.1}
\alias{metsInvasiveSpecies.1}
\alias{metsHumanInfluence}
\alias{metsHumanInfluence.1}
\alias{metsGeneral}
\alias{metsGeneral.1}
\alias{metsGeneral.cleanup}
\alias{metsFishCover}
\alias{metsFishCover.1}
\alias{metsChannelHabitat}
\alias{metsChannelHabitat.1}
\alias{metsChannelHabitat.cleanup}
\alias{metsChannelChar}
\alias{metsChannelChar.1}
\alias{metsChannelChar.cleanup}
\alias{metsCanopyDensiometer}
\alias{metsCanopyDensiometer.1}
\alias{metsCanopyDensiometer.cleanup}
\alias{metsBedStability}
\alias{metsBedStability.1}
\alias{metsBankMorphology}
\alias{metsBankMorphology.1}
\alias{metsSubstrateEmbed}
\alias{metsSubstrateEmbed.1}
\alias{metsSubstrateEmbed.cleanup}
\alias{metsSubstrateCharacterization}
\alias{metsSubstrateCharacterization.1}
\alias{metsSlopeBearing}
\alias{metsSlopeBearing.makeProtocols}
\alias{metsSlopeBearing.1}
\alias{metsRiparianVegetation}
\alias{metsRiparianVegetation.1}
\alias{metsResidualPools}
\alias{metsResidualPools.dataOrganization}
\alias{metsResidualPools.poolCharacteristics}
\alias{metsResidualPools.dimensions}
\alias{metsResidualPools.siteSummaries}
\alias{metsLittoralDepth}
\alias{metsLittoralDepth.1}
\alias{metsLittoralDepth.cleanup}
\alias{metsLegacyTree}
\alias{metsLegacyTree.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate NRSA prtocol metrics.
}
\description{
Calculate NRSA prtocol metrics.
}
\usage{
metsLargeWoody()
metsLgWoody.1(df1, df2, df3, protocols)
metsInvasiveSpecies()
metsInvasiveSpecies.1(df)
metsHumanInfluence()
metsHumanInfluence.1(df)
metsGeneral()
metsGeneral.1(indat)
metsGeneral.cleanup(indb)
metsFishCover()
metsFishCover.1(fishcover, protocols)
metsChannelHabitat()
metsChannelHabitat.1(indat)
metsChannelHabitat.cleanup(indb)
metsChannelChar()
metsChannelChar.1(indat, chanCon)
metsChannelChar.cleanup(indb)
metsCanopyDensiometer()
metsCanopyDensiometer.1(indat)
metsCanopyDensiometer.cleanup(indb)
metsBedStability()
metsBedStability.1(mets, protocols)
metsBankMorphology()
metsBankMorphology.1(df1, protocols)
metsSubstrateEmbed()
metsSubstrateEmbed.1(indat)
metsSubstrateEmbed.cleanup(indb)
metsSubstrateCharacterization()
metsSubstrateCharacterization.1(df1, df2, df3)
metsSlopeBearing()
metsSlopeBearing.1(thal, chanGeom, protocols)
metsSlopeBearing.makeProtocols()
metsRiparianVegetation()
metsRiparianVegetation.1(visrip)
metsResidualPools()
metsResidualPools.dataOrganization(thal, actransp, slopes)
metsResidualPools.1(thal, actransp, slopes, protocols)
metsResidualPools.siteSummaries(poolInfo, stationInfo)
metsResidualPools.poolCharacteristics(poolDims)
metsResidualPools.dimensions(thalSeries, thalProtocol, minSampPct = 85, oldeMethods = FALSE)
metsLittoralDepth()
metsLittoralDepth.1(indat)
metsLegacyTree()
metsLegacyTree.1(df1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{
%%     ~~Describe \code{df1} here~~
}
  \item{df2}{
%%     ~~Describe \code{df2} here~~
}
  \item{df3}{
%%     ~~Describe \code{df3} here~~
}
  \item{protocols}{
%%     ~~Describe \code{protocols} here~~
}
 \item{thal}{
%%     ~~Describe \code{thal} here~~
}
  \item{actransp}{
%%     ~~Describe \code{actransp} here~~
}
  \item{slopes}{
%%     ~~Describe \code{slopes} here~~
}
  \item{protocols}{
%%     ~~Describe \code{protocols} here~~
}
  \item{poolInfo}{
%%     ~~Describe \code{poolInfo} here~~
}
  \item{stationInfo}{
%%     ~~Describe \code{stationInfo} here~~
}
  \item{poolDims}{
%%     ~~Describe \code{poolDims} here~~
}
  \item{thalSeries}{
%%     ~~Describe \code{thalSeries} here~~
}
  \item{thalProtocol}{
%%     ~~Describe \code{thalProtocol} here~~
}
  \item{minSampPct}{
%%     ~~Describe \code{minSampPct} here~~
}
  \item{oldeMethods}{
%%     ~~Describe \code{oldeMethods} here~~
}
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    intermediateMessage("Large Woody debris calculations", loc = "start")
    chan <- odbcConnect("NRSA2")
    df1 <- fetchNRSATable(chan, "tblWOOD2")
    intermediateMessage("fetch_data.1", loc = "start")
    df2 <- fetchNRSATable(chan, "tblTHALWEG2")
    df3 <- fetchNRSATable(chan, "tblBANKGEOMETRY2")
    intermediateMessage("fetch_data.2", loc = "start")
    protocols <- siteProtocol(unique(df1$UID))
    intermediateMessage("set_protocols.3", loc = "start")
    mets <- metsLgWoody.1(df1, df2, df3, protocols)
    if (is.character(mets)) 
        return(mets)
    rc <- writeNRSACalcResults(mets, "metsLargeWoody.csv")
    intermediateMessage(" Done.", loc = "end")
    return(rc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
