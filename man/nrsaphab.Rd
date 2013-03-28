\name{nrsaphab}
\alias{nrsaphab}
\alias{nrsaphab.resetFlag}
\alias{nrsaphab.unpackFlag}
\alias{nrsaphab.allFlags}
\alias{nrsaphab.assemble}
\alias{nrsaphab.packFlags}
\alias{nrsaphab.dbUpdate}
\alias{nrsaphab.calculate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility functions to run calculation functions for all NRSA protocol metrics.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nrsaphab(bankMorphology = TRUE, bedStability = TRUE, canopyDensiometer = TRUE, channelCharacters = TRUE, channelHabitat = TRUE, channelMorphology = TRUE, fishCover = TRUE, general = TRUE, humanInfluence = TRUE, invasiveSpecies = TRUE, largeWoody = TRUE, legacyTree = TRUE, littoralDepth = TRUE, residualPools = TRUE, riparianVegetation = TRUE, slopeBearing = TRUE, substrateCharacterization = TRUE, substrateEmbeddedness = TRUE)
nrsaphab.calculate(flags)
nrsaphab.resetFlag(flags, flag)
nrsaphab.unpackFlag(flags, flag)
nrsaphab.allFlags()
nrsaphab.assemble(flags)
nrsaphab.packFlags(bankMorphology, bedStability, canopyDensiometer, channelCharacters, channelHabitat, channelMorphology, fishCover, general, humanInfluence, invasiveSpecies, largeWoody, legacyTree, littoralDepth, residualPools, riparianVegetation, slopeBearing, substrateCharacterization, substrateEmbeddedness)
nrsaphab.dbUpdate()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bankMorphology}{
%%     ~~Describe \code{bankMorphology} here~~
}
  \item{bedStability}{
%%     ~~Describe \code{bedStability} here~~
}
  \item{canopyDensiometer}{
%%     ~~Describe \code{canopyDensiometer} here~~
}
  \item{channelCharacters}{
%%     ~~Describe \code{channelCharacters} here~~
}
  \item{channelHabitat}{
%%     ~~Describe \code{channelHabitat} here~~
}
  \item{channelMorphology}{
%%     ~~Describe \code{channelMorphology} here~~
}
  \item{fishCover}{
%%     ~~Describe \code{fishCover} here~~
}
  \item{general}{
%%     ~~Describe \code{general} here~~
}
  \item{humanInfluence}{
%%     ~~Describe \code{humanInfluence} here~~
}
  \item{invasiveSpecies}{
%%     ~~Describe \code{invasiveSpecies} here~~
}
  \item{largeWoody}{
%%     ~~Describe \code{largeWoody} here~~
}
  \item{legacyTree}{
%%     ~~Describe \code{legacyTree} here~~
}
  \item{littoralDepth}{
%%     ~~Describe \code{littoralDepth} here~~
}
  \item{residualPools}{
%%     ~~Describe \code{residualPools} here~~
}
  \item{riparianVegetation}{
%%     ~~Describe \code{riparianVegetation} here~~
}
  \item{slopeBearing}{
%%     ~~Describe \code{slopeBearing} here~~
}
  \item{substrateCharacterization}{
%%     ~~Describe \code{substrateCharacterization} here~~
}
  \item{substrateEmbeddedness}{
%%     ~~Describe \code{substrateEmbeddedness} here~~
}
}
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
function (bankMorphology = TRUE, bedStability = TRUE, canopyDensiometer = TRUE, 
    channelCharacters = TRUE, channelHabitat = TRUE, channelMorphology = TRUE, 
    fishCover = TRUE, general = TRUE, humanInfluence = TRUE, 
    invasiveSpecies = TRUE, largeWoody = TRUE, legacyTree = TRUE, 
    littoralDepth = TRUE, residualPools = TRUE, riparianVegetation = TRUE, 
    slopeBearing = TRUE, substrateCharacterization = TRUE, substrateEmbeddedness = TRUE) 
{
    flags <- nrsaphab.packFlags(bankMorphology, bedStability, 
        canopyDensiometer, channelCharacters, channelHabitat, 
        channelMorphology, fishCover, general, humanInfluence, 
        invasiveSpecies, largeWoody, legacyTree, littoralDepth, 
        residualPools, riparianVegetation, slopeBearing, substrateCharacterization, 
        substrateEmbeddedness)
    flags2 <- nrsaphab.calculate(flags)
    nrsaphab.assemble(flags2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
