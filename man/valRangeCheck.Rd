\name{valRangeCheck}
\alias{valRangeCheck}
\alias{valRangeSlope}
\alias{valRangeThalwegSupplemental.incremntCheck}
\alias{valRangeThalwegSupplemental.neighborBasedChecks}
\alias{valRangeThalwegSupplemental}
\alias{valRangeCheck.1}
\alias{valRangeCheck.cleanup}
\alias{valRangeSlope.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs range checks of data.
}
\description{
Performs range checks of data.
}
\usage{
valRangeCheck(tableName, supplementalTest = NULL, since = NULL)
valRangeSlope(since = NULL)
valRangeThalwegSupplemental.incremntCheck(df)
valRangeThalwegSupplemental.neighborBasedChecks(df)
valRangeThalwegSupplemental(df)
valRangeSlope.1(cg, thal, meta.df, siteInfo)
valRangeCheck.1(df, meta.df, siteInfo, supplementalTest)
valRangeCheck.cleanup(chan)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
}
  \item{supplementalTest}{
%%     ~~Describe \code{supplementalTest} here~~
}
  \item{since}{
%%     ~~Describe \code{since} here~~
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
function (tableName, supplementalTest = NULL, since = NULL) 
{
    chan <- odbcConnect("NRSA2")
    on.exit(valRangeCheck.cleanup(chan))
    if (!is.null(since)) {
        since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", 
            sep = "")
    }
    df <- fetchNRSATable(chan, tableName, where = since)
    if (!is.data.frame(df)) 
        return(df)
    meta.df <- fetchNRSATable(chan, "tblPARAMETERDESCRIPTIONS2", 
        filterSideChannels = FALSE)
    if (!is.data.frame(meta.df)) 
        return(meta.df)
    siteInfo <- fetchNRSATable(chan, "tblVISITS2", filterSideChannels = FALSE)
    if (!is.data.frame(siteInfo)) 
        return(siteInfo)
    validationResults <- valRangeCheck.1(df, meta.df, siteInfo, 
        supplementalTest)
    if (!is.data.frame(validationResults)) 
        return(validationResults)
    print(sprintf("Detected \%d out-of-range values", nrow(validationResults)))
    rc <- writeNRSAValidationResults(validationResults, paste(NRSAvalidationLocation, 
        "valRangeSupplemental", tableName, ".csv", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
