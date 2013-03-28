\name{valLegalCheck}
\alias{valLegalCheck}
\alias{valLegalUnitsCheck.1}
\alias{valLegalUnitsCheck.cleanup}
\alias{valLegalUnitsCheck}
\alias{valLegalCheck.1}
\alias{valLegalCheck.cleanup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check for legal values.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
valLegalCheck(tableName, since = NULL)
valLegalUnitsCheck.1(df, meta.df, siteInfo)
valLegalUnitsCheck.cleanup(chan)
valLegalUnitsCheck(tableName, since = NULL)
valLegalCheck.1(df, meta.df, siteInfo)
valLegalCheck.cleanup(chan)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
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
function (tableName, since = NULL) 
{
    chan <- odbcConnect("NRSA2")
    on.exit(valLegalCheck.cleanup(chan))
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
    validationResults <- valLegalCheck.1(df, meta.df, siteInfo)
    if (!is.data.frame(validationResults)) 
        return(validationResults)
    print(sprintf("Detected \%d illegal values", nrow(validationResults)))
    rc <- writeNRSAValidationResults(validationResults, paste(NRSAvalidationLocation, 
        "valLegal", tableName, ".csv", sep = ""))
    return(NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
