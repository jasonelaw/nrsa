\name{valMissingCheck}
\alias{valMissingCheck}
\alias{valMissingCheck.1}
\alias{valMissingCheck.cleanup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks for missing values.
}
\description{
Checks for missing values.
}
\usage{
valMissingCheck(tableName, since = NULL)
valMissingCheck.1(df, meta.df, siteInfo)
valMissingCheck.cleanup(chan)
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
    on.exit(valMissingCheck.cleanup(chan))
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
    missingResults <- valMissingCheck.1(df, meta.df, siteInfo)
    if (!is.data.frame(missingResults)) 
        return(missingResults)
    print(sprintf("Detected \%d rows with missing values", nrow(missingResults)))
    rc <- writeNRSAValidationResults(missingResults, paste(NRSAvalidationLocation, 
        "valMissing", tableName, ".csv", sep = ""))
    return(NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
