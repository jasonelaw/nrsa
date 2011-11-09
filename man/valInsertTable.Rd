\name{valInsertTable}
\alias{valInsertTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Not really sure
}
\description{
Not really sure what this does.
}
\usage{
valInsertTable(inLoc, tblName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inLoc}{
%%     ~~Describe \code{inLoc} here~~
}
  \item{tblName}{
%%     ~~Describe \code{tblName} here~~
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
function (inLoc, tblName) 
{
    valResults <- readNRSAValidationResults(inLoc)
    if (!is.data.frame(valResults)) {
        return(valResults)
    }
    valResults$UID <- NULL
    valResults$DATE_COL <- as.Date(valResults$DATE_COL, "\%m/\%d/\%Y")
    chan <- odbcConnect("NRSA2")
    on.exit(valUpdateTable.cleanup(chan))
    siteInfo <- fetchNRSATable(chan, "tblVISITS2")
    if (!is.data.frame(siteInfo)) {
        return(paste("Error: Could not retrieve tblVISITS2 ", 
            ". ", siteInfo))
    }
    currentData <- fetchNRSATable(chan, tblName)
    if (!is.data.frame(currentData)) {
        return(paste("Error: Could not retrieve current contents of ", 
            tblName, ". ", currentData))
    }
    updateData <- constructNRSAValidationUpdate(valResults, siteInfo, 
        currentData)
    if (!is.data.frame(updateData)) {
        return(paste("Error: Could not construct update with validation data for ", 
            tblName, ". ", updateData))
    }
    keysUsed <- names(updateData)[names(updateData) \%in\% c(NRSAKeyColumns, 
        "PARAMETER")]
    rc <- dbInsert(chan, tblName, updateData, keysUsed)
    return(rc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
