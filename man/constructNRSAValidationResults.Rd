\name{constructNRSAValidationResults}
\alias{constructNRSAValidationResults}
\alias{constructNRSAValidationUpdate}
\alias{readNRSAValidationResults}
\alias{writeNRSAValidationResults}
\alias{combineValidationResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct validation results
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
constructNRSAValidationResults(df, formMetadata, siteInfo, ssFmt = "EXCEL")
constructNRSAValidationUpdate(valResults, siteInfo, currentData)
readNRSAValidationResults(fileName)
writeNRSAValidationResults(df, outLoc)
combineValidationResults(r1, r2, keys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{formMetadata}{
%%     ~~Describe \code{formMetadata} here~~
}
  \item{siteInfo}{
%%     ~~Describe \code{siteInfo} here~~
}
  \item{ssFmt}{
%%     ~~Describe \code{ssFmt} here~~
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
function (df, formMetadata, siteInfo, ssFmt = "EXCEL") 
{
    intermediateMessage("Construction of validation results", 
        loc = "start")
    byVars <- c("SAMPLE_TYPE", "PARAMETER")
    metadataCols <- c("PARAMETER", "FORMABBR", "SAMPLE_TYPE")
    formMetadata <- formMetadata[!duplicated(formMetadata[c("PARAMETER", 
        "SAMPLE_TYPE")]), ]
    df <- merge(df, formMetadata[metadataCols], by = byVars, 
        all.x = TRUE, all.y = FALSE)
    intermediateMessage(".1")
    df <- merge(df, siteInfo[c("UID", "SITE_ID", "VISIT_NO", 
        "DATE_COL")], by = "UID", all.x = TRUE, all.y = FALSE)
    intermediateMessage(".2")
    parametersChRip_p2 <- c("CANBTRE", "CANSTRE", "CANVEG", "UNDERVEG", 
        "UNDNWDY", "UNDWDY", "GCNWDY", "GCWDY", "BARE", "WALL", 
        "BUILD", "PAVE", "ROAD", "PIPES", "LANDFL", "PARK", "ROW", 
        "PAST", "LOG", "MINE", "ALGAE", "MACPHY", "WOODY", "BRISH", 
        "LVTREE", "OVRHNG", "UNDCUT", "BOULDR", "STRUCT", "SHOR2RIP", 
        "CONSTRT", "SEEOVRBK", "DENSIOM")
    df$ffName <- ifelse(df$FORMABBR \%in\% c("ChRip", "Thal"), 
        ifelse(df$FORMABBR == "ChRip" & format(as.POSIXct(df$DATE_COL), 
            "\%Y") > 2008 & df$SAMPLE_TYPE \%in\% c("PHAB_CHANB", 
            "PHAB_CHANBFRONT"), paste("NRSA", df$FORMABBR, df$SITE_ID, 
            paste("V", df$VISIT_NO, sep = ""), df$TRANSECT, ifelse(df$PARAMETER \%in\% 
                parametersChRip_p2, "P2", "P1"), sep = "_"), 
            paste("NRSA", df$FORMABBR, df$SITE_ID, paste("V", 
                df$VISIT_NO, sep = ""), df$TRANSECT, sep = "_")), 
        paste("NRSA", df$FORMABBR, df$SITE_ID, paste("V", df$VISIT_NO, 
            sep = ""), sep = "_"))
    df$ffName <- paste("L:/Apps/Scantron/Images/", format(as.POSIXct(df$DATE_COL), 
        "\%Y"), "/Flowing Waters/", df$ffName, ".tif", sep = "")
    if (ssFmt == "OO") {
        df$FORMIMAGE <- paste("=HYPERLINK(\"file:///", df$ffName, 
            "\" ; \"", df$FORMABBR, "\")", sep = "")
    }
    else if (ssFmt == "EXCEL") {
        df$FORMIMAGE <- paste("=HYPERLINK(\"file:///", df$ffName, 
            "\" , \"", df$FORMABBR, "\")", sep = "")
    }
    intermediateMessage(".3")
    tableKeys <- names(df)[names(df) \%in\% NRSAKeyColumns & names(df) != 
        "UID"]
    ordering <- eval(parse(text = sprintf("order(\%s)", paste("df$", 
        c("SITE_ID", "VISIT_NO", "DATE_COL", tableKeys, "TESTDESCRIPTION", 
            "FORMABBR", "PARAMETER"), sep = "", collapse = ", "))))
    df <- df[ordering, ]
    intermediateMessage(".4")
    unitsIfPresent <- names(df)[names(df) \%in\% "UNITS"]
    flagIfPresent <- names(df)[names(df) \%in\% "FLAG"]
    exportColumns <- c("UID", "SITE_ID", "VISIT_NO", "DATE_COL", 
        tableKeys, "PARAMETER", "TESTDESCRIPTION", "RESULT", 
        unitsIfPresent, flagIfPresent, "FORMIMAGE", "COMMENTS")
    df$COMMENTS <- "                                              "
    df <- df[exportColumns]
    rownames(df) <- NULL
    intermediateMessage(".Done", loc = "end")
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
