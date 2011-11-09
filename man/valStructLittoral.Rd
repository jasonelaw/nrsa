\name{valStructLittoral}
\alias{valStructLittoral}
\alias{valStructInvasiveLegacy}
\alias{valStructFishPhoto}
\alias{valStructFishCollection}
\alias{valStructFieldMeasure}
\alias{valStructFieldCalibration}
\alias{valStructDischarge}
\alias{valStructChannelGeometry}
\alias{valStructChannelCrossSection}
\alias{valStructChanDepth}
\alias{valStructChanCov}
\alias{valStructChanChar}
\alias{valStructBenthic}
\alias{valStructBankGeometry}
\alias{valStructWood}
\alias{valStructVisits}
\alias{valStructVisAssess}
\alias{valStructTorrent}
\alias{valStructThalweg}
\alias{valStructSamples}
\alias{valStructParameterDescriptions}
\alias{valStructOtherInvasives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check data structure of NRSA protocol data.
}
\description{
Performs integrity checks of NRSA protocol data.  Checks for problems that
might cause the metric calculation to fail or to violate assumptsions
of the metric calculation functions.
}
\usage{
valStructLittoral(df, test = "all")
valStructInvasiveLegacy(df, test = "all")
valStructFishPhoto(df, test = "all")
valStructFishCollection(df, test = "all")
valStructFieldMeasure(df, test = "all")
valStructFieldCalibration(df, test = "all")
valStructDischarge(df, test = "all")
valStructChannelGeometry(df, test = "all")
valStructChannelCrossSection(df, test = "all")
valStructChanDepth(df, test = "all")
valStructChanCov(df, test = "all")
valStructChanChar(df, test = "all")
valStructBenthic(df, test = "all")
valStructBankGeometry(df, test = "all")
valStructWood(df, test = "all")
valStructVisits(df, test = "all")
valStructVisAssess(df, test = "all")
valStructTorrent(df, test = "all")
valStructThalweg(df, test = "all")
valStructSamples(df, test = "all")
valStructParameterDescriptions(df, test = "all")
valStructOtherInvasives(df, test = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
The data frame to check
}
  \item{test}{
'all'       default, performs all tests
'vital'     performs only vital tests
'nonvital'  performs only nonvital tests
'synopsis'  performs all tests, returning counts of detected errors for all tests
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
TRUE or a character string.
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
function (df, test = "all") 
{
    intermediateMessage("Structure validation of littoral data ", 
        loc = "start")
    probs <- NULL
    if (!(test \%in\% c("all", "vital", "nonvital", "synopsis"))) {
        probs <- "Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
        return(as.matrix(probs))
    }
    intermediateMessage(".1")
    probs <- stValColumnPresence(df, c("UID", "TRANSECT", "PARAMETER", 
        "RESULT", "FLAG", "SAMPLE_TYPE"))
    if (!is.null(probs)) {
        colnames(probs)[1] <- "User.MUST.fix.these.errors.before.proceding"
        intermediateMessage(".  Done (terminated early).", loc = "end")
        return(probs)
    }
    if (test \%in\% c("all", "vital", "synopsis")) {
        intermediateMessage(".2")
        pp <- stValMissingValues(df, "UID")
        if (test == "synopsis") {
            probs <- rbind(probs, ifelse(is.null(pp), "No Missing UID values", 
                "Missing UID values exist (vital)"))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".3")
        pp <- stValMissingValues(df, "TRANSECT")
        if (test == "synopsis") {
            probs <- rbind(probs, ifelse(is.null(pp), "No Missing TRANSECT values", 
                "Missing TRANSECT values exist (vital)"))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".4")
        pp <- stValMissingValues(df, "PARAMETER")
        if (test == "synopsis") {
            probs <- rbind(probs, ifelse(is.null(pp), "No Missing PARAMETER values", 
                "Missing PARAMETER values exist (vital)"))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".5")
        pp <- stValMissingValues(df, "SAMPLE_TYPE")
        if (test == "synopsis") {
            probs <- rbind(probs, ifelse(is.null(pp), "No Missing SAMPLE_TYPE values", 
                "Missing SAMPLE_TYPE values exist (vital)"))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".6")
        pp <- stValUnexpectedValues(df, "TRANSECT", c("A", "B", 
            "C", "D", "E", "F", "G", "H", "I", "J", "K"), "UID")
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Unexpected TRANSECT values: \%d (vital)", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".7")
        pp <- stValCountRows(df, c("UID", "TRANSECT", "PARAMETER"), 
            1)
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Duplicate UID-TRANSECT values: \%d (vital)", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
    }
    if (test \%in\% c("all", "nonvital", "synopsis")) {
        intermediateMessage(".8")
        pp <- stValAbsentValues(df, "TRANSECT", c("A", "B", "C", 
            "D", "E", "F", "G", "H", "I", "J", "K"), "UID")
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Absent TRANSECT values: \%d", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".9")
        pp <- stValUnexpectedValues(df, "PARAMETER", c("BOTTOMDOM", 
            "BOTTOMSEC", "SHOREDOM", "SHORESEC", "SUBOBS"), c("UID", 
            "TRANSECT"))
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Unexpected PARAMETER values: \%d", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".10")
        pp <- stValAbsentValues(df, "PARAMETER", c("BOTTOMDOM", 
            "BOTTOMSEC", "SHOREDOM", "SHORESEC", "SUBOBS"), c("UID", 
            "TRANSECT"))
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Absent PARAMETER values: \%d", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
        intermediateMessage(".11")
        pp <- stValUnexpectedValues(df, "SAMPLE_TYPE", c("PHAB_CHANBFRONT"), 
            c("UID", "TRANSECT", "PARAMETER"))
        if (test == "synopsis") {
            probs <- rbind(probs, sprintf("Unexpected SAMPLE_TYPE values: \%d", 
                ifelse(is.null(pp), 0, nrow(pp))))
        }
        else {
            probs <- rbind(probs, pp)
        }
    }
    intermediateMessage(".  Done", loc = "end")
    return(probs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
