#' Calculates summaries of shore to vegetation distance
#'
#'Calculates the metrics xshor2vg, mxshor, mnshor which summarize the shore to
#'riparian vegetation distance over the reach.  For boatable sites only.
#'@param uid a vector of site identifiers
#'@param shor2rip a vector of distances from the shore to riparian vegeatation
#'@return a 'metric' data.frame (i.e., one with uid, metric, result fields)
#'@export
calculateShoreToVegDistance <- function(uid, shor2rip){
  uid <- as.factor(uid)
  if(is.factor(shor2rip)){
    shor2rip <- convertFactorToNumeric(shor2rip)
  }
  f <- function(x){
    c(xshor2vg = mean(x$shor2rip, na.rm = T),
      mxshor   = max(x$shor2rip, na.rm = T),
      mnshor   = min(x$shor2rip, na.rm = T))
  }
  mets <- plyr::ddply(data.frame(uid, shor2rip), c('uid'), f)
  mets <- meltMetrics(mets)
  progressReport("Finished with shore to vegetation metrics.")
  return(mets)
}

#' Calculates the proportion of the reach over which you can see over the bank
#' 
#' Calculates the metric pct_ovrb; the proportion of the reach over which an
#' observer can see over the bank.  For boatable sites only.
#' @param uid a vector of site identifiers
#' @param see.over.bank a logical vector or vector containing the character string
#' 'YES'.
#' @return a 'metric' data.frame (i.e., one with uid, metric, result fields)
#' @export
#' @examples
#' uid <- rep(1:10, each = 11)
#' see.over.bank <- sample(c('YES', 'NO'), n = 10 * 11, replace = T)
#' calculateProportionSeeOverBank(uid, see.over.bank)
calculateProportionSeeOverBank <- function(uid, see.over.bank){
  uid <- as.factor(uid)
  if (!is.logical(see.over.bank)){
    see.over.bank <- see.over.bank == 'YES'
  }
  mets <- plyr::ddply(data.frame(uid, see.over.bank), c('uid'), 
                function(x) c(pct_ovrb = mean(x$see.over.bank, na.rm = T) * 100))
  mets <- meltMetrics(mets)
  progressReport("Finished with pct_ovrb.")
  return(mets)
}

#' Calculate the channel constraint metrics
#' 
#' Calculate the channel constaint metrics, pctch_b, pctch_c, pctch_n, and
#' pctch_u.  These are the proprtion of the reach in each contraint category.
#' For boatable sites only.
#' 
#' @param uid a vector of site identifiers
#' @param constraint a vector of channel constraint codes: 'B', 'C', 'N', 'U'.
#' @return a 'metric' data.frame (i.e., one with uid, metric, result fields)
#' @export
#' @examples
#' uid <- rep(1:10, each = 11)
#' constraint <- sample(c('B', 'C', 'N', 'U'), 10*11, replace = T)
#' calculateChannelConstraint(uid, constraint)
calculateChannelConstraint <- function(uid, constraint){
  constraint <- as.factor(constraint)
  levels(constraint) <- list('pctch_b' = 'B', 'pctch_c' = 'C', 
                             'pctch_n' = 'N', 'pctch_u' = 'U')
  tbl <- prop.table(table(uid = uid, metric = constraint), margin = 1) * 100
  mets <- allFacToChar(as.data.frame.table(tbl, responseName = 'result'))
  progressReport('Finished with channel constraint metrics.')
  return(mets)
}

#' Formats the channel constraint form data as metrics
#'
#' Formats the channel constraint form data as metrics.  There is only one form
#' per site so no calculations are required; just some formatting.  
#' For boatable sites only.
#' @param uid a vector of site identifiers
#' @param parameter a vector of parameters including: BANKFULL, CONSTRNT, FEATURES,
#' PATTERN, PERCENT, VALLEY, VALLYBOX
#' @param result a vector of results for each parameter
#' @return a 'metric' data.frame
getChannelConstraint <- function(uid, bankfull, constraint, features, pattern, percent, valley, valleybox){
  x <- data.frame(uid, bankfull, constraint, features, pattern, percent, valley, valleybox)
  x <- reshape2::melt(x, id.vars = 'uid', variable.name = 'metric', value.name = 'result')
  x$metric <- 
    factor(x      = as.character(x$metric), 
           levels = c("bankfull", "constraint", "features", "pattern", 
                      "percent", "valley", "valleybox"),
           labels = c("conbankfull", "constraint", "confeatures", "conpattern",
                      "conpercent", "convalley", "convalleybox"))
if(any(is.na(x$metric))){
  stop('There are unknown parameter values in getChannelConstraint')
}
  return(allFacToChar(x))
}
