#' Calculates summaries of shore to vegetation distance
#'
#'Calculates the metrics xshor2vg, mxshor, mnshor which summarize the shore to
#'riparian vegetation distance over the reach.  For boatable sites only.
#'@param uid a vector of site identifiers
#'@param shor2rip a vector of distances from the shore to
#'@importFrom Rigroup igroupMeans igroupMaxs igroupMins
#'@return a 'metric' data.frame (i.e., one with uid, metric, result fields)
#'@export
calculateShoreToVegDistance <- function(uid, shor2rip){
  uid <- as.factor(uid)
  shor2rip <- FactorToNumeric(shor2rip)
  xshor2vg <- igroupMeans(shor2rip, uid, na.rm = T)
  mxshor   <- igroupMaxs( shor2rip, uid, na.rm = T)
  mnshor   <- igroupMins( shor2rip, uid, na.rm = T)

  mets <- data.frame(uid = levels(uid), xshor2vg, mxshor, mnshor)
  meltMetrics(mets)
}

#' Calculates the proportion of the reach over which you can see over the bank
#' 
#' Calculates the metric pct_ovrb; the proportion of the reach over which an
#' observer can see over the bank.  For boatable sites only.
#' @param uid a vector of site identifiers
#' @param see.over.bank a logical vector or vector containing the character string
#' 'YES'.
#' @return a 'metric' data.frame (i.e., one with uid, metric, result fields)
#' @importFrom Rigroup igroupMeans
#' @export
#' @examples
#' uid <- rep(1:10, each = 11)
calculateProportionSeeOverBank <- function(uid, see.over.bank){
  uid <- as.factor(uid)
  if (!is.logical(see.over.bank)){
    see.over.bank <- see.over.bank == 'YES'
  }
  pct_ovrb <- igroupMeans(see.over.bank, uid, na.rm = T)
  mets <- data.frame(uid      = levels(uid),
                     pct_ovrb = pct_ovrb)
  meltMetrics(mets)
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
  as.data.frame.table(tbl, responseName = 'result')
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
getChannelConstraint <- function(uid, parameter, result){
  x <- data.frame(uid = uid, metric = parameter, result = result) 
  x$metric <- 
    factor(x      = as.character(x$metric), 
           levels = c("BANKFULL", "CONSTRNT", "FEATURES", "PATTERN", 
                      "PERCENT", "VALLEY", "VALLYBOX"),
           labels = c("conbankfull", "constraint", "confeatures", "conpattern",
                      "conpercent", "convalley", "convalleybox"))
if(any(is.na(x$metric))){
  stop('There are unknown parameter values in getChannelConstraint')
}
  return(x)
}