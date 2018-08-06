#'LWD class metadata
#'
#'A data frame of LWD size class metadata that is in the LWD metric calculations. The volume
#'of a piece of LWD is determined by the forumla:
#'Volume = pi * [0.5*(minDiam + (maxDiam - minDiam)/3)]^2 * [minLength + (maxLength - minLength)/3]
#'The size variable is used to calculate the cumulative volume and cumulative 
#'count metrics (i.e., c1d, c1d_msq, etc).
#'@return A data frame with the metadata 
WoodClassesData <- function(){
  # Formula for volume wrong in Kaufmann.  Should be:
  # Volume = pi * [0.5*(minDiam + (maxDiam - minDiam)/3)]^2 * [minLength + (maxLength - minLength)/3]
  lwdv <- function(mind, maxd, minl, maxl){
    # Not used here, but used to calculate volumes in dataframe below
    dadj <- (maxd - mind)/3
    ladj <- (maxl - minl)/3
    r <- (mind + dadj) / 2
    l <- minl + ladj
    pi * r^2 * l
  }
  classes <- structure(list(
    diameter      = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 3L, 6L, 7L, 5L, 3L, 6L, 7L, 5L, 3L, 6L, 7L), 
                              .Label = c("[0.1,0.3]", "(0.3,0.6]", "(0.6,0.8]", "(0.8,Inf)", "[0.3,0.6]", "(0.8,1]", "(1,Inf)"), class = "factor"), 
    length        = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 5L, 5L, 5L, 5L), 
                              .Label = c("[1.5,5]", "(5,15]","(15,Inf)", "[5,15]", "(30,Inf)", "(15,30]"), class = "factor"), 
    size.class    = c("sdsl", "mdsl", "ldsl", "xdsl", "sdml", "mdml", "ldml", "xdml", 
                      "sdll", "mdll", "ldll", "xdll", "sdsl", "mdsl", "ldsl", "xdsl", 
                      "sdml", "mdml", "ldml", "xdml", "sdll", "mdll", "ldll", "xdll"), 
    d.lower       = c(0.1, 0.3, 0.6, 0.8, 0.1, 0.3, 0.6, 0.8, 0.1, 0.3, 0.6, 0.8, 0.3, 0.6, 0.8, 1, 0.3, 0.6, 0.8, 1, 0.3, 0.6, 0.8, 1), 
    d.upper       = c(0.3, 0.6, 0.8, 2, 0.3, 0.6, 0.8, 2, 0.3, 0.6, 0.8, 2, 0.6, 0.8, 1, 2, 0.6, 0.8, 1, 2, 0.6, 0.8, 1, 2), 
    l.lower       = c(1.5, 1.5, 1.5, 1.5, 5, 5, 5, 5, 15, 15, 15, 15, 5, 5, 5, 5, 15, 15, 15, 15, 30, 30, 30, 30), 
    l.upper       = c(5, 5, 5, 5, 15, 15, 15, 15, 30, 30, 30, 30, 15, 15, 15, 15, 30, 30, 30, 30, 75, 75, 75, 75), 
    volume        = c(0.0581776417331443, 0.335103216382911, 0.930842267730309, 3.0159289474462, 0.181805130416076, 1.0471975511966, 
                      2.90888208665722, 9.42477796076938, 0.436332312998582, 2.51327412287183, 6.98131700797732, 22.6194671058465, 
                      1.0471975511966, 2.90888208665722, 4.9160107264507, 11.6355283466289, 2.51327412287183, 6.98131700797732, 
                      11.7984257434817, 27.9252680319093, 5.65486677646163, 15.707963267949, 26.5464579228338, 62.8318530717959), 
    size          = c("T", "S", "S", "M", "S", "M", "L", "L", "M", "L", "L", "X", 
                      "T", "S", "S", "M", "S", "M", "L", "L", "M", "L", "L", "X"), 
    diameter.class = c("s", "m", "l", "x", "s", "m", "l", "x", "s", "m", "l", "x", 
                      "s", "m", "l", "x", "s", "m", "l", "x", "s", "m", "l", "x"), 
    length.class   = c("s", "s", "s", "s", "m", "m", "m", "m", "l", "l", "l", "l", 
                      "s", "s", "s", "s", "m", "m", "m", "m", "l", "l", "l", "l"), 
    is.wadeable   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                      TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                      FALSE, FALSE, FALSE, FALSE, FALSE)), 
    .Names        = c("diameter", "length", "size.class", "d.lower", "d.upper", "l.lower", "l.upper", "volume", "size", "diameter.class", "length.class", "is.wadeable"), 
    row.names     = c(NA, 24L), class = "data.frame")
  classes$size <- factor(classes$size, levels = c("T", "S", "M", "L", "X"))
  return(classes)
}

#'Calculate number of LWD observed transects
#'
#'\code{observedLWDTransects} is used to calculate the number of transects that
#'were observed for the LWD data: nc for 'wet' LWD data, ns for 'dry' LWD data,
#'and numtran which is used in \link{calculateLWDSiteLength}.
#'
#'If \code{strict.epa = TRUE}, then numtran just 
#'returns the number of transects per site submitted to the function. If 
#'\code{strict.epa = TRUE}, the function returns the number of \bold{completely
#'observed} transects per site for numtran.
#'
#'@param count vector of counts of LWD pieces
#'@param in.bankfull logical vector; true if the lwd was within bankfull
#'@param uid vector of site-visit indicators
#'@param transect vector of transect names
#'@param strict.epa whether the numtran metric should be calculated like EPA.
#'@import plyr
#'@export
observedLWDTransects <- function(uid, transect, size.class, count, is.wadeable, strict.epa = T){
  x <- prepareLwdData(uid, size.class, count, is.wadeable, transect = transect)
  x$in.bankfull <- mapvalues(x$in.bankfull, c(T, F), c('nc', 'ns'))
  x$observed <- !is.na(x$count)
  ret <- xtabs(observed ~ uid + in.bankfull + transect, x)
  if(strict.epa){
    ret <- rowSums(ret == 12L, dims = 2)
    numtran <- xtabs(~uid, data = unique(x[, c("uid", "transect")]))
    ret <- cbind(ret, numtran = numtran)
  } else {
    ret <- addmargins(ret, 2)
    ret <- sweep(ret, 2, c(12L, 12L, 24L), FUN = '==')
    ret <- rowSums(ret, dims = 2)
    dimnames(ret)[[2]] <- mapvalues(dimnames(ret)[[2]], "Sum", "numtran")
  }
  names(dimnames(ret)) <- c('uid', 'metric')
  progressReport("Finished calculating number of transects observed.")
  return(ret)
}

#'Calculate LWD metrics
#'
#'This function calculates LWD counts within each site, size class, and whether
#'it is within the bankfull area
#'@param uid vector of site-visit indicators
#'@param size.class A vector of size class codes (e.g., WSDSL, DXDLL, ...)
#'@param count vector of counts of LWD pieces
#'@param is.wadeable logical describing whether the site is wadeable or not
#'@param epa.names should the metrics be in data.frame format with EPA's names or in contingency table style.
#'Defaults to EPA style.
#'@import plyr
#'@export
calculateLWDCountMetrics <- function(uid, size.class, count, is.wadeable, epa.names = T){
  x <- prepareLwdData(uid, size.class, count, is.wadeable)
  counts <- xtabs(count ~ length.class + diameter.class + in.bankfull + uid, x, na.action = na.omit)
  counts <- addmargins(counts, 1:3)
  counts <- if(epa.names) countsToMetricDF(counts) else counts
  progressReport("Finished calculating counts of LWD in each length, diameter class.",
                 if (epa.names) counts$metric else NULL)
  counts
}

#'Calculate cumulative LWD metrics
#'
#'This function calculates count and volume metrics by size class.
#'
#'The \code{lwdlength} and \code{lwdarea} argument should be as 
#'calculated by \link{calculateLWDSiteLength} and \link{calculateLWDSiteArea}
#'functions, respectively. 
#'
#'@return If the \code{epa.names} argument is \code{FALSE}, then the
#'function returns an array of cumulative counts within size classes. 
#'If \code{TRUE}, the function returns a metric data.frame with epa 
#'metric names.
#'
#'@param uid vector of site-visit indicators
#'@param size.class A vector of size class codes (e.g., WSDSL, DXDLL, ...)
#'@param count vector of counts of LWD pieces
#'@param is.wadeable logical describing whether the site is wadeable or not
#'@param lwdlength numeric, the total stream length over which LWD has been quantified at the site.
#'@param lwdarea numeric, the total area over which LWD has been quantified at the site.
#'@param epa.names should the metrics be in data.frame format with EPA's names or in contingency table style.
#'Defaults to EPA style.
#'@export
calculateLWDCumulativeMetrics <- function(uid, size.class, count, is.wadeable, lwdlength, lwdarea, epa.names = T){
  x <- prepareLwdData(uid, size.class, count, is.wadeable, lwdlength = lwdlength, lwdarea = lwdarea)
  ccounts <- xtabs(cbind(volume, count) ~ size + in.bankfull + uid, x)
  ccounts <- addmargins(ccounts, 2)
  ccounts <- apply(ccounts, 2:4, function(x) cumsum(rev(x)))
  m100 <- sweepSiteStatistic(x$lwdlength, x$uid, ccounts) * 100
  msq  <- sweepSiteStatistic(x$lwdarea,   x$uid, ccounts[,"TRUE",,, drop = F])
  ret <- list(cumulative.counts      = ccounts, 
              cumulative.counts.m100 = m100, 
              cumulative.counts.msq  = msq)
  ret <- if (epa.names) ccountsToMetricDF(ret) else ret 
  progressReport("Finished calculating cumulative counts of LWD in each size class.",
                 if (epa.names) ret$metric else NULL)
  ret
}

countsToMetricDF <- function(x){
  kDimensionMap  <- c('Sum' = 't')
  kInBankfullMap <- c('Sum' = 't', 'TRUE' = 'w','FALSE' = 'd')
  ret <- reshape2:::melt(x, value.name = 'result')
  ret <- revalueDataFrame(ret,
                          list(in.bankfull    = kInBankfullMap, 
                               diameter.class = kDimensionMap, 
                               length.class   = kDimensionMap))
  ret$metric <- tolower(paste('rch', ret$in.bankfull, ret$diameter.class, 'd', ret$length.class, 'l', sep = ''))
  ret$metric <- renameLWDCountMetrics(ret$metric)
  arrange(ret[,c('uid', 'metric', 'result')], uid, metric)
}

renameLWDCountMetrics <- function(x, revert = F){
  kMetricMap <- c(rchdsdtl = "smdrydia", rchwsdtl = "smwetdia", 
                  rchtsdtl = "smdiatot", rchdmdtl = "mddrydia", rchwmdtl = "mdwetdia", 
                  rchtmdtl = "mddiatot", rchdldtl = "lgdrydia", rchwldtl = "lgwetdia", 
                  rchtldtl = "lgdiatot", rchdxdtl = "xldrydia", rchwxdtl = "xlwetdia", 
                  rchtxdtl = "xldiatot", rchdtdsl = "shdrylen", rchwtdsl = "shwetlen", 
                  rchttdsl = "shlentot", rchdtdml = "mddrylen", rchwtdml = "mdwetlen", 
                  rchttdml = "mdlentot", rchdtdll = "lgdrylen", rchwtdll = "lgwetlen", 
                  rchttdll = "lglentot", rchdtdtl = "rchdryt",  rchwtdtl = "rchwett", 
                  rchttdtl = "rchwdt")
  if(revert){
    mapvalues(x, kMetricMap, names(kMetricMap))
  } else {
    mapvalues(x, names(kMetricMap), kMetricMap)
  }
}

ccountsToMetricDF <- function(x){
  kInBankfullMap <- c('Sum' = 't', 'TRUE' = 'w','FALSE' = 'd')
  kSizeMap       <- c('X' = '5', 'L' = '4', 'M' = '3', 'S' = '2', 'T' = '1')
  x <- lapply(x, function(x){
    names(dimnames(x))[4] <- 'metric'
    return(x)
  })
  x <- rename(x, c(cumulative.counts      = "",
                   cumulative.counts.m100 = "m100", 
                   cumulative.counts.msq  = "_msq"))
  ans <- do.call('meltMetrics', x)
  ans <- revalueDataFrame(ans, list(in.bankfull = kInBankfullMap,
                                    size        = kSizeMap))
  # Get the metric names right
  ans$metric <- paste0(substr(ans$metric, 1, 1), ans$size, ans$in.bankfull, ans$.id)
  arrange(ans[,c('uid', 'metric', 'result')], uid, metric)
}

#'Calculate length of area sampled for LWD
#'
#'\code{calculateLWDSiteLength} is used to calculate the area of the sampled
#'lwd plots so that counts/area can be calculated.
#'
#'@param lwdlength a vector of site lengths as returned by \link{calculateLWDSiteLength}
#'@param xbkf_w a vector of average bankfull width as returned by \link{calculateChannelMetrics}
#'@param is.wadeable a logical vector; \code{TRUE} if the site was wadeable
#'@export
calculateLWDSiteArea <- function(lwdlength, xbkf_w, is.wadeable){
  # Calculates the area in which LWD data is collected
  ifelse(is.wadeable,
         xbkf_w * lwdlength,
         lwdlength * 10)
}

#'Calculate length of area sampled for LWD
#'
#'\code{calculateLWDSiteLength} is used to calculate the length of the sampled
#'area so that counts/length and counts/area can be calculated. For wadeable,
#'sites the length of stream where lwd is sampled is just the total reach length
#'for the site, because the lwd plots are the entire area between the transects.
#'For non-wadeable sites, the site length is the number of sampled transects * 20,
#'because each littoral plot is 20 m long.
#'
#'@param reachlen a vector of reachlength metrics for the sites. For wadeable,
#'sites use \link{calculateWadeableReachLength}. For Non-wadeable sites, use 
#'\link{calculateBoatableReachLength}.
#'@param numtran a vector with the number of transects sampled. Use \link{observedLWDTransects},
#'to determine the number of lwd transects measured.
#'@param is.wadeable a logical vector; \code{TRUE} if the site was wadeable
#'@export
calculateLWDSiteLength <- function(reachlen, numtran, is.wadeable){
  # Calculates the length of stream reach over which LWD data is collected
  ifelse(is.wadeable,
         reachlen,
         numtran * 20)
}

sweepSiteStatistic <- function(x, uid, arr, uid.FUN = unique, sweep.FUN = '/'){
  uid.dim <- which(names(dimnames(arr)) == 'uid')
  xuid <- tapply(x, uid, uid.FUN)[dimnames(arr)$uid]
  sweep(arr, uid.dim, xuid, FUN = sweep.FUN)
}

prepareLwdData <- function(uid, size.class, count, is.wadeable, ...){
  kAllowedCodes  <- c("dldll", "dldml", "dldsl", "dmdll", "dmdml", "dmdsl", "dsdll", 
                      "dsdml", "dsdsl", "dxdll", "dxdml", "dxdsl", "wldll", "wldml", 
                      "wldsl", "wmdll", "wmdml", "wmdsl", "wsdll", "wsdml", "wsdsl", 
                      "wxdll", "wxdml", "wxdsl")
  other.vars <- list(...)
  size.class <- tolower(size.class)
  stopifnot(is.logical(is.wadeable), size.class %in% kAllowedCodes)
  x <- data.frame(count       = count, 
                  uid         = uid, 
                  in.bankfull = substring(size.class, 1, 1) == 'w',
                  size.class  = substring(size.class, 2, 5),
                  is.wadeable = is.wadeable)
  if(length(other.vars)) x <- data.frame(x, other.vars)
  x <- merge(x, WoodClassesData(), by = c('size.class', 'is.wadeable'), all.x = T)
  x$volume <- x$count * x$volume
  x
}
