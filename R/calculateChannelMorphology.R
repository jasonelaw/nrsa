sumNA <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  } else {
    sum(x, ...)
  }
}

#' Join extra side channel transects
#' 
#' This function joins the extra side channel transects with the parallel transect.
#' Each measurement on the main channel is joined with the parallel measurement 
#' on the side channel using an aggregation function.  All measurements done at
#' the transect (rather than at a station) are assumed to be at station 0 for purposes
#' this function.  This allows us to do the join in one pass rather than separately.
#' @importFrom plyr ddply
#' @importFrom stringr str_replace
#' @export
#' @examples
#' d <- data.frame(uid = c(1,1), transect = c('A', 'XA'), station = c(0,0), wetwid = 1:2, bankwid = 1:2, bankhgt = 1:2, incishgt = 1:2)
#' do.call('joinExtraTransects', d)
joinExtraTransects <- function(uid, transect, station, wetwid, bankwid, bankhgt, incishgt){
  transect <- str_replace(transect, 'X', '')
  x <- data.frame(uid, transect, station, wetwid, bankwid, bankhgt, incishgt)
  f <- function(x){
    suppressWarnings(
      c(wetwid.sm   = sumNA(x$wetwid, na.rm = T),
        wetwid.mx   = max(x$wetwid, na.rm = T),
        bankwid.sm  = sumNA(x$bankwid, na.rm = T),
        incishgt.mx = max(x$incishgt, na.rm = T),
        bankhgt.mx  = max(x$bankhgt, na.rm = T),
        bankhgt.mn  = mean(x$bankhgt, na.rm = T)))
  }
  ans <- ddply(x, .(uid, transect, station), f)
  ans$incishgt.mx <- ifelse(is.na(ans$incishgt.mx) | ans$incishgt.mx == 0, 
                            ans$bankhgt.mx, 
                            ans$incishgt.mx)
  replaceInfNaN <- function(x){
    x[!is.finite(x)] <- NA
    return(x)
  }
  ans[4:9] <- lapply(ans[4:9], replaceInfNaN)
  return(ans)
}

#' Calculate channel metrics
#' 
#' Calculate the channel metrics: xbkf_w, sdbkf_w, n_bw, xinc_h, sdinc_h, n_incis, 
#' xbkf_h, sdbkf_h, n_bh.
#' 
#' The EPA uses data where the extra transect - transect pairs have been joined.  
#' The \code{bankwid} argument is the sum of the extra transect pairs, the \code{incishgt} 
#'  and \code{bankhgt} arguments are the max of the extra transect pairs.  These can 
#'  be calculated using \link{joinExtraTransects}.
#' @param uid a vector of site identifiers
#' @param bankwid a vector of bankfull width measurements; sum of extra transect pairs
#' @param incishgt a vector of incision heights; max of extra transect pairs
#' @param bankhgt a vector of bankfull heights; max of extra transect pairs
#' @export
#' @examples
#' d <- data.frame(uid = rep(1:10, 11), wetwid = runif(110), bankwid = runif(110), incishgt = runif(110), bankhgt = runif(110))
#' calculateChannelMetrics(d$uid, d$wetwid, d$bankwid, d$incishgt, d$bankhgt)
calculateChannelMetrics <- function(uid, bankwid, incishgt, bankhgt){
  kNamesMap <- c("mean.bankwid" = "xbkf_w",  "sd.bankwid" = "sdbkf_w", "count.bankwid" = "n_bw",
                 "mean.incishgt" = "xinc_h", "sd.incishgt" = "sdinc_h", "count.incishgt" = "n_incis", 
                 "mean.bankhgt" = "xbkf_h", "sd.bankhgt" = "sdbkf_h", "count.bankhgt" = "n_bh")

  x <- data.frame(uid, bankwid, incishgt, bankhgt)
  xm <- melt(x, id.var = 'uid', variable.name = 'parameter', value.name = 'result')
  f <- function(x){
    c(mean  = mean(x$result, na.rm = T),
      sd    = sd(x$result, na.rm = T),
      count = count(x$result))
  }
  ans <- ddply(xm, .(uid, parameter), f)
  ans <- melt(ans, id.var = c('uid', 'parameter'), variable.name = 'metric', value.name = 'result')
  ans$metric <- paste(ans$metric, ans$parameter, sep = '.')
  ans$metric <- revalue(ans$metric, kNamesMap)
  ans$parameter <- NULL
  return(ans)
}

#' Calculates wetted width metrics
#' 
#' Calculates wetted width metrics: xwidth, sdwidth, and n_w.
#' 
#' Uses the max wetted width of the transect - extra transect pair as returned 
#' by \link{joinExtraTransects}.
#' @param uid a vector of site identifiers
#' @param wetwid a vector of wetted widths; uses the max of the extra transect pairs
#' @return a 'metric' data.frame
#' @importFrom plyr ddply
#' @importFrom NARSShared count
#'@export
calculateWettedWidthMetrics <- function(uid, wetwid){
  x <- data.frame(uid, wetwid)
  f <- function(x){
    c(xwidth  = mean(x$wetwid, na.rm = T),
      sdwidth = sd(x$wetwid, na.rm = T),
      n_w     = count(x$wetwid))
  }
  ans <- ddply(x, .(uid), f)
  ans <- meltMetrics(ans)
  return(ans)
}

#' Calculate thalweg width depth ratios
#' 
#' Calculate metrics dealing with the product and ratio of the channel width and
#' depth: xwxd, sdwxd, n_wd, xwd_rat, sdwd_rat, and n_wdr.
#' 
#' These are station level data (i.e., from the thalweg form) and so do not need any special
#' extra transect treatment.
#' @param uid a vector site identifiers
#' @param wetwid a vector of wetted widths
#' @param depth a vector of thalweg depths
#' @return a 'metric' data.frame
#' @export
#' @importFrom plyr ddply
calculateThalwegRatios <- function(uid, wetwid, depth){
  wdprod <- wetwid * depth
  wdratio <- wetwid / depth
  x <- data.frame(uid, wdprod, wdratio)
  f <- function(x){
    c('xwxd'     = mean(x$wdprod, na.rm = T),
      'sdwxd'    = sd(x$wdprod, na.rm = T),
      'n_wd'     = count(x$wdprod),
      'xwd_rat'  = mean(x$wdratio, na.rm = T),
      'sdwd_rat' = sd(x$wdratio, na.rm = T),
      'n_wdr'    = count(x$wdratio))
  }
  ans <- ddply(x, .(uid), f)
  meltMetrics(ans)
}

#' Calculates metrics of bankfull width to (bankfull height + depth) ratios
#' 
#' This function calculates summary metrics of the ratio: bankfull width / 
#' (bankfull height + thalweg depth).  The mean and count of measurements are
#' calculated.  
#' 
#' The EPA calculations used extra trnasects in these calculations: 
#' sum of the bankfull widths at the extra transect and regular transect and the
#' mean of the bankfull height at the extra transect and the regular transect.  
#' These values are returned by the \link{joinExtraTransects} function.
#' @param uid a vector of site identifiers
#' @param bankwid a vector of bankfull widths; EPA uses the sum of the extra transects.
#' @param bankhgt a vector of bankfull heights; EPA uses the mean of the extra transects.
#' @param depth a vector of thalweg depths
#' @importFrom plyr ddply
#'@export
calculateTransectRatios <- function(uid, bankwid, bankhgt, depth){
  bfwd <- bankwid / (bankhgt + depth)
  x <- data.frame(uid, bfwd)
  f <- function(x){
    c('bfwd_rat' = mean(x$bfwd, na.rm = T),
      'n_bfrat'  = count(x$bfwd))
  }
  ans <- ddply(x, 'uid', f)
  meltMetrics(ans)
}

#' Calculate thalweg depth metrics: xdepth, sddepth, n_d
#' 
#' Calculates the thalweg depth metrics (xdepth, sddepth, n_d) form thalweg depth
#' data.
#' 
#' The EPA code assumes input data in meters, which this function also assumes.
#' However, the EPA code reports the metrics for wadeable sites in centimeters.
#' This code includes an argument \code{units}, which allows the user to specify
#' the desired output units.  The \code{mixed} option mimics the EPA behavior.
#' @param uid a vector of site identifiers
#' @param is.wadeable a logical vector, TRUE for each depth that is from
#' a wadeable protocol site (versus a non-wadeable protocol).
#' @param depth a vector of thalweg depths
#' @param desired output units.  Can be one of \code{"m"}, \code{"cm"}, or \code{"mixed"}.
#' @return a 'metric' data.frame
#' @importFrom plyr ddply
#' @importFrom NARSShared count
#' @export
calculateThalwegDepthMetrics <- function(uid, is.wadeable, depth, units = c('m', 'cm', 'mixed')){
  #ensure units match: some notes and code about converting from m to cm
  units <- match.arg(units)
  depth <- switch(units,
                  m = depth,
                  cm = depth * 100,
                  mixed = ifelse(is.wadeable, depth * 100, depth))
  x <- data.frame(uid, depth)
  f <- function(x){
    c(xdepth  = mean(x$depth, na.rm = T),
      sddepth = sd(x$depth, na.rm = T),
      n_d     = count(x$depth))
  }
  ans <- ddply(x, .(uid), f)
  meltMetrics(ans)
}
