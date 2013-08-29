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
joinExtraTransects <- function(uid, transect, station, wetwid, bankwid, bankhgt, incishgt){
  transect <- str_replace(transect, 'X', '')
  x <- data.frame(uid, transect, station, wetwid, bankwid, bankhgt, incishgt)
  f <- function(x){
    suppressWarnings(
      c(wetwid.sm   = sumNA(x$wetwid, na.rm = T),
        wetwid.mx   = max(x$wetwid, na.rm = T),
        bankwid     = sumNA(x$bankwid, na.rm = T),
        incishgt    = max(x$incishgt, na.rm = T),
        bankhgt.mx  = max(x$bankhgt, na.rm = T),
        bankhgt.mn  = mean(x$bankhgt, na.rm = T)))
  }
  ans <- ddply(x, .(uid, transect, station), f)
  ans$incishgt <- ifelse(is.na(ans$incishgt) | ans$incishgt == 0, 
                         ans$bankhgt.mx, 
                         ans$incishgt)
  replaceInfNaN <- function(x){
    x[!is.finite(x)] <- NA
    return(x)
  }
  ans[4:9] <- lapply(ans[4:9], replaceInfNaN)
  return(ans)
}

#' Calculate channel metrics
#' 
#' Calculate channel metrics
#' @param uid
#' @param wetwid
#' @param bankwid
#' @param incishgt
#' @param bankhgt
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
#' Calculates wetted width metrics
#' 
#' Uses the max wetted width of the transect - extra transect pair as returned 
#' by \link{joinExtraTransects}.
#' @param uid a vector of site identifiers
#' @param wetwid a vector of wetted widths; uses the max of the transect - extra
#'  transect pair
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
#' depth
#' @param uid
#' @param wetwid
#' @param depth
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

#'@export
calculateTransectRatios <- function(uid, bankwid, bankhgt, depth){
  bfwd <- bankwid / (bankght + depth)
  x <- data.frame(uid, bfwd)
  f <- function(x){
    c('bfwd_rat' = mean(x$bfwd, na.rm = T),
      'n_bfrat'  = count(x$bfwd))
  }
  ans <- ddply(x, 'uid', f)
  meltMetrics(ans)
}

#'@export
calculateThalwegDepthMetrics <- function(uid, depth){
  #ensure units match: some notes and code about converting from m to cm
  x <- data.frame(uid, depth)
  f <- function(x){
    c(xdepth  = mean(x$depth, na.rm = T),
      sddepth = sd(x$depth, na.rm = T),
      n_d     = count(x$depth, na.rm = T))
  }
  ans <- ddply(x, .(uid), f)
  meltMetrics(ans)
}
