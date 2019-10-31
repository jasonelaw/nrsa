
calculateResidualPoolMetrics <- function(uid, transect, station, is.wadeable, depth, increment, slope){
  x <- organizeThalwegData(uid, transect, station, is.wadeable, depth, increment, slope)
  x$distance <- nrsa:::calculateDistanceAlongThalweg(x$loc, x$increment, x$is.wadeable, x$uid)
  pool <- nrsa:::calculatePool(x$distance, x$depth, x$slope, x$uid)
  pool$reachlen <- calculatePoolReachLength(uid, loc, increment)
  calculatePoolMetrics(pool)
}

#' @param uid a vector of site identifiers
#' @param transect a vector of transect identifiers
#' @param station a vector of station identifiers
#' @param is.wadeable logical; \code{TRUE} if the site is wadeable
#' @param depth a vector of thalweg depths
#' @param increment a vector of distances from each thalweg point to the
#'   previous
#' @param slope a vector of average site slope
#' @param minimum.proportion the minimum proportion sampled necessary for the
#'   site to be included.
#' @export
#' @import plyr
organizeThalwegData <- function(uid, transect, station, is.wadeable, depth, increment, slope, minimum.proportion = 0.85){
  x <- data.frame(uid, transect, station, depth, increment, slope, is.wadeable)
  site.seq <- createSiteSequence(x$uid, x$transect, x$station, x$is.wadeable)
  x <- plyr::join(x, site.seq, by = c('uid', 'transect', 'station'))
  x <- x[order(x$uid, x$loc), ]
  nicks <- createNickPoints(x$uid, x$loc, x$depth, x$increment, x$slope, x$is.wadeable)
  x <- rbind.fill(x, nicks)
  i <- identifyIncompleteSites(x$uid, x$loc, x$depth, minimum.proportion = minimum.proportion)
  message('Removing incomplete sites:', toString(sQuote(unique(x$uid[i]))))
  x <- subset(x, !i, select = c(uid, transect, station, loc, depth, increment, slope, is.wadeable))
  arrange(x, uid, loc)
}

createThalwegProfile <- function(uid, transect, station, is.wadeable, depth, increment, slope){
  x <- data.frame(uid, transect, station, is.wadeable, depth, increment, slope)
  x <- join(x, ddply(x, .(uid), function(x) c(max.station.site = max(x$station[!is.na(x$depth)]) + 1)), by = 'uid')
  x <- join(x, ddply(x, .(uid, transect), function(x) c(max.station.site = max(x$station[!is.na(x$depth)]) + 1)), by = c('uid', 'transect'))
  class(x) <- c('thalweg', 'data.frame')
  x
}

#' @examples 
#' x <- expand.grid(uid = 1:5, transect = LETTERS[1:10], station = 0:9)
#' x$is.wadeable <- rep(c)
#' createSiteSequence(x$uid, x$transect, x$station, x$is.wadeable)
#' @import plyr
createSiteSequence <- function(uid, transect, station, is.wadeable){
  is.wadeable[is.na(is.wadeable)] <- TRUE #assume wadeable if missing
  d <- data.frame(uid, transect, station)
  d <- arrange(d, uid, transect, station)
  x <- split(d, is.wadeable)
  wade <- function(x){
    n.station <- nWadeableStationsPerTransect(x$uid, x$transect, x$station)
    x <- merge(x, n.station, by = c('uid', 'transect'))
    nt <- mapvalues(x$transect, LETTERS[1:11], 0:10, warn_missing = FALSE)
    nt    <- as.numeric(as.character(nt))
    x$loc <- as.integer(nt * x$n.station + x$station + 1)
    x$n.station <- NULL
    x
  }
  boat <- function(x){
    f <- function(x){
      nt <- mapvalues(x$transect, LETTERS[1:11], 0:10, warn_missing = FALSE)
      nt <- as.numeric(as.character(nt))
      n.station       <- tapply(x$station, x$transect, max)
      cum.n.station   <- cumsum(n.station)
      x$n.station     <- n.station[x$transect]
      x$cum.n.station <- cum.n.station[x$transect]
      x$loc <- x$cum.n.station - x$n.station + nt + x$station
      x     <- arrange(x, desc(loc))
      x$loc <- rev(x$loc) + 1
      x$n.station <- x$cum.n.station <- NULL
      x
    }
    ddply(x, .(uid), f)
  }
  wans <- bans <- NULL
  if(any(!is.wadeable)){
    bans <- boat(x[['FALSE']])
  }
  if(any(is.wadeable)){
    wans <- wade(x[['TRUE']])
  }
  ans <- rbind(wans, bans)
  arrange(ans, uid, loc)
}

filterBoatable <- function(station, depth){
  sta <- as.numeric(levels(station))[station]
  x[!(is.na(depth) & sta > 9), ]
}

makeLoc <- function(uid, n.station, wadeable = T){
   f <- function(uid, n.station, wadeable){
     transect <- factor(tlev <- LETTERS[1:10],   levels = if(wadeable) tlev else rev(tlev))
     station <- factor(slev <- 0:(n.station-1), levels = if(wadeable) slev else rev(slev))
     ret <- expand.grid(uid, transect, station)
     ret$loc <- as.numeric(interaction(ret$transect, ret$station, drop = T, lex.order = T))
     ret
   }
  ret <- plyr::mdply(data.frame(uid, n.station, wadeable), f)
  return(ret[order(ret$uid, ret$loc), c('uid', 'transect', 'station', 'loc')])
}

transectFactor <- function(x, is.wadeable){
  tlev <- LETTERS[1:10]
  factor(as.character(x), levels = if(is.wadeable) tlev else rev(tlev))
}

stationFactor <- function(x, is.wadeable){
  x <- as.integer(as.character(x))
  slev <- 0:max(x)
  factor(x, levels = if(is.wadeable) slev else rev(slev))
}

wadeThalweg <- function(n){
  df <- expand.grid(transect = transectFactor(LETTERS[1:10], T),
                    station  = stationFactor(0:(n-1), T))
  df$loc <- interaction(df$transect, df$station, lex.order = T, drop = T)
  df[order(df$loc),]
}

boatThalweg <- function(transect, station){
  df <- expand.grid(transect = transectFactor(transect, F),
                    station  = stationFactor(0:11, F))
  df[order(df$transect, df$station),]
}

# Create site order based on design

# Wadeable sites have the same nuber of stations per transect. Boatable stations may have a variable
# number from 10 - 12 because of the difficulty of evenly spacing the stations from a boat.
# 
makeLoc2 <- function(transect, n.station, wadeable = T){
   f <- function(transect, n.station, wadeable){
     station <- factor(slev <- 0:(n.station-1), levels = if(wadeable) slev else rev(slev))
     ret <- data.frame(transect, station)
     ret
   }
  tlev <- LETTERS[1:10]
  transect <- factor(as.character(transect), levels = if(wadeable) tlev else rev(tlev))
  ret <- plyr::mdply(data.frame(transect, n.station), f, wadeable = wadeable)
  ret$loc <- as.numeric(interaction(ret$transect, ret$station, drop = T, lex.order = T))
  return(ret[order(ret$loc), c('transect', 'station', 'loc')])
}

#' Identify incomplete sites
#' 
#' Identify sites whose proportion of non-missing depths falls below a minimum.
#' @param uid a vector of site identifiers
#' @param loc a vector of thalweg sequence ids
#' @param depth a vector of thalweg depths
#' @param minimum proportion of non-missing depths
identifyIncompleteSites <- function(uid, loc, depth, minimum.proportion = 0.85){
  uid <- factor(uid)
  n.expected <- tapply(loc, uid, max, na.rm = T)
  n.missing  <- tapply(depth, uid, function(x) sum(is.na(x)))
  n.present  <- tapply(loc, uid, count.notna)
  prop.sampled <- (n.present - n.missing) / n.expected
  incomplete   <- prop.sampled < minimum.proportion
  incomplete[uid]
}



#' @import plyr
createNickPoints <- function(uid, loc, depth, increment, slope, is.wadeable, ...){
  i <- loc == ave(loc, uid, FUN = min)
  nickDepth <- function(x){
    xdepth  <- mean(x$depth, na.rm = T)
    sddepth <- sd(x$depth, na.rm = T)
    c(depth = pmax(xdepth - sddepth, 0))
  }
  nick.depth <- ddply(data.frame(uid, depth), .(uid), nickDepth)
  nick <- data.frame(uid = uid[i], loc = 0, increment = increment[i], slope = slope[i], is.wadeable = is.wadeable[i])
  nick <- merge(nick, nick.depth, by = 'uid')
  return(nick)
}

#' Calculate distance from sequence of station numbers and increments
#' 
#' Calculate distance along thalweg profile from a sequence of station numbers 
#' and increments between stations.  The first station number is expected to be
#' 0.  Here, the distance will be 0.  The increments are assumed to be the distance
#' from the previous station.
#' 
#' Not sure if station is really necessary.  Only necessary if there are missing
#' values in station numbers.  That is, if the sequence is allowed to skip from
#' station 8 to 10 with no station 9.  This implementation acknowledges that, using
#' \link{diffinv} if there are no station gaps.
#' 
#' In addition, there seems to be a problem with which increment values are used
#' in wadeable versus boatable sites to figure the distance.  This may make some
#' sort of sense because of the reversal of wadeable versus boatable sampling directions.
#' This adds some complexity to the function and requires the protocol.
#' @param station a vector of station ids, starting at 0.
#' @param increment a vector of distances from the previous station to the current
#' @param is.wadeable logical, TRUE if the measurement comes from a wadeable site
#' @param uid an optional site indicator or grouping variable. Calculations are
#' performed separately for group defined by \code{uid}
#' @return a vector of distances
#' @export
#' @import plyr
#' @examples
#' calculateDistanceAlongThalweg(0:9, rep(5,10))
#' calculateDistanceAlongThalweg(c(0:8,11), rep(5,10))
#' calculateDistanceAlongThalweg(rep(0:4, times = 2), rep(5,10), rep(1:2, each = 5))
calculateDistanceAlongThalweg <- function(station, increment, is.wadeable, uid = NULL) {
  stopifnot(identical(length(station), length(increment)),
            identical(length(station), length(is.wadeable)))
  f <- function(station, increment, is.wadeable, ...){
    station.miss <- isTRUE(all.equal(diff(station), rep(1, length(station) - 1)))
    is.wadeable <- unique(is.wadeable)
    stopifnot(identical(as.integer(length(is.wadeable)), 1L))
    rem <- if (is.wadeable) length(increment) else 1L
    if (station.miss) {
      distance <- diffinv(increment[-rem])
    } else {
      distance <- cumsum(diff(station) * increment[rem])
      distance <- c(0, distance)
    }
    return(distance)
  }
  if (is.null(uid)){
    return(f(station, increment, is.wadeable))
  } else {
    x <- data.frame(uid, station, increment, is.wadeable)
    lans <- dlply(x, .(uid), splat(f))
    ans  <- unlist(lans, use.names = FALSE)
    return(ans)
  }
}

#' @import plyr
calculatePoolReachLength <- function(uid, loc, increment){
  x <- data.frame(uid, loc, increment)
  ans <- ddply(x, .(uid), function(x){
    i <- x$loc %in% c(0, max(x$loc))
    c(reachlen = sum(x$increment[!i], na.rm = T))
  })
  ans$reachlen[match(as.character(x$uid), as.character(ans$uid))]
}

#' Determine which depths in a vector of depths represent pools
#' 
#' Determine which depths in a vector of depths represent pools and return 
#' information about them.  A residual pool is any area in a pool that holds 
#' water when the discharge is 0.
#' @param distance a vector of cumulative distance for the thalweg profile
#' @param depth a vector of thalweg profile depths
#' @param slope the average site slope; if uid is passed this is expected to be 
#'   a vector as long as the other arguments.  Otherwise it may be a length 1
#'   numeric.
#' @param uid a vector of site identifiers: pools are calculated separately for
#'   each site
#' @param convert.stack logical, if \code{TRUE} the slope is converted using the
#'   formula of Stack (1988).
#' @param pool.only logical, if \code{TRUE}, any nonpool points are removed from the
#' data.frame that is returned.
#' @import plyr
#' @export
calculatePool <- function(distance, depth, slope, uid = NULL, convert.stack = F, pool.only = F) {
  if (convert.stack) {
    #slope formula according to Stack (1988)
    slope <- 0.12 + 0.25 * slope
  } 
  f <- function(distance, depth, slope, ...){
    slope <- unique(slope)
    dimensions <- calculatePoolDimensions(distance, depth, slope)
    pool.info  <- getPoolInformation(dimensions)
    return(pool.info)
  }
  if (is.null(uid)){
    pool.info <- f(distance, depth, slope)
  } else {
    x         <- data.frame(uid, distance, depth, slope)
    pool.info <- ddply(x, .(uid), splat(f), .progress = plyrProgress())
  }
  if (pool.only){
    pool.info <- pool.info[pool.info$is.pool, ]
  }
  progressReport("Residual pool dimensions are finished.")
  return(pool.info)
}

#' Residual depths from a thalweg profile
#' 
#' Calculate residual pool depths from a thalweg profile containing distance 
#' depth and slope, treating every depth as a possible base depth.
#' 
#' The resulting residual depths are returned in the lower triangle of a n * n 
#' matrix, where n is the number of depths.  The diagonal of the matrix
#' represents the succesive base points. Each row after the diagonal gives the
#' residual depths relative to the base point for that column.  Any row with a
#' residual depth > 0 represents a thalweg point that is in a pool.
#'     /
#'    /|
#'   / |  
#'  /  |
#' /
#' @param d a vector of distances along a thalweg profile
#' @param y a vector of thalweg profile depth measurements
#' @param s a site wide average slope of length 1

#' @return a n * n lower diagonal matrix containing the residual depths.  Each 
#'   column represents the residual depths calculated treating a different
#'   thalweg point as the base depth.
#' @examples
#' allResidualDepth(0:9, runif(10), .01)
residualDepth <- function(d, y, s){
  # depth - (base.depth + (distance * slope))
  # depth -  base.depth - (distance * slope)
  n   <- length(d)
  dim <- c(n, n)
  tri <- .row(dim) > .col(dim)
  d <- dist(d)                     # distance[i] - distance[j], where i>j
  y <- outer(y, y, FUN = '-')[tri] #    depth[i] -    depth[j], where i>j
  s <- matrix(s, dim[1], dim[2])[tri] # allows for variable slopes over thalweg profile although not currently used.
  rd <- y - d * s                  # residual depth for depth[i], base.point[j]
  abind::abind(distance = inv.lower.tri(d), depth = inv.lower.tri(rd), along = 3)
}

calculatePoolDimensions <- function(distance, depth, slope){
  stopifnot(identical(length(distance), length(depth)), !is.na(slope))
  # Here is the actual computation...
  ret <- residualDepth(distance, depth, slope)
  # set values before the base point to missing.
  len.mat <- rbind(NA, matrixStats::colDiffs(ret[,,'distance']))
  area.mat <- len.mat * ret[,,'depth']
  return(abind::abind(ret, length = len.mat, area = area.mat, along = 3))
}

#' Determines basic information about a sequence of pools
#' 
#' Identifies pool base points, gives ids to individual pools, and numbers
#' points within a pool. from a matrix of residual pool depths.
#' 
#' The function first identifies which point on the thalweg profile is in a
#' pool. For a logical vector, the function then identifies runs in the logical
#' vector. Then it identifies the first point in each run.  Then sets the non
#' pools to FALSE. Then identifies which points are still TRUE and substracts 1
#' to get the previous point.
#' 
#' @param x a lower triangular matrix of residual pool dimensions.
#' @return a data.frame of pool information.
#' @export
getPoolInformation <- function(dimensions) {
  # do any rows of depth matrix have a TRUE value?
  is.pool <- matrixStats:::rowAnys(dimensions[, , 'depth'] > 0, 
                                   na.rm = T)
  
  i <- seq_along(is.pool)
  pools       <- rle(is.pool)
  pool.starts <- rle.start(pools)[pools$values]
  pool.ends   <- rle.end(pools)[pools$values]
  
  ret <- data.frame(is.pool,
                    pool.id    = rle.id(pools),
                    pool.pt    = sequence(pools$lengths),
                    base.point = i %in% (pool.starts - 1),
                    depth = NA, length = NA, distance = NA, area = NA)
  dimensions <- dimensions[,ret$base.point,, drop = F]
  profiles <- mapply(FUN = function(x, i, j){x[i:j,, drop = F]}, 
                     x = alply(dimensions, 2), 
                     i = pool.starts, 
                     j = pool.ends, SIMPLIFY = FALSE)
  ret[ret$is.pool,c('distance', 'depth', 'length', 'area')] <- do.call('rbind', profiles)
  ret
}

run.info <- function(x){
  #stopifnot(is.logical(x))
  if(inherits(x, 'rle')){
    rx <- x
  } else {
    rx <- rle(x)
  }
  data.frame(id = seq_along(rx$values), values = rx$values, lengths = rx$lengths, start = start(rx), end = end(rx))
}

inverse.run.info <- function(x){
  ri <- run.info(x)
  ret <- ri[rep.int(ri$id, ri$lengths), ]
  ret$i <- sequence(ri$lengths)
  ret
}


#' Calculate residual pool metrics
#' 
#' Calculates residual pool metrics from data set where the residual pool length,
#' depth, and area have been calculated separately for each depth in a thalweg profile.
#' @param uid a vector of site identifiers
#' @param pool a pool identifier; unique for each pool within a site
#' @param depth the residual pool depth for that point in the profile
#' @param length the residual pool length for that point in the profile 
#' @param area the residual pool area for that point in the profile
#' @param reachlen a vector of reach lengths for each uid; 
#' @return a metric data.frame
#' @export
#' @import plyr
calculatePoolMetrics <- function(uid, pool, depth, length, area, reachlen, ...){
  calcPoolSummaries <- function(x){
    ans <- c(poolen = sum(x$length, na.rm = T),
             poolar = sum(x$area, na.rm = T),
             xdep   = mean(x$depth, na.rm = T),
             rpvdep = sd(x$depth, na.rm = T))
    depmet.names <- c('mindep', 'dep25','meddep', 'dep75', 'rpmxdep')
    depmet <- quantile(x$depth, probs = seq(0, 1, 0.25), type = 2, na.rm = T)
    ans <- c(ans, setNames(depmet, depmet.names))
    return(ans)
  }
  
  calcSitePoolSummaries <- function(x){
    c(rpxdep = mean(x$depth, na.rm = T), # these all have weird ifelse() constructions in EPA code.
      rpvdep = sd(x$depth, na.rm = T),
      rpmxdep = max(x$depth, na.rm = T))
  }
  
  calcUidSummaries <- function(x){
    ints <- c(0.05, 0.1, 0.2, 0.50, 0.75, 1)
    gt   <- outer(x$rpmxdep, ints, '>')
    rpgt <- colSums(gt, na.rm = T)
    names(rpgt) <- paste0('rpgt', c('05', '10', '20', '50', '75', '100'))
    
    rpgtx <- matrix(x$rpmxdep, ncol = 3, nrow = nrow(x), 
                    dimnames = list(NULL, c('rpgt05x', 'rpgt10x', 'rpgt20x')))
    is.na(rpgtx) <- !gt[, 1:3]
    rpgtx <- colMeans(rpgtx, na.rm = T)
    
    ans <- c(rpxlen  = mean(x$poolen, na.rm = T),
             rpvlen  = sd(x$poolen, na.rm = T),
             rpmxlen = max(x$poolen, na.rm = T),
             totplen = sum(x$poolen, na.rm = T),
             rpxarea = mean(x$poolar, na.rm = T),
             rpvarea = sd(x$poolar, na.rm = T),
             rpmxar  = max(x$poolar, na.rm = T),
             areasum = sum(x$poolar, na.rm = T))
    rp100 <- ans['areasum'] * 100 / unique(x$reachlen)
    ans   <- c(ans, rp100 = unname(rp100), rpgt, rpgtx)
    return(ans)
  }
  
  x <- data.frame(uid, pool, depth, length, area, reachlen)
  pool.summaries <- ddply(x, .(uid, pool), calcPoolSummaries, .progress = plyrProgress())
  pool.summaries <- merge(pool.summaries, unique(x[,c('uid', 'reachlen')]), by = 'uid')
  site.summaries <- ddply(pool.summaries, .(uid), calcUidSummaries, .progress = plyrProgress())
  
  site.pool.summaries <- ddply(x, .(uid), calcSitePoolSummaries, .progress = plyrProgress())

  ans <- merge(site.summaries, site.pool.summaries, by = 'uid')
  progressReport('Finished calculating residual pool metrics.')
  return(ans)
}

plotPools <- function(x, ...){
  stopifnot(x$slope > 0)
    pool <- calculatePool(x$distance, x$depth, x$slope)
  pool$xcoord <- x$distance
  pool$water.depth <- x$depth
  pool$elev <- x$elevation
  #pool <- pool[pool$is.pool,]
  ylim <- c(min(x$elevation), max(x$slope * x$distance))
  plot(x$distance, x$elevation, col = c('black', 'red')[pool$is.pool + 1], ylim = ylim, pch = 3, ...)
  abline(0, x$slope)
  segments(x$distance, x$elevation, x$distance, x$elevation + pool$depth, lwd = 2, col = 'red')
  segments(x$distance, x$elevation, x$distance, x$elevations + pool$water.depth, lwd = 1, col = 'black', lty = 2)
  
  rect(pool$xcoord - pool$length, pool$elev, pool$xcoord, pool$elev + pool$depth, lty = 3)
  invisible(pool)
}

plotPools.thalweg <- function(x, ...){
  stopifnot(x$slope > 0)
  pool <- nrsa:::calculatePool(x$distance, x$depth, x$slope)
  pool$xcoord <- x$distance
  pool$water.depth <- x$depth
  pool$elev <- x$elevation
  #pool <- pool[pool$is.pool,]
  ylim <- c(min(x$elevation), max(x$slope * x$distance))
  plot(x$distance, x$elevation, col = c('black', 'red')[pool$is.pool + 1], ylim = ylim, pch = 3, ...)
  abline(0, x$slope)
  segments(x$distance, x$elevation, x$distance, x$elevation + pool$depth, lwd = 2, col = 'red')
  segments(x$distance, x$elevation, x$distance, x$elevation + pool$water.depth, lwd = 1, col = 'black', lty = 2)
  
  rect(pool$xcoord - pool$length, pool$elev, pool$xcoord, pool$elev + pool$depth, lty = 3)
  invisible(pool)
}

thalweg.profile <- function(distance, depth, slope){
  o <- order(distance)
  ret <- data.frame(distance = distance,
                    depth    = depth,
                    slope    = slope,
                    elevation = elev(distance, depth, slope))
  ret <- as.list(ret[o,])
  ret$stepfun <- stepfun(ret$distance[-1], ret$elevation)
  class(ret) <- 'thalweg'
  ret
}

elev <- function(d, y, s){
  (s * d) - y
}

inv.lower.tri <- function(x){
  d <- (1 + sqrt(8 * length(x) + 1))/2
  ret <- matrix(NA, d, d)
  ret[lower.tri(ret)] <- x
  diag(ret)           <- 0
  ret
}

`start.rle` <- function (x){
  pl <- cumsum(c(1, x$lengths))
  pl[-length(pl)]
}

`end.rle` <- function(x){
  cumsum(x$lengths)
}

`rle.id` <- function(x){
  rep(seq_along(x$lengths), x$lengths)
}

test.profile <- function(){
  x <- 1:5
  x <- c(x, rev(x))
  x <- rep_len(x, 50)
  s <- runif(50, 0, 0.1)
  thalweg.profile(1:50, x, s)
}

siteOrder <- function(transect, station){
  
}