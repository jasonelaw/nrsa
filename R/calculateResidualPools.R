#' Residual depths from a thalweg profile
#' 
#' Calculate residual pool depths from a thalweg profile containing distance 
#' depth and slope, treating every depth as a possible base depth.
#' 
#' The resulting residual depths are returned in the lower triangle of a n * n 
#' matrix, where n is the number of depths.  The diagonal of the matrix
#' represents the succesive base points. Each row after the diagonal gives the
#' residual depths relative to the base point for that column.  Any row with a
#' residual depth > 0 is in a pool represents a thalweg point that is in a pool.
#' @param distance a vector of distances along a thalweg profile
#' @param depth a vector of thalweg profile depth measurements
#' @param slope a site wide average slope of length 1

#' @return a n * n lower diagonal matrix containing the residual depths.  Each 
#'   column represents the residual depths calculated treating a different
#'   thalweg point as the base depth.
#' @examples
#' allResidualDepth(0:9, runif(10), .01)
calculatePoolDimensions <- function(distance, depth, slope){
  stopifnot(identical(length(distance), length(depth)), !is.na(slope))
  # Set up the data
  ld <- length(depth)
  depth.mat  <- matrix(depth, ld, ld)
  base.depth <- t(depth.mat)
  dist.mat   <- matrix(distance, ld, ld)
  dist.mat   <- sweep(dist.mat, 2, distance)
  # Here is the actual computation...
  residual.depth <- depth.mat - (base.depth + (dist.mat * slope))
  # set values before the base point to missing.
  residual.depth[upper.tri(residual.depth)] <- NA 
  dist.mat[upper.tri(dist.mat)] <- NA
  return(abind::abind(depth = residual.depth, length = dist.mat, along = 3))
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
  is.pool <- dimensions[, , 'depth', drop = F] > 0
  is.pool <- rowSums(is.pool, na.rm = T) > 0
  
  pools   <- rle(is.pool)
  lp      <- length(pools$values)
  pool.pt <- sequence(pools$lengths)
  
  base.pt <- pool.pt == 1
  base.pt[!is.pool] <- F
  base.pt <- 1:length(is.pool) %in% (which(base.pt) - 1)
  
  pool.id <- rep(seq_len(lp),
                 times = pools$lengths)
  
  is.na(pool.pt) <- is.na(pool.id) <- !is.pool
  
  res.dim <- extractResidualPoolDimensions(dimensions, base.pt, is.pool)
  
  data.frame(is.pool, pool.id, pool.pt, base.pt, res.dim)
}

#' Cuts a sequence of dimensions to only those dimensions that represent pools.
#' 
#' The \link{calculatePoolDimensions} function calculates pool dimensions (depth
#' and length) for every possible base point and each thalweg point after the 
#' base point. For base points that actually define pools, the pool ends when 
#' the residual depth becomes negative (or the site ends).  This function trims 
#' a sequence of pool dimensions so that only the points in the sequence that 
#' represent pools are returned.  In addition, the length is calculated as the
#' difference between thalweg sampling points (i.e., only the length
#' attributable to that point, rather than the cumulative length of the pool up
#' that point).
#' @param x a n * 2 matrix with column 1 being the residual depth and column 2 
#'   the residual length
#' @param na.omit should NAs be trimmed from the sequence.  This is to remove 
#'   the NAs from the diagonal and upper triangle pool dimension array returned 
#'   by \link{calculatePoolDimensions}.  These represent thalweg points that 
#'   come before the base point and are NA by definition.
cutDimensions <- function(x, na.omit = T){
  n <- nrow(x)
  xseq      <- seq_len(n)
  cut.point <- pmin(which(x[,'depth'] < 0)[1], n + 1, na.rm = T)
  if(na.omit){
    nas <- is.na(x[,'depth'])
  } else {
    nas <- F
  }
  i   <- xseq < cut.point & !nas
  ans <- x[i, ]
  # the -1 excludes the base point which is included in the dimension matrix so 
  # that the diff of length can be calculated to from cumulative length to length
  # at that point.
  cbind(depth  = ans[-1, 'depth'], 
        length = diff(ans[, 'length']))
}

#' Extracts the dimensions of residual pools from the array of dimensions of
#' possible pools
#' 
#' The \link{calculatePoolDimensions} function calculates the dimensions of
#' every possible pool while treating each successive depth as a potential base
#' point. This function extracts those dimensions that actually represent pools
#' from the array. It returns a 3 * n matrix of residual length, depth and area.
#' @param dimensions an array returned by \link{calculatePoolDimensions}
#' @param base.point a vector as long as the thalweg profile with the base point
#'   for each pool
#' @param is.pool logical vector, \code{TRUE} if the depth represents a pool.
#' @import plyr
extractResidualPoolDimensions <- function(dimensions, base.point, is.pool){
  # extract the base points which actually define pools from columns of all
  # potential base points
  dimensions <- dimensions[, base.point, , drop = F]
  # cut the matrix of residual depths (column 1) and residual lengths (column 2)
  # for each base point to only those depths that represent a pool.
  profiles   <- alply(dimensions, 2, cutDimensions, na.omit = T)
  ans <- matrix(NA, nrow = length(is.pool), ncol = 2,
                dimnames = list(NULL, c('depth', 'length')))
  ans[is.pool, ] <- do.call('rbind', profiles)
  area <- ans[, 'depth'] * ans[, 'length']
  ans  <- cbind(ans, area = area)
  return(ans)
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
#' @import plyr
#' @export
calculatePool <- function(distance, depth, slope, uid = NULL, convert.stack = F) {
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
  progressReport("Residual pool dimensions are finished.")
  return(pool.info[pool.info$is.pool, ])
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
    depmet.names <- c('0%' = 'mindep', '25%' = 'dep25', '50%' = 'meddep', 
                      '75%' = 'dep75', '100%' = 'rpmxdep')
    depmet <- quantile(x$depth, probs = seq(0, 1, 0.25), type = 2, na.rm = T)
    depmet <- rename(depmet, depmet.names)
    ans <- c(poolen = sum(x$length, na.rm = T),
             poolar = sum(x$area, na.rm = T),
             xdep   = mean(x$depth, na.rm = T),
             rpvdep = sd(x$depth, na.rm = T))
    ans <- c(ans, depmet)
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
  n.present  <- tapply(loc, uid, count)
  prop.sampled <- (n.present - n.missing) / n.expected
  incomplete   <- prop.sampled < minimum.proportion
  incomplete[uid]
}

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

#' @import plyr
createNickPoints <- function(uid, loc, depth, increment, slope, is.wadeable, ...){
  i <- loc == 1
  if(!all(unique(uid[i]) %in% unique(uid))){
    warning('There are some uids without a loc at 1.  No nick point will be generated for these uids!')
  }
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

#' Organize thalweg data for residual pool analysis
#' 
#' This function adds a sequence indicator to a thalweg profile, creates
#' additional 'nick points', and removes incomplete sites.
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
  x <- merge(x, site.seq, by = c('uid', 'transect', 'station'), all.x = T)
  x <- x[order(x$uid, x$loc), ]
  nicks <- createNickPoints(x$uid, x$loc, x$depth, x$increment, x$slope, x$is.wadeable)
  x <- rbind.fill(x, nicks)
  i <- identifyIncompleteSites(x$uid, x$loc, x$depth, minimum.proportion = minimum.proportion)
  message('Removing incomplete sites:', toString(sQuote(unique(x$uid[i]))))
  x <- subset(x, !i, select = c(uid, transect, station, loc, depth, increment, slope, is.wadeable))
  arrange(x, uid, loc)
}