#' Calculate slope and bearing metrics
#' 
#' Functions for calculating slope and bearing metrics from NRSA protocol channel
#' geometry data.  The metrics calculated are: xbearing, sinu, xslope, vslope, nslope.
#' 
#' The general strategy for calculationg slope and bearing metrics using these functions is:
#' \enumerate{
#'    \item Calculate transect spacing for wadeable as: \code{increment * nWadeableStationsPerTransect}
#'    \item Calculate wadeable distance for each slope/bearing as \code{transp * proportion}
#'    \item Calculate boatable transect spacing as \code{sum(distance)} and proportion as \code{distance / sum(distance)}
#'    \item Calculate grade (as percent) from boatable elevation change measurements in centimeters, if applicable.
#'    \item Calculate angle metrics using \code{calculateAngleMetrics}
#'    \item Calculate slope metrics using \code{calculateSlopeMetrics}
#'  }
#'  The overall working of \code{calculateAngleMetrics} differs somewhat than the
#'  EPA code.  EPA calculated total distances East and North at each transect,
#'  then summed to get the total distance East and North for the site.  \code{calculateAngleMetrics}
#'  treats the channel geometry as a single line string and calculates the length
#'  of the line string and the distance from the first to last point.  It calculates
#'  xbearing as the azimuth between the first and last points.
#'  
#' Function calculateTransectSpacing calculates the transect spacing. These should be the same for every transect. 
#' But for some sites, it's possible to get discrepancies between
#' transects because of numerical issues. In that case the transect spacing 
#' won't be exactly the same at every transect. For that reason,
#' we take the mean of the the transect spacing values for the site.
#'  
#'@param uid a vector of site identifiers
#'@param transect a vector of transect identifiers
#'@param azimuth a vector of bearings; more properly azimuths
#'@param distance a vector of distances
#'@param transpc a vector of transect spacing values
#'@export
#'@import plyr
#'@examples
#'  d <- data.frame(uid = c(1,1), azimuth = c(180,90), distance = c(1,1))
#'  calculateAngleMetrics(d$uid, d$azimuth, d$distance)
#'  d <- data.frame(uid = rep(1,6), transect = rep(1:2,each = 3),
#'                  slope = rep(c(10, 20, 30), 2), proportion = rep(1,6) / 3)
#'  with(d, calculateSlopeMetrics(uid, transect, slope, proportion))
calculateAngleMetrics <- function(uid, azimuth, distance){
  x <- data.frame(uid, azimuth, distance)
  f <- function(x){
    coords     <- coordinatesFromTraverse(x$azimuth, x$distance)
    n          <- nrow(coords)
    last.point <- coords[n, ]
    
    xbearing <- getAngleFromDisplacement(last.point[1], last.point[2])
    xbearing <- radToDeg(xbearing)
    
    line.length <- p2pDistance(coords[, 1], coords[, 2])
    crow.dist   <- p2pDistance(coords[c(1, n), 1], coords[c(1, n), 2])
    fish.dist   <- sum(line.length)
    sinu     <- fish.dist / crow.dist
    c(xbearing = unname(xbearing), sinu = sinu)
  }
  ans <- ddply(x, .(uid), f)
  ans <- meltMetrics(ans)
  progressReport("Angle metrics completed.")
  return(ans)
}

#' @rdname calculateAngleMetrics
#' @import plyr
#' @export
calculateSlopeMetrics <- function(uid, transect, slope, proportion){
  uid <- as.character(uid)
  ones <- tapply(proportion, list(uid, transect), sum) == 1
  ones <- apply(ones, 1, all)
  drop.uids <- names(ones)[ones]
  if (!all(ones)){
    warning("There are transects whose proportions don't sum to 1")
  }
  x <- data.frame(uid, transect, slope, proportion)
  low.n <- tapply(!is.na(x$slope), x$uid, sum) < 3
  low.n.uids <- F
  if (any(low.n)){
    warning('There are site with less than three slopes: no xslope or vslope reported')
  }
  f <- function(x){
    c(tmean = weighted.mean(x$slope, x$proportion, na.rm = T))
  }
  tmean <- ddply(x, .(uid, transect), f)
  f <- function(x){
    c(xslope = mean(x$tmean),
      vslope = sd(x$tmean),
      nslp   = count.notna(x$tmean))
  }
  ans <- ddply(tmean, .(uid), f)
  ans <- meltMetrics(ans)
  is.na(ans$result) <- ans$metric %in% c('xslope', 'vslope') & ans$uid %in% low.n.uids
  progressReport('Slope metrics completed.')
  return(ans)
}

#' @export
fillSlopeBearingData <- function(uid, transect, is.wadeable, increment, n.station, distance, proportion, slope_percent, slope_cm, azimuth, is.slope.length = F){
  transpc <- increment * n.station
  transpc[!is.wadeable]    <- ave(distance, uid, transect, FUN = sum)[!is.wadeable]
  distance[is.wadeable]    <- (transpc * proportion)[is.wadeable]
  proportion[!is.wadeable] <- (distance / transpc)[!is.wadeable]
  
  slope_percent[is.na(slope_percent)] <- calcGrade(slope_cm, distance * 100, unit = 'percent', is.slope.length = is.slope.length)[is.na(slope_percent)]
  
  return(data.frame(uid, transect, is.wadeable, transpc, distance, proportion, slope = slope_percent, azimuth))
}

#' @rdname calculateAngleMetrics
#' @import plyr
#' @export
calculateTransectSpacing <- function(uid, transpc){
  meltMetrics(ddply(data.frame(uid, transpc), .(uid), summarize, transpc = mean(transpc)))
}

#' Returns a vector of point to point distances for a line.
#' 
#' For a line which is represented by a two dimensional matrix with x and y 
#' coordinate columns, this function returns a vector of distances from each
#' point to the next point.  The vector is of length \code{nrow(x) - 1}.
#' @param x a two column matrix
#' @return a vector of distances
p2pDistance <- function(x, y){
  sqrt(diff(x)^2 + diff(y)^2)
}

#' Calculate the grade from rise and run
#' 
#' Calculate the grade from rise and run.  The grade can be returned as a proportion,
#' percent or angle in radians.  In addition, the run can be input as a slope length.
#' In this case, the actual run is first calculated using the pythagorean theorem
#' and then used in subsequent calculations.
#' @param rise a vector of slope rise values
#' @param run a vector of slope run values
#' @param unit what unit should the grade be returned in
#' @param is.slope.length logical, TRUE if the run vector is actually a slope length
#' @return a vector of grade values
#' @references
#' “Grade (slope).” Wikipedia, the Free Encyclopedia, September 2, 2013. 
#' \url{http://en.wikipedia.org/w/index.php?title=Grade_(slope)&oldid=571239608}.
calcGrade <- function(rise, run, unit = c('proportion', 'percent', 'angle'), is.slope.length = F){
  if(is.slope.length){
    run <- sqrt(run^2 - rise^2)
  }
  unit  <- match.arg(unit)
  slope <- rise / run
  slope <- switch(unit,
                  proportion = slope,
                  percent    = slope * 100,
                  angle      = atan(slope))
  return(slope)
}

#' Computes coordinates for a traverse
#' 
#' Computes the coordinates for a two dimensional traverse with an arbitrary start
#' point (assumed to be (0,0)).
#' @param azimuth a vector of azimuth angles in degrees
#' @param distance a vector of distances
#' @param start the coordinates of the starting point for the traverse as a length two vector
#' @return the coordinates for the traverse
#' @export
coordinatesFromTraverse <- function(azimuth, distance, start = c(0,0)){
  la <- length(azimuth)
  start <- matrix(start, ncol = 2, nrow = la + 1, byrow = TRUE)
  disp <- getDisplacementFromDistAzimuth(azimuth, distance, units = 'degrees') 
  coord <- apply(disp, 2, cumsum)
  start[2:(la + 1), ] <- start[2:(la + 1), ] + coord
  return(start)
}

#' Returns the angle from the y axis for a vector
#' 
#' Returns the angle from the y axis for a two dimenisonal vector (x,y).
#' @param x the x coordinate
#' @param y the y coordinate
#' @return the angle in radians for \code{getAngleFromDisplacement}; angles in the
#' appropriate units for \code{degToRad} and \code{radToDeg}; Displacement distances
#' for each azimuth distance pair for \code{getDisplacementFromDistAzimuth}.
#' @export
getAngleFromDisplacement <- function(x, y){
  angle <- atan2(x, y)
  if (angle < 0){
    return(angle + (2 * pi))
  } else {
    return(angle)
  }
}

#' @rdname getAngleFromDisplacement
degToRad <- function(x){
  return(x * pi / 180)
}

#' @rdname getAngleFromDisplacement
radToDeg <- function(x){
  return(x * 180 / pi)
}

#' @rdname getAngleFromDisplacement
getDisplacementFromDistAzimuth <- function(azimuth, distance, units = c('radian', 'degrees')){
  units <- match.arg(units)
  if(identical(units, 'degrees')){
    azimuth <- degToRad(azimuth)
  }
  x <- sin(azimuth) * distance
  y <- cos(azimuth) * distance
  cbind(x = x, y = y)
}

Traverse <- function(radius, azimuth, inclination, degrees = T){
  ret <- cbind(radius, azimuth, inclination)
  class(ret) <- 'Traverse'
  ret
}

coords <- function(x){
  UseMethod('coords')
}

coords.Traverse <- function(x, system = 'cartesian'){
  ret <- apply(.cartesian(x), 2, cumsum)
  Points(ret[,1], ret[,2], ret[,3])
}

Points <- function(x, y, z){
  ret <- cbind(x, y, z)
  class(ret) <- 'Points'
  ret
}

cartesian <- function(x){
  UseMethod('cartesian')
}

.cartesian <- function(r, a, i){
  x <- r * sin(i) * cos(a)
  y <- r * sin(i) * sin(a)
  z <- r * cos(i)
  cbind(x,y,z)
}

.spherical <- function(x, y, z){
  r <- sqrt(x^2 + y^2 + z^2)
  i <- acos(z / r)
  a <- atan2(y, x)
  cbind(r, a, i)
}
#obj1 <- Traverse(c(1,1, 1, 1), nrsa:::degToRad(c(0, 90, 180, 270)), nrsa:::degToRad(c(92, 93, 95, 90)))
#coords(obj1)
