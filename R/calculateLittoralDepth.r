#'Calculate non-wadeable littoral depth metrics
#'
#'\code{calculateLittoralDepth} calculates the non-wadeable
#'littoral depth metrics.  There are two different implementations that vary
#'primarily in speed.  \code{calculateLittoralDepth} is faster although the
#'implementation is somewhat more obtuse.  \code{calculateLittoralDepth2} uses
#'\link{ddply} and is fairly simple.
#'@param uid a vector of site-visit indicators
#'@param depth a vector of depth measurements (with the same units)
#'@importFrom Rigroup igroupMeans igroupMins igroupMaxs igroupAlls
#'@importFrom plyr ddply
#'@export
#'@examples
#'calculateLittoralDepth(uid = rep(1:10, each = 10), depth = rnorm(100))
#'calculateLittoralDepth(uid = rep(1, 5), depth = rep(NA, 5))
calculateLittoralDepth <- function(uid, depth){
  uid <- as.factor(uid)
  allna <- igroupAlls(is.na(depth), uid)
  x  <- igroupMeans(depth, uid, na.rm = T)
  mn <- igroupMins(depth, uid, na.rm = T)
  mx <- igroupMaxs(depth, uid, na.rm = T)
  std <- tapply(depth, uid, sd, na.rm = T)
  ans <- cbind(x, mn, mx, std)
  dimnames(ans) <- list(uid    = levels(uid), 
                        metric = c('xlit', 'mnlit', 'mxlit', 'vlit'))
  nas <- matrix(allna, nrow(ans), ncol(ans))
  is.na(ans) <- nas
  progressReport('Finished non-wadeable littoral depth metrics')
  ans
}

#'@rdname calculateLittoralDepth
#'@export
calculateLittoralDepth2 <- function(uid, depth){
  x <- data.frame(uid = as.factor(uid),
                  depth = depth)
  f <- function(x){
    allna <- all(is.na(x$depth))
    if(!allna){
      return(c(xlit  = mean(x$depth, na.rm = T),
               mnlit = min(x$depth, na.rm = T),
               mxlit = max(x$depth, na.rm = T),
               vlit  = sd(x$depth, na.rm = T)))
    } else {
      return(c(xlit = NA, mnlit = NA, mxlit = NA, vlit = NA))
    }
  }
  ans <- ddply(x, .(uid), f)
  progressReport('Finished non-wadeable littoral depth metrics')
  return(ans)
}