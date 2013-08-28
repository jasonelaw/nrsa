#' Estimates the intended number of wadeable thalweg stations to be visited at
#' an NRSA study site.
#' 
#' Estimates the intended number of wadeable thalweg stations at each transect
#' which are considered sampled (even if there is no data) for the purposes of
#' calculating residual pools and channel lengths.  The number of stations at
#' a transect is calculated as the greater of either the number of stations
#' occuring in the dataframe for that transect, or the most common count of
#' stations (i.e. station mode) occuring at that site.
#' 
#' It takes advantage of the fact that stations
#' are numeric, and thus that the last station to be expected for a transect
#' is the number of transects to be expected at that transect, adding 1 to
#' the station to account for station numbering starting at 0.  Transect K
#' always has 1 station.
#' 
#' The by option allows you to calculate the number of stations by site or transect.
#' Doing it by site makes the calculations of reach length (in 
#' \link{calculateWadeableReachLength}) easier.
#' 
#' @param uid a vector of site identifiers
#' @param transect a vector of transect identifiers
#' @param station a vector of station identifiers
#' @param by 
#' @importFrom plyr ddply
#' @importFrom NARSShared modalvalue modalCount
#' @export
#' @examples
#' 
#' d <- expand.grid(uid = 1:10, transects = LETTERS[1:11], station = 0:9)
#' nWadeableStations(d$uid, d$transects, d$station, by = 'site')
#' nWadeableStationsPerTransect(d$uid, d$transects, d$station)
nWadeableStations <- function(uid, transect, station, by = c('transect', 'site')){
  by <- match.arg(by)
  x <- data.frame(uid, transect, station)
  f <- function(x){
    st.last     <- tapply(x$station, x$transect, max)
    st.mode     <- modalvalue(st.last)
    st.mode.cnt <- modalCount(st.last)
    if (st.mode.cnt > 6){
      n.sta <- ifelse(st.last < st.mode, st.mode, st.last)
    } else {
      n.sta <- st.last
    }
    if ('K' %in% names(n.sta)){
      n.sta['K'] <- 0
    }
    ans <- data.frame(transect = names(n.sta), n.station = n.sta + 1)
  }
  ans <- ddply(x, .(uid), f)
  if (by == 'site'){
    ans <- ddply(ans, .(uid), summarize, n.station = sum(n.station))
  }
  return(ans)
}

#' @export
#' @rdname nWadeableStations
nWadeableStationsPerTransect <- function(uid, transect, station){
  nWadeableStations(uid, transect, station, by = 'transect')
}
