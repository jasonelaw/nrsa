#' Calculates number of side channel transects: 'sidecnt'
#' 
#' Calculates the number of side channel transects for wadeable sites only.  These are transects
#' that are marked with an 'X' prefix in the database.  The metric name is 'sidecnt'.
#' The inputs are just the vector of site identifiers and transect identifiers for
#' each site.
#' @param uid a vector of site identifiers
#' @param transect the unique vector of transects for each site
#' @return a 'metric' data.frame
#' @export
#' @examples
#' d <- expand.grid(uid = 1:10, transect = c(LETTERS, 'XA'))
#' calculateSideChannelCount(d$uid, d$transect)
calculateSideChannelCount <- function(uid, transect){
  kXtraTransects  <- paste0('X', LETTERS[1:11])
  is.side.channel <- transect %in% kXtraTransects
  sidecnt <- tapply(is.side.channel, uid, sum)
  sidecnt <-convertNamedVectorToMetricDF(sidecnt)
  progressReport("Finished side channel count: sidecnt.")
  return(sidecnt)
}

#' Proportion of side of reach with side channels.
#' 
#' Calculates the proportion of the reach that has side channels.  The input data
#' is 'SIDCHN' for wadeable and OFF_CHAN for boatable sites.
#' @param uid a vector of site identifiers
#' @param transect a vector of transect identifiers
#' @param is.sidechannel a vector containing the 'SIDCHN' and 'OFF_CHAN' data, 
#' either \code{'Y'} or \code{'N'} or \code{NA}.
#' @return a 'metric' data.frame
#' @export
calculatePercentSideChannel <- function(uid, transect, is.sidechannel){
  kXtraTransects        <- paste0("X", LETTERS[1:11])
  kSideChannelIndicator <- "Y"
  
  is.sidechannel <- is.sidechannel == kSideChannelIndicator
  i <- transect %in% kXtraTransect
  pct_side <- tapply(is.sidechannel[i], uid[i], mean, na.rm = T) * 100
  pct_side <- convertNamedVectorToMetricDF(pct_side)
  progressReport('Finished percent side channel: pct_side.')
  return(pct_side)
}

#' Calculates the reach length for a boatable site
#' 
#' Calculates the reach length for a boatable site based on the actual transect
#' spacing recorded on the transect form.  There should be one input value for each
#' transect at the site.
#' @param uid a vector of site identifiers
#' @param actransp the actual transect spacing
#' @export
#' @return a 'metric' data.frame
calculateBoatableReachLength <- function(uid, actransp){
  actransp <- ifelse(actransp <= 0, NA, actransp)
  reachlen <- tapply(actransp, uid, sum, na.rm = T)
  ans <- convertNamedVectorToMetricDF(reachlen)
  progressReport("Finished boatable reach length: reachlen.")
  return(ans)
}

#' Calculates the reach length for a wadeable site
#' 
#' Calculates the reach length for a wadeable site using the number of stations per transect
#' and the increment between stations.  The number of stations per transect
#' must be calculated using \link{nWadeableStationsPerTransect} prior to passing
#' to this function.  There is one unique increment per site and as many n.station
#' as there are transects at a site.
#' @param uid a vector of site identifiers
#' @param n.station the number of station per transect
#' @param increment the distance between stations at a wadeable site
#' @return a 'metric' data.frame
#' @export
#' @examples
#' increment <- expand.grid(uid = 1:10, increment = 1.5)
#' d <- expand.grid(uid = 1:10, transects = LETTERS[1:11], station = 0:9)
#' n.sta <- nWadeableStations(d$uid, d$transects, d$station, by = 'site')
#' d <- merge(increment, n.sta, by = 'uid')
#' calculateWadeableReachLength(d$uid, d$n.sta, d$increment)
calculateWadeableReachLength <- function(uid, n.station, increment){
  n.station <- tapply(n.station, uid, sum) # total stations at site
  increment <- tapply(increment, uid, unique)
  # There are 1 fewer increments than number of stations, so we must substract one
  reachlen  <- n.station * increment - increment
  ans <- convertNamedVectorToMetricDF(reachlen)
  progressReport("Finished boatable reach length: reachlen.")
  return(ans)
}

#' Determines whether a site was sampled from x site validation data
#' 
#' Function returns the 'sampled' metric based on the data in the from the stream
#' verification form (field VALXSITE from the VERIFICATION table).
#' @param uid a vector of site identifiers
#' @param valxsite a vector of sampling methods.  The following ones indicate the
#' site was sampled: BOATABLE, PARBYBOAT, ALTERED, INTWADE, PARBYWADE, WADEABLE
#' @return a 'metric' data.frame
#' @export
isSiteSampled <- function(uid, valxsite){
  kSampledCategories <- c('BOATABLE','PARBYBOAT','ALTERED',
                          'INTWADE','PARBYWADE','WADEABLE')
  sampled <- ifelse(valxsite %in% kSampledCategories,'Y','N')
  ans <- data.frame(uid = uid, metric = 'sampled', result = sampled)
  return(ans)
}
