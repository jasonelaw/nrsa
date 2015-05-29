kChannelHabitatCodes         <- c('FA','CA','RA','RI','GL','PB', 'PP','PD','PL','PT','P','DR', 'PO')
kFastChannelHabitatCodes     <- c('FA', 'CA', 'RA', 'RI')
kSlowChannelHabitatCodes     <- c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'GL', 'PO')
kPoolChannelHabitatCodes     <- c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'PO')
kWadeOnlyChannelHabitatCodes <- c('PB', 'PP', 'PD', 'PL', 'PT', 'P')
kBoatOnlyChannelHabitatCodes <- c('PO')

#'Calculate channel habitat metrics
#'
#'\code{calculateChannelHabitatProportions} is used to calculate the channel
#'habitat metrics
#'@param uid a vector of site-visit indicators
#'@param habitat a vector of channel habitat codes
#'@param is.wadeable a logical vector with \code{TRUE} for wadeable sites
#'@export
calculateChannelHabitatProportions <- function(uid, habitat, is.wadeable){
  #compute individual metrics
  habitat <- factor(as.character(habitat), levels = kChannelHabitatCodes)
  chan.hab.counts <- table(uid = uid, metric = habitat)
  chan.hab.pcts <- prop.table(chan.hab.counts, 1) * 100
  
  #compute summed metrics
  pct_fast <- rowSums(chan.hab.pcts[, kFastChannelHabitatCodes, drop = F])
  pct_slow <- rowSums(chan.hab.pcts[, kSlowChannelHabitatCodes, drop = F])
  pct_pool <- rowSums(chan.hab.pcts[, kPoolChannelHabitatCodes, drop = F])
  composite.mets <- rbindMetrics(convertNamedVectorToMetricDF(pct_fast),
                                 convertNamedVectorToMetricDF(pct_slow),
                                 convertNamedVectorToMetricDF(pct_pool))
  
  # remove metrics that are specific to either wadeable or boatable.
  wade.uids <- unique(uid[is.wadeable])
  boat.uids <- unique(uid[!is.wadeable])
  chan.hab.pcts <- as.data.frame(chan.hab.pcts, responseName = 'result')
  chan.hab.pcts <- allFacToChar(chan.hab.pcts)
  i <- !((chan.hab.pcts$metric %in% kBoatOnlyChannelHabitatCodes & chan.hab.pcts$uid %in% wade.uids) |
         (chan.hab.pcts$metric %in% kWadeOnlyChannelHabitatCodes & chan.hab.pcts$uid %in% boat.uids))
  chan.hab.pcts <- chan.hab.pcts[i,]
  # remove 'PO' sum because it is redudant with pct_pool
  chan.hab.pcts <- chan.hab.pcts[chan.hab.pcts$metric != 'PO',]
  chan.hab.pcts$metric <- paste('pct_', tolower(chan.hab.pcts$metric), sep = '')
  mets <- rbindMetrics(chan.hab.pcts, composite.mets)
  progressReport('Finished with channel habitat metrics.')
  return(mets)
}

calculateChannelHabitatProportions2 <- function(uid, habitat, is.wadeable){
  #compute individual metrics
  habitat <- factor(as.character(habitat), levels = kChannelHabitatCodes)
  data.frame(uid, habitat, is.wadeable) %>%
    group_by(uid, habitat) %>%
    tally()
  chan.hab.counts <- table(uid = uid, metric = habitat)
  chan.hab.pcts <- prop.table(chan.hab.counts, 1) * 100
  
  #compute summed metrics
  pct_fast <- rowSums(chan.hab.pcts[, kFastChannelHabitatCodes, drop = F])
  pct_slow <- rowSums(chan.hab.pcts[, kSlowChannelHabitatCodes, drop = F])
  pct_pool <- rowSums(chan.hab.pcts[, kPoolChannelHabitatCodes, drop = F])
  composite.mets <- convertVectorsToMetricDF(pct_fast = pct_fast,
                                             pct_slow = pct_slow,
                                             pct_pool = pct_pool)
  
  # remove metrics that are specific to either wadeable or boatable.
  wade.uids <- unique(uid[is.wadeable])
  boat.uids <- unique(uid[!is.wadeable])
  chan.hab.pcts <- as.data.frame(chan.hab.pcts, responseName = 'result')
  chan.hab.pcts <- allFacToChar(chan.hab.pcts)
  i <- !((chan.hab.pcts$metric %in% kBoatOnlyChannelHabitatCodes & chan.hab.pcts$uid %in% wade.uids) |
         (chan.hab.pcts$metric %in% kWadeOnlyChannelHabitatCodes & chan.hab.pcts$uid %in% boat.uids))
  chan.hab.pcts <- chan.hab.pcts[i,]
  # remove 'PO' sum because it is redudant with pct_pool
  chan.hab.pcts <- chan.hab.pcts[chan.hab.pcts$metric != 'PO',]
  chan.hab.pcts$metric <- paste('pct_', tolower(chan.hab.pcts$metric), sep = '')
  mets <- rbindMetrics(chan.hab.pcts, composite.mets)
  progressReport('Finished with channel habitat metrics.')
  return(mets)
}