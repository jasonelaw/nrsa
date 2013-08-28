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
  all.codes <- c('FA','CA','RA','RI','GL','PB', 'PP','PD','PL','PT','P','DR', 'PO')
  habitat <- factor(as.character(habitat), levels = all.codes)
  chan.hab.counts <- table(uid = uid, metric = habitat)
  chan.hab.pcts <- prop.table(chan.hab.counts, 1) * 100
  
  #compute summed metrics
  pct.fast <- rowSums(chan.hab.pcts[, c('FA', 'CA', 'RA', 'RI'), drop = F])
  pct.slow <- rowSums(chan.hab.pcts[, c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'GL', 'PO'), drop = F])
  pct.pool <- rowSums(chan.hab.pcts[, c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'PO'), drop = F])
  composite.mets <- 
    data.frame(uid    = names(pct.fast),
               metric = rep(c('pct_fast', 'pct_slow', 'pct_pool'), each = length(pct.fast)),
               result = c(pct.fast, pct.slow, pct.pool))
  
  # remove metrics that are specific to either wadeable or boatable.
  wade.uids <- unique(uid[is.wadeable])
  boat.uids <- unique(uid[!is.wadeable])
  wade.only <- c('PB', 'PP', 'PD', 'PL', 'PT', 'P')
  boat.only <- 'PO'
  chan.hab.pcts <- as.data.frame(chan.hab.pcts, responseName = 'result')
  chan.hab.pcts <- allFacToChar(chan.hab.pcts)
  chan.hab.pcts <- subset(chan.hab.pcts, 
                          subset = !((metric %in% boat.only & uid %in% wade.uids) |
                                     (metric %in% wade.only & uid %in% boat.uids)))
  # remove 'PO' sum because it is redudant with pct_pool
  chan.hab.pcts <- subset(chan.hab.pcts, metric != 'PO')
  chan.hab.pcts$metric <- paste('pct_', tolower(chan.hab.pcts$metric), sep = '')
  mets <- rbindMetrics(chan.hab.pcts, composite.mets)
  return(mets)
}

