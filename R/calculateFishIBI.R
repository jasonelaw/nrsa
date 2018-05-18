# This list contains ranges for raw metric scores that represent 0 and 10 subindex scores, respectively.
# Some raw metrics have different ranges for first order and larger streams.
kSubindexRange <- list(n.native.species         = list(first = c(0,5), gt.first = c(0,11)), # first value is first order stream, 
                       n.native.families        = list(first = c(0,4), gt.first = c(0,7)),  # second is > 1st order
                       n.native.benthic.species = list(first = c(0,3), gt.first = c(0,7)),
                       n.native.watercolumn     = list(first = c(0,2), gt.first = c(0,4)),
                       n.sensitive              = list(first = c(0,2), gt.first = c(0,5)),
                       n.hider                  = c(0,   4),
                       n.nlns                   = c(0,   3),
                       percent.tolerant         = c(0.1, 0),
                       percent.filter.feeding   = c(0,   0.1),
                       percent.top.carnivore    = c(0,   0.1),
                       percent.omnivorous       = c(0.1, 0),
                       percent.lunker           = c(0,   1),
                       percent.anomaly          = c(0.02, 0))

#' Calculates Fish IBI
#' 
#' Calculates a fish IBI based on Hughes et al "A process for developing and evaluating indices
#' of fish assemblage integrity"
#' 
#' The \code{fishTraits} (unexported, use nrsa:::fishTraits to view code) function returns a \code{data.frame} with trait data for the IBI. 
#' 
#' @param site vector of site-visit identifiers
#' @param order a numeric vector; stream order correspoding to the location
#' @param species a vector of taxa names; must correspond to name field returned by \link{fishTraits}
#' @param length a vector of fish lengths in centimeters
#' @param anomaly a logical vector, \code{TRUE} if an anomaly was observed on the fish
#' @param join.traits.by which field from the data.frame returned by \link{fishTraits} should be used to match the species
#' @param return.subindices logical, should the subindices be returned or just the IBI?
#' @param return.raw.metrics should raw metrics be returned rather than IBI values?
#' @import plyr
#' @export
calculateFishIBI <- function(site, order, species, length, anomaly, join.traits.by = 'common_name', 
                             return.subindices = F, return.raw.metrics = F, ODFW.version = F){
  metric.names <- names(kSubindexRange)
  not.in.traits <- setdiff(species, traits[[join.traits.by]])
  if(length(not.in.traits) > 0){
    warning('There are species that do not match species names in the trait file:', toString(not.in.traits),
            '. Metrics will not be accurate!')
  }
  x <- data.frame(site, order, species, length, anomaly)
  x <- rename(x, structure(join.traits.by, .Names = 'species'))
  x <- merge(x, traits, by = join.traits.by)
  
  # Metric calc
  ret <- plyr::ddply(x, c('site', 'order'), summarize,
               #n.target                 = sum(!is.na(lunker)),
               n.native.species         = nunique(species[native]),
               n.native.families        = nunique(family[native]),
               n.native.benthic.species = nunique(species[native.ben]),
               n.native.watercolumn     = nunique(species[native.wc]),
               n.hider                  = nunique(species[hider]),
               n.sensitive              = nunique(species[sensitive]),
               n.nlns                   = nunique(species[nlns]),
               percent.tolerant         = mean(tolerant),
               percent.filter.feeding   = mean(filter.feed),
               percent.top.carnivore    = mean(top.carniv),
               percent.omnivorous       = mean(omnivore),
               percent.lunker           = nunique(species[length > lunker]) / 7L,#mean(length > lunker, na.rm = T),#sum(length > lunker, na.rm = T) / n.target,
               percent.anomaly          = mean(anomaly))
  # Fix divide by 0
  ret$percent.lunker[ret$n.target == 0] <- 0
  
  if(ODFW.version){
    metric.names <- metric.names[-match("percent.top.carnivore", metric.names)]
  }
  
  if(!return.raw.metrics){
    # Calculate IBI: rescale, then calculate mean of subindices and multiply by 10 to rescale to 0-100
    # Original calcs sum subindices, multiply by 10 / 1.3. This is just the mean(subindices) rescaled to 0-100.
    ret <- rescaleMetrics(ret)
    ret$fish.ibi <- rowMeans(ret[, metric.names]) * 10 
  }
  
  if(return.subindices || return.raw.metrics){
    return(ret)
  } else {
    return(ret[,c('site', 'fish.ibi')])
  }
}

#' Map fish IBI raw metric scores to rescaled subindex scores
#' 
#' Maps raw metric scores to subindex scores for the Fish IBI using linear
#' interpolation. The subindex scores must be on a 0 to 10 scale. Raw metric scores
#' may be counts or percentages. The interpolation function used depends on the stream order of the site.
#' @param x a dataframe of raw metric scores
#' @return a dataframe identical to input, except that metrics with names matching \code{nrsa:::kMetrics}
#' are rescaled to 0 - 10.
rescaleMetrics <- function(x){
  metric.names <- names(kSubindexRange)
  stopifnot(metric.names %in% names(x), 'order' %in% names(x), !any(is.na(x$order)))
  Fs <- rapply(kSubindexRange, function(x) approxfun(x, c(0,10), rule = 2), how = 'list')
  FUN <- function(m, f, order) {
    if(is.function(f)){
      f(m)
    } else {
      ifelse(order > 1, f$gt.first(m), f$first(m))
    }
  }
  x[,metric.names] <- mapply(FUN, x[,metric.names], Fs, MoreArgs = list(order = x$order))
  x
}
