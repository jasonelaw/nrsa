kMetrics <- c('n.native.species', 'n.native.families', 'n.native.benthic.species',
              'n.native.watercolumn', 'n.hider', 'n.sensitive', 'n.nlns', 'percent.tolerant', 
              'percent.filter.feeding', 'percent.top.carnivore', 'percent.omnivorous',
              'percent.lunker', 'percent.anomaly')

kSubindexRange <- list(n.native.species         = list(zero = c(0,0), ten = c(5,11)), # first value is first order stream, 
                       n.native.families        = list(zero = c(0,0), ten = c(4,7)),  # second is > 1st order
                       n.native.benthic.species = list(zero = c(0,0), ten = c(3,7)),
                       n.native.watercolumn     = list(zero = c(0,0), ten = c(2,4)),
                       n.sensitive              = list(zero = c(0,0), ten = c(2,5)),
                       n.hider                  = list(zero = 0,   ten = 4),
                       n.nlns                   = list(zero = 0,   ten = 3),
                       percent.tolerant         = list(zero = 0.1, ten = 0),
                       percent.filter.feeding   = list(zero = 0,   ten = 0.1),
                       percent.top.carnivore    = list(zero = 0,   ten = 0.1),
                       percent.omnivorous       = list(zero = 0.1, ten = 0),
                       percent.lunker           = list(zero = 0,   ten = 1),
                       percent.anomaly          = list(zero = 0.02, ten = 0))

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
  traits <- fishTraits()
  not.in.traits <- setdiff(species, traits[[join.traits.by]])
  if(length(not.in.traits) > 0){
    warning('There are species that do not match species names in the trait file:', toString(not.in.traits),
            '. Metrics will not be accurate!')
  }
  x <- data.frame(site, order, species, length, anomaly)
  x <- rename(x, structure(join.traits.by, .Names = 'species'))
  x <- merge(x, traits, by = join.traits.by)
  
  # Metric calc
  ret <- ddply(x, .(site, order), summarize,
               n.target                 = sum(target.lunker),
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
               percent.lunker           = mean(length > lunker, na.rm = T),#sum(length > lunker, na.rm = T) / n.target,
               percent.anomaly          = mean(anomaly))
  # Fix divide by 0
  ret$percent.lunker[ret$n.target == 0] <- 0
  
  if(ODFW.version){
    kMetrics <- kMetrics[-match("percent.top.carnivore", kMetrics)]
  }
  
  if(!return.raw.metrics){
    # Calculate IBI: rescale, then calculate mean of subindices and multiply by 10 to rescale to 0-100
    # Original calcs sum subindices, multiply by 10 / 1.3. This is just the mean(subindices) rescaled to 0-100.
    ret <- rescaleMetrics(ret)
    ret$fish.ibi <- rowMeans(ret[, kMetrics]) * 10 
  }
  
  if(return.subindices || return.raw.metrics){
    return(ret)
  } else {
    return(ret[,c('site', 'fish.ibi')])
  }
}

#' Create a function that maps raw metric scores to subindex scores
#' 
#' Create a function that maps raw metric scores to subindex scores for the Fish IBI using linear
#' interpolation. The subindex scores must be on a 0 to 10 scale. Raw metric scores
#' may be counts or percentages. The interpolation function used depends on the stream order of the site.
#' @param zero numeric vector. The raw metric score representing a 0 subindex score. Can be of length 2, in which case the
#' first value is for stream order 1 and the second is for larger stream orders.
#' @param ten numeric vector of length 1 or 2. It is the value of the raw metric score that represents a 10 subindex score.
#' Can be of length 2, in which case the
#' first value is for stream order 1 and the second is for larger stream orders.
#' 
#' Returns a function that accepts two arguments, raw metric score and stream order and
#' returns the subindex score for the mapping specified.
indexfun <- function(zero, ten){
  stopifnot(length(zero) < 3, length(ten) < 3, length(zero) == length(ten))
  if(length(zero) > 1){
    f1 <- approxfun(c(zero[1], ten[1]), c(0, 10), rule = 2)
    f2 <- approxfun(c(zero[2], ten[2]), c(0, 10), rule = 2)
    metricToSubindex <- function(x, order, ...){
      stopifnot(identical(length(x), length(order)))
      ifelse(order < 2, f1(x), f2(x))
    }
  } else {
    metricToSubindex <- approxfun(c(zero, ten), c(0, 10), rule = 2)
    formals(metricToSubindex) <- alist(v =, order = NULL)
  }
  return(metricToSubindex)
}

rescaleMetrics <- function(x){
  stopifnot(kMetrics %in% names(x), 'order' %in% names(x), !any(is.na(x$order)))
  index.fun   <- lapply(kSubindexRange, function(l, f) do.call(f, l), f = nrsa:::indexfun)
  x[,kMetrics] <- lapply(kMetrics, function(i) index.fun[[i]](x[[i]], x$order))
  x
}

#' @rdname calculateFishIBI
fishTraits <- function(){
  trait.list <- 
    list(family = c("Catostomidae", "Catostomidae", "Catostomidae", "Centrarchidae", "Centrarchidae", 
                    "Centrarchidae", "Centrarchidae","Centrarchidae", "Centrarchidae", "Centrarchidae",
                    "Centrarchidae", "Centrarchidae", "Centrarchidae", "Centrarchidae", "Centrarchidae", 
                    "Centrarchidae", "Clupeidae", "Cobitidae", "Cottidae", "Cottidae", "Cottidae", 
                    "Cottidae", "Cottidae", "Cottidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", 
                    "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", 
                    "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Fundulidae",
                    "Gasterosteidae", "Ictaluridae", "Ictaluridae", "Ictaluridae", "Percidae", "Percopsidae", 
                    "Petromyzontidae", "Petromyzontidae", "Petromyzontidae", "Petromyzontidae", "Poeciliidae", 
                    "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae", 
                    "Petromyzontidae"),
         species = c("Catostomus macrocheilus", "Catostomus spp.", "Catostomus platyrhynchus", "Lepomis cyanellus", 
                     "Lepomis gibbosus", "Lepomis gulosus", "Lepomis macrochirus", "Lepomis spp.", 
                     "Micropterus dolomieu", "Micropterus salmoides", "Micropterus spp.", "Pomoxis annularis", 
                     "Pomoxis nigromaculatus", "Pomoxis spp.", "Lepomis auritus", "Lepomis microlophus", 
                     "Alosa sapidissima", "Misgurnus anguillicaudatus", "Cottus asper", "Cottus gulosus", 
                     "Cottus perplexus", "Cottus rhotheus", "Cottus spp.", "Cottus beldingi", 
                     "Acrocheilus alutaceus", "Carassius auratus", "Cyprinus carpio", 
                     "Mylocheilus caurinus", "Pimephales promelas", "Ptychocheilus oregonensis", 
                     "Rhinichthys cataractae", "Rhinichthys osculus", "Rhinichthys spp.", 
                     "Richardsonius balteatus", "Ctenopharyngodon idella", "Oregonichthys crameri", 
                     "Rhinichthys falcatus", "Gila bicolor", "Fundulus diaphanus", 
                     "Gasterosteus aculeatus", "Ameiurus melas", "Ameiurus natalis", 
                     "Ameiurus nebulosus", "Perca flavescens", "Percopsis transmontana", 
                     "Lampetra richardsoni", "Lampetra spp.", "Lampetra tridentata", 
                     "Lampetra spp.", "Gambusia affinis", "Oncorhynchus clarki", "Oncorhynchus kisutch", 
                     "Oncorhynchus mykiss", "Oncorhynchus spp.", "Oncorhynchus tshawytscha", 
                     "Prosopium williamsoni", "Petromyzontidae"), 
         common_name = c("largescale sucker", "unidentified sucker", "mountain sucker", 
                         "green sunfish", "pumpkinseed", "warmouth", "bluegill", "sunfish", 
                         "smallmouth bass", "largemouth bass", "bass spp.", "white crappie", 
                         "black crappie", "crappie", "redbreast sunfish", "redear sunfish", 
                         "american shad", "weather loach", "prickly sculpin", "riffle sculpin", 
                         "reticulate sculpin", "torrent sculpin", "unidentified sculpin", 
                         "paiute sculpin", "chiselmouth", "goldfish", "common carp", "peamouth", 
                         "fathead minnow", "northern pikeminnow", "longnose dace", "speckled dace", 
                         "dace spp.", "redside shiner", "grass carp", "oregon chub", "leopard dace", 
                         "tui chub", "banded killifish", "three-spined stickleback", "black bullhead", 
                         "yellow bullhead", "brown bullhead", "yellow perch", "sand roller", 
                         "western brook lamprey", "unidentified lamprey", "pacific lamprey", 
                         "lamprey ammocoete", "mosquitofish", "cutthroat trout", "coho salmon", 
                         "rainbow/steelhead", "unidentified salmonid", "chinook salmon", 
                         "mountain whitefish", "unidentified lamprey"), 
         origin = c("N", "N", "N", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                    "A", "A", "A", "A", "N", "N", "N", "N", "N", "N", "N", "A", "A", 
                    "N", "A", "N", "N", "N", "N", "N", "A", "N", "N", "A", "A", "N", 
                    "A", "A", "A", "A", "N", "N", "N", "N", "N", "A", "N", "N", "N", 
                    "N", "N", "N", "N"), 
         habitat = c("B", "B", "B", "W", "W", "W", "W", "W", "W", "W", "W", "W", 
                     "W", "W", "W", "W", "W", "B", "B", "B", "B", "B", "B", "B", "B", 
                     "B", "B", "W", "B", "W", "B", "B", "B", "W", "B", "W", "B", "W", 
                     "W", "W", "B", "B", "B", "W", "B", "B", "B", "B", "B", "W", "W", 
                     "W", "W", "W", "W", "B", "B"), 
         hider = c("", "", "", "", "", "", "", "", "", "", "", "", 
                   "", "", "", "", "", "", "", "H", "H", "H", "", "H", "", "", 
                   "", "", "", "", "H", "H", "H", "", "", "H", "H", "", "", 
                   "H", "H", "H", "H", "", "H", "H", "H", "H", "H", "H", "H", 
                   "", "H", "", "", "", "H"), 
         tolerance = c("I", "", "S", "T", "I", "T", "T", "", "I", "T", "", "T", "T", 
                       "T", "T", "T", "I", "T", "I", "I", "I", "S", "", "S", "I", "T", 
                       "T", "I", "T", "I", "I", "I", "I", "I", "T", "S", "I", "I", "T", 
                       "I", "T", "T", "T", "I", "I", "S", "S", "S", "S", "T", "S", "S", 
                       "S", "S", "S", "S", "S"), 
         foraging = c("O", "", "S/S", "T", "I", "T", "I", "", "T", "T", "T", "T", 
                      "T", "T", "I", "I", "I", "O", "I", "I", "I", "T", "", "I", "S/S", 
                      "O", "O", "I", "O", "T", "I", "I", "I", "I", "H", "I", "I", "O", 
                      "O", "I", "O", "O", "O", "T", "I", "F/S", "F/S", "F/S", "F/S", 
                      "O", "T", "T", "T", "T", "T", "I", "F/S"), 
         reproduction = c("L", "L", "L", "PN", "PN", "PN", "PN", "PN", "LN", "PN", "", 
                          "VN", "PN", "", "PN", "PN", "", "V", "CN", "CN", "CN", "CN", 
                          "CN", "CN", "L", "V", "V", "L", "P/CN", "L", "L", "LN", "", "LV", 
                          "V", "V", "L", "", "V", "VN", "P/CN", "P/CN", "P/CN", "V", "L", 
                          "NLN", "NLN", "NLN", "NLN", "LB", "NLN", "NLN", "NLN", "NLN", 
                          "NLN", "NLN", "NLN"))
  ret <- structure(trait.list, 
                   .Names = c("family", "species","common_name", "origin", "habitat", 
                              "hider", "tolerance", "foraging", "reproduction"),
                   row.names = 1L:57L,
                   class = "data.frame")
  
  lunkers <- c("Cottus asper"              = 10,
               "Cottus rhotheus"           = 10,
               "Oncorhynchus clarki"       = 25,
               "Oncorhynchus mykiss"       = 30,
               "Acrocheilus alutaceus"     = 30,
               "Ptychocheilus oregonensis" = 30,
               "Catostomus macrocheilus"   = 30)
  
  ret$native      <- ret$origin  == 'N'
  ret$native.ben  <- ret$habitat == 'B' & ret$native
  ret$native.wc   <- ret$habitat == 'W' & ret$native
  ret$hider       <- ret$hider   == 'H'
  ret$sensitive   <- ret$tolerance    == 'S'
  ret$tolerant    <- ret$tolerance    == 'T'
  ret$nlns        <- ret$reproduction == 'NLN'
  ret$filter.feed <- ret$foraging == 'F/S'
  ret$top.carniv  <- ret$foraging == 'T' & ret$native
  ret$omnivore    <- ret$foraging == 'O'
  
  ret$lunker      <- lunkers[ret$species]
  ret$target.lunker <- ret$species %in% names(lunkers)
  
  return(ret)
}
