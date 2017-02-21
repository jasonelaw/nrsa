kHICategories                <- c("0", "P", "C", "B")
kHIAgriculturalParameters    <- c('row', 'past')
kHINonAgriculturalParameters <- c("build", "landfl", "log", "mine", "park", 
                                  "pave", "pipes", "road", "wall")
kHIParameters                <- c(kHIAgriculturalParameters, 
                                  kHINonAgriculturalParameters)
#'Calculate human influence metrics
#'
#'This function calculates the human influence metrics.  The parameter names are
#'assumed to be in lower case. Missing data is not allowed and will result
#'in an error.
#'
#'@param uid a vector that differentiates among sites or site visits. Metrics will be
#'calculated for each value of \code{uid}.
#'@param parameter a vector of parameter names for the 11 human influence parameters:
#'must be one of \code{"row"}, \code{"past"}, \code{"build"}, \code{"landfl"}, \code{"log"},
#'\code{"mine"}, \code{"park"}, \code{"pave"}, \code{"row"}, \code{"pipes"}, \code{"road"}, 
#'or \code{"wall"}.
#'@param result a vector of results \code{0} (not present), \code{P} (present > 10 m), 
#'\code{C} (present < 10 m), \code{B} (present on bank).
#'
#'@return returns a dataframe with three columns: uid, metric, result
#'@import plyr
#'@export
#'@examples
#'hi <- expand.grid(uid = 1:1000, transect = letters[1:10], 
#'                  parameter = c("wall", "build", "pave", "road", "pipes", "landfl", "park", 
#'                                "row", "past", "log", "mine"))
#'hi$result <- sample(c('0', 'C', 'B', 'P'), size = 11 * 10 * 1000, replace = T)
#'calculateHumanInfluence(hi$uid, hi$parameter, hi$result)
calculateHumanInfluence <- function(uid, parameter, result){
  parameter <- tolower(parameter)
  assert_is_a_non_missing_nor_empty_string(result)
  assert_all_are_same_length(uid, parameter, result)
  assert_is_subset(parameter, kHIParameters)
  assert_is_subset(result, kHICategories)
  stopifnot(!is.na(result),
            length(uid) == length(parameter),
            length(uid) == length(result),
            parameter %in% kHIParameters,
            result %in% kHICategories)
  parameter <- factor(parameter, kHIParameters)
  result    <- factor(as.character(result), kHICategories)
  hprop <- calculateHIProportions(uid, parameter, result)
  w1h   <- calculateHIWeightedSums(hprop)
  hi    <- calculateHIParameterSums(hprop)
  
  # Put everything together and clean up names
  w1h$.id <- 'w1h'
  ans  <- rbind.fill(w1h, hi)
  ans2 <- ddply(ans[ans$metric %in% c('B', 'C', 'P'),], .(.id, uid), 
                summarize, result = sum(result))
  ans  <- rbind.fill(ans, ans2)
  ans  <- ans[!(ans$metric %in% '0'), ]
  ans$metric <- renameHumanInfluenceMetrics(ans$.id, ans$metric)
  ans$.id <- NULL
  progressReport('Done with human influence mean mets')
  arrange(ans, uid, metric)
}

calculateHIParameterSums <- function(x){
  xag   <- x[, kHIAgriculturalParameters,, drop = F]
  xnoag <- x[, kHINonAgriculturalParameters,, drop = F]
  hag   <- margin.table(xag, c(1,3))
  hnoag <- margin.table(xnoag, c(1,3))
  hall  <- margin.table(x, c(1,3))
  meltMetrics(hag = hag, hnoag = hnoag, hall = hall)
}

#' Calculate weighted human influence metrics
#' 
#' Calculate weighted human influence metrics
#' @param proportions a uid x parameter x category matrix of proportions
#' @param weights a weight vector for categories 0, P, C, and B.
calculateHIWeightedSums <- function(proportions, weights = c(0.0, 0.6667, 1.0, 1.5)){
  proportions <- proportions[, , kHICategories, drop = F]
  w1h <- sweep(proportions, 3, weights, '*')
  w1h      <- rowSums(w1h, dims = 2)
  w1_hall  <- rowSums(w1h)
  w1_hag   <- rowSums(w1h[, kHIAgriculturalParameters, drop = F])
  w1_hnoag <- rowSums(w1h[, kHINonAgriculturalParameters, drop = F])
  meltMetrics(cbind(w1h = w1h, ag = w1_hag, noag = w1_hnoag, all = w1_hall))
}

calculateHIProportions <- function(uid, parameter, result){
  tbl <- table(uid, parameter, result)
  n   <- rowSums(tbl, dims = 2)
  tbl  <- abind::abind(tbl, 
                       CB = tbl[,, 'B'] + tbl[,, 'C'],
                       along = 3)
  sweep(tbl, 1:2, n, '/')
}

#'Calculate human influence standard deviation metrics
#'
#'This function calculates the human influence standard deviation metrics. 
#'
#'@param uid a vector that differentiates among sites or site visits.
#'@param transect a vector of transect names for the site
#'@param result a vector of results 0 (not present), P (present > 10 m), 
#'C (present < 10 m), B (present on bank)
#'
#'@return returns a dataframe with three columns: uid, metric, result
#'@import plyr
#'@export
calculateHumanInfluenceSD <- function(uid, transect, result){
  x <- data.frame(uid = uid, transect = transect, result = result)
  #x <- subset(x, result %in% c('C', 'B'))
  x$result <- factor(tolower(x$result[, drop = T]))
  x$transect <- as.character(x$transect)
  ans <- ddply(x, .(uid), function(x){
    tbl <- table(x$result, x$transect)[c('c', 'b'), ]
    cb <- tbl['b', ] + tbl['c', ]
    wcb <- 1.5 * tbl['b', ] + tbl['c', ]
    tbl <- rbind(tbl, cb = cb, wcb = wcb)
    apply(tbl, 1, sd)
  } )
  ans <- rename(ans, c(b = 'sdb_hall', c = 'sdc_hall', cb = 'sdcb_hall', wcb = 'sdwcb_hall'))
  progressReport('Done with human influence sd mets')
  meltMetrics(ans)
}

#'Renames metrics for human influence
#'
#'Renames human influence metrics based on a group variable (created by meltMetrics)
#'and a metric variable (created during grouping operations).  See calculateHumanInfluence
#'code for how the group vector and metric vector are created and what the values are.
#'This is definitely not to be called outside the nrsa package because it depends
#'expicitly on the code in calculateHumanInfluence.
#'
#'@param group a vector of groups based on the names passed to meltMetrics
#'@param metric a vector metric indicators
#'@import plyr
renameHumanInfluenceMetrics <- function(group, metric){
  metric <- mapvalues(as.character(metric), NA, '')
  metric <- revalue(metric, c('B' = 'b', 'C' = 'c', 'CB' = 'cb', 'P' = 'f'))
  metric <- revalue(metric, c('build' = 'bldg', 'landfl' = 'ldfl', 'log'  = 'log', 
                              'mine'  = 'mine', 'park'   = 'park', 'past' = 'pstr', 
                              'pave'  = 'pvmt', 'pipes'  = 'pipe', 
                              'road'  = 'road', 'row'    = 'crop', 'wall' = 'wall'))
  metrics <- ifelse(group == 'w1h',
                    sprintf('%s_%s', group, metric),
                    sprintf('x%s_%s', metric, group))
  #Special cases
  revalue(metrics, c('w1h_noag' = 'w1_hnoag', 'w1h_ag' = 'w1_hag', 
                     'w1h_all'  = 'w1_hall', 'xcb_hnoag' = 'xcb_hnag'))
}

#'Calculates a bank hardening metric for City of Portland use.
#'
#'Calculates a bank hardening metric for City of Portland use.  The percentage of bank
#'where either bldg, pave, wall, or road are marked 'B' is calculated.
#'@param uid a vector that differentiates among sites or site visits.
#'@param transect a vector of transect indicators
#'@param plot a vector of transect direction indicators
#'@param parameter a vector of parameter names for the 11 human influence parameters
#'@param result a vector of results 0 (not present), P (present > 10 m), 
#'C (present < 10 m), B (present on bank)
#'@import plyr
#'@export
calculateBankHardening <- function(uid, transect, plot, parameter, result){
  x <- data.frame(uid = uid, transect = transect, plot = plot, parameter = parameter, result = result)
  x <- x[x$parameter %in% c('build', 'pave', 'wall', 'road'), ]
  plot.data <- ddply(x, .(uid, transect, plot), function(x){
    if(all(is.na(x$result))){
      return(c(is.hardened = NA))
    } else {
      return(c(is.hardened = any(x$result %in% 'B')))
    } 
  })
  plot.data$is.hardened <- factor(plot.data$is.hardened, levels = c('TRUE', 'FALSE'))
  ans <- ddply(plot.data, .(uid), function(x) c(result = unname(prop.table(table(x$is.hardened))['TRUE'])))
  ans$metric <- 'bankhard'
  progressReport("Finished with bank hardening metric: bankhard.")
  return(ans[,c('uid', 'metric', 'result')])
}