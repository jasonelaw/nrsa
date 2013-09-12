#'Calculate human influence metrics
#'
#'This function calculates the human influence metrics.  The parameter names are
#'assumed to be in lower case
#'
#'@param uid a vector that differentiates among sites or site visits.
#'@param parameter a vector of parameter names for the 11 human influence parameters
#'@param result a vector of results 0 (not present), P (present > 10 m), 
#'C (present < 10 m), B (present on bank)
#'
#'@return returns a dataframe with three columns: uid, metric, result
#'@importFrom abind abind
#'@importFrom plyr ddply rbind.fill mapvalues arrange
#'@export
calculateHumanInfluence <- function(uid, parameter, result){
  #TODO: Add a 'bank hardening' metric which is sum of percentages for each transect.
  stopifnot(length(uid) == c(length(parameter), length(uid) == length(result)))
  levels(parameter) <- tolower(levels(parameter))
  ag.pars <- c('row', 'past')
  noag.pars <- setdiff(levels(parameter), ag.pars)
  # Calculate sums by parameter category over the transects and transect directions
  # and divide by number of observations per parameter.  
  denom <- table(uid, parameter)
  tbl   <- table(uid, parameter, result)
  tbl.CB <- tbl[,, 'B', drop = F] + tbl[,, 'C', drop = F]
  dimnames(tbl.CB)[[3]] <- 'CB'
  tbl  <- abind(tbl, tbl.CB, along = 3)
  hall <- sweep(tbl, 1:2, denom, '/')
  # Do the weighted sums
  w1h <- sweep(hall[,,c('0','P', 'C', 'B')], 3, c(0.0, 0.6667, 1.0, 1.5), '*')
  w1h <- margin.table(w1h, 1:2)
  w1_hag   <- rowSums(w1h[, ag.pars])
  w1_hnoag <- rowSums(w1h[, noag.pars])
  w1_hall  <- w1_hag + w1_hnoag
  w1h      <- cbind(w1h, ag = w1_hag, noag = w1_hnoag, all = w1_hall)
  # Add sums across the parameters
  hag   <- margin.table(hall[, ag.pars,, drop = F], c(1,3))
  hnoag <- margin.table(hall[, noag.pars,, drop = F], c(1,3))
  hall  <- margin.table(hall, c(1,3))
  # Put everything together and clean up names
  ans  <- meltMetrics(hag = hag, hnoag = hnoag, hall = hall, w1h = w1h)
  ans2 <- ddply(subset(ans, metric %in% c('B', 'C', 'P')), .(.id, uid), 
                summarize, result = sum(result))
  ans  <- rbind.fill(ans, ans2)
  ans  <- subset(ans, !(metric %in% '0'))
  ans$metric <- renameHumanInfluenceMetrics(ans$.id, ans$metric)
  ans$.id <- NULL
  progressReport('Done with human influence mean mets')
  arrange(ans, uid, metric)
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
#'@importFrom plyr ddply rename
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
#'@importFrom plyr mapvalues revalue
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
#'@importFrom plyr ddply
#'@export
calculateBankHardening <- function(uid, transect, plot, parameter, result){
  x <- data.frame(uid = uid, transect = transect, plot = plot, parameter = parameter, result = result)
  x <- subset(x, parameter %in% c('build', 'pave', 'wall', 'road'))
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