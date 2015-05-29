#' Calculate fish cover metrics
#' 
#' \code{calculateFishCoverMeans} calculates fish cover metrics.  The metrics
#' returned are: pfc_alg, pfc_rck, pfc_brs, pfc_lvt, pfc_aqm, pfc_ohv, pfc_hum,
#' pfc_ucb, pfc_lwd xfc_alg, xfc_rck, xfc_brs, xfc_lvt, xfc_aqm, xfc_ohv,
#' xfc_hum, xfc_ucb, xfc_lwd pfc_all, pfc_big, pfc_nat xfc_all, xfc_big, xfc_nat.
#' \code{calculateBankCoverVar}
#' calculates a group of fish cover metrics dealing with the variability in cover
#' provided by bank features (overhanging or undercut banks).  The metrics are
#' sdfc_ucb, sdfc_ohv idrucb, idrohv, iqrucb, iqrohv.
#'
#' @param uid a vector of site-visit indicators
#' @param parameter a vector parameter codes as returned by \link{formatFishCover}.
#' Values restricted to be one of alg, rck, brs, lvt, aqm, ohv, hum, ucb, lwd.
#' @param cover a vector of cover values as returned by \link{formatFishCover}.  These
#' are not the raw cover values (0-4) from the field form.
#' @import plyr
#' @export
#' @examples
#' df <- expand.grid(uid = 1:10, transect = LETTERS[1:10], 
#'                   parameter = c("ALGAE", "BOULDR", "BRUSH", "LVTREE", 
#'                                 "MACPHY", "OVRHNG", "STRUCT", "UNDCUT", "WOODY"))
#' df$cover <- sample(0:4, size = nrow(df), replace = T)
#' fc <- formatFishCover(df$uid, df$parameter, df$cover)
#' fcm <- calculateFishCoverMeans(fc$uid, fc$parameter, fc$cover)
#' fcsd <- with(subset(fc, parameter %in% c('ohv', 'ucb')),
#'              calculateBankCoverVar(uid, parameter, cover))
calculateFishCoverMeans <- function(uid, parameter, cover){
#   isNatural <- c('rck', 'brs', 'lvt', 'ohv', 'ucb', 'lwd')
#   isBig     <- c('rck', 'hum', 'ucb', 'lwd')
# 
#   x <- data.frame(uid = uid, parameter = parameter, cover = cover)
#   x$presence <- x$cover > 0
#   x$is.big <- x$parameter %in% isBig
#   x$is.natural <- x$parameter %in% isNatural
# 
#   xfc <- fastSummaries(x$cover,
#                        x[,c('uid', 'parameter', 'is.big', 'is.natural')],
#                        igroupMeans, na.rm = T)
#   xfc$metric <- 'xfc'
#   pfc <- fastSummaries(x$presence, 
#                        x[,c('uid', 'parameter', 'is.big', 'is.natural')], 
#                        igroupMeans, na.rm = T)
#   pfc$metric <- 'pfc'
#   x <- rbind(xfc, pfc)
#   xc <- reshape2::dcast(x, ... ~ metric, value.var = 'result')
#   sum.calc <- function(x){
#     c(pfc_all = sum(x$pfc, na.rm = T),
#       pfc_big = sum(x$pfc[x$is.big], na.rm = T),
#       pfc_nat = sum(x$pfc[x$is.natural], na.rm = T),
#       xfc_all = sum(x$xfc, na.rm = T),
#       xfc_big = sum(x$xfc[x$is.big], na.rm = T),
#       xfc_nat = sum(x$xfc[x$is.natural], na.rm = T))
#   }
#   index.mets <- ddply(xc, .(uid), sum.calc)
#   index.mets <- meltMetrics(index.mets)
#   x$metric   <- paste(x$metric, x$parameter, sep = '_')
#   mets <- rbind(subset(x, select = c('uid', 'metric', 'result')),
#                 index.mets)
#   progressReport("Fish cover means finished.")
#   return(mets)
}

calculateFishCoverMeans2 <- function(uid, parameter, cover){
  isNatural <- c('rck', 'brs', 'lvt', 'ohv', 'ucb', 'lwd')
  isBig     <- c('rck', 'hum', 'ucb', 'lwd')
  
  x <- 
    data.frame(uid = uid, parameter = parameter, cover = cover) %>%
      mutate(presence = cover > 0) %>%
      group_by(uid, parameter) %>%
      summarize(xfc = mean(cover, na.rm = T),
                pfc = mean(presence, na.rm = T)) 
  
  index.mets <-
    x %>%
      mutate(is.big     = parameter %in% isBig,
             is.natural = parameter %in% isNatural) %>%
      group_by(uid) %>%
      summarize(pfc_all = sum(pfc, na.rm = T),
                pfc_big = sum(pfc[is.big], na.rm = T),
                pfc_nat = sum(pfc[is.natural], na.rm = T),
                xfc_all = sum(xfc, na.rm = T),
                xfc_big = sum(xfc[is.big], na.rm = T),
                xfc_nat = sum(xfc[is.natural], na.rm = T)) %>%
      gather(metric, result, -uid) %>%
      select(uid, metric, result)
  
  x <- 
    x %>%
      gather(metric.root, result, xfc, pfc) %>%
      unite(metric, metric.root, parameter) %>%
      rbind_list(index.mets) %>%
      arrange(uid, metric)
#   progressReport("Fish cover means finished.")
  return(x)
}

#'@rdname calculateFishCoverMeans
#'@param parameter For calculateBankCoverVar, should a vector of 'ohv' or 'ucb' codes.
#'@import plyr
#'@export
calculateBankCoverVar <- function(uid, parameter, cover){
  stopifnot(parameter %in% c('ohv', 'ucb'))
  x <- data.frame(uid = uid, parameter = parameter, cover = cover)
  spread.calc <- function(x){
    c(sdfc = sd(x$cover, na.rm = T), 
      idr  = idr(x$cover), 
      iqr  = iqr(x$cover))
  }
  mets <- ddply(x, .(uid, parameter), spread.calc)
  mets <- reshape2::melt(mets, id.var = c('uid', 'parameter'), 
               value.name = 'result', 
               variable.name = 'metric')
  mets$metric <- ifelse(mets$metric == 'sdfc', 
                        paste(mets$metric, mets$parameter, sep = "_"), 
                        paste(mets$metric, mets$parameter, sep = ""))
  mets <- subset(mets, select = c('uid', 'metric', 'result'))
  #progressReport('Bank cover variation metrics finished.')
  return(mets)
}

calculateBankCoverVar2 <- function(uid, parameter, cover){
  stopifnot(parameter %in% c('ohv', 'ucb'))
  data.frame(uid = uid, parameter = parameter, cover = cover) %>%
    group_by(uid, parameter) %>%
    summarize(sdfc = sd(cover, na.rm = T), 
              idr  = idr(cover), 
              iqr  = iqr(cover)) %>%
    gather(met.prefix, result, -uid, -parameter) %>%
    unite(metric, met.prefix, parameter, sep = '') %>%
    mutate(metric = str_replace(metric, 'sdfc', 'sdfc_')) %>%
    select(uid, metric, result)
  #progressReport('Bank cover variation metrics finished.')
}

#' Format fish cover data for calculations
#'
#' \code{formatFishCover} formats the fish cover data for calculations.  It checks remaps parameter
#' names, converts cover codes to proportions and returns a data frame that can be
#' used as input to the other fish cover metric functions.
#'
#' @param uid a vector of site-visit indicators
#' @param parameter a vector parameter codes.
#' These should be the original nrsa codes: ALGAE, BOULDR, LVTREE, etc
#' @param cover a vector of cover values (integers 0-4)
#' @export
formatFishCover <- function(uid, parameter, cover){
  # Constants
  coverVals   <- c('0' = 0, '1' = 0.05, '2' = 0.25, '3' = 0.575, '4' = 0.875)
  isBankCover <- c('ohv', 'ucb')
  AcceptableParameters <- c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE', 'MACPHY', 
                            'OVRHNG', 'STRUCT', 'UNDCUT', 'UNDERCUT', 'WOODY')
  ParameterMetricMap <- list('alg' = 'ALGAE',  'rck' = 'BOULDR', 'brs' = 'BRUSH', 
                             'lvt' = 'LVTREE', 'aqm' = 'MACPHY', 'ohv' = 'OVRHNG',
                             'hum' = 'STRUCT', 'ucb' = c('UNDCUT', 'UNDERCUT'),
                             'lwd' = 'WOODY')
  stopifnot(parameter %in% AcceptableParameters, cover %in% 0:4 | is.na(cover))
  x <- data.frame(uid = uid, parameter = parameter, cover = cover)
  x$parameter <- as.factor(x$parameter)
  levels(x$parameter) <- ParameterMetricMap
  x$cover <- coverVals[as.character(x$cover)]
  x$is.bankcover <- x$parameter %in% isBankCover
  progressReport('Fish cover data formatted.')
  return(x)
}
