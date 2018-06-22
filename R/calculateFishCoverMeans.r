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
#' @param parameter a vector parameter codes: ALGAE, BOULDR, BRUSH, LVTREE, 
#' MACPHY, OVRHNG, STRUCT, UNDCUT, WOODY.
#' @param cover a vector of cover values: 0-4.
#' @export
#' @examples
#' df <- expand.grid(uid = 1:10, transect = LETTERS[1:10], 
#'                   parameter = c("ALGAE", "BOULDR", "BRUSH", "LVTREE", 
#'                                 "MACPHY", "OVRHNG", "STRUCT", "UNDCUT", "WOODY"))
#' df$cover <- sample(0:4, size = nrow(df), replace = T)
#' calculateFishCover(df$uid, df$parameter, df$cover)
calculateFishCover <- function(uid, parameter, cover){
  fc <- formatFishCover(uid, parameter, cover)
  fcm <- calculateFishCoverMeans(fc$uid, fc$parameter, fc$cover)
  
  fc   <- fc[fc$parameter %in% c('ohv', 'ucb'),]
  fcsd <- calculateBankCoverVar(fc$uid, fc$parameter, fc$cover)
  rbind(fcm, fcsd)
}

calculateFishCoverMeans <- function(uid, parameter, cover){
  isNatural <- c('rck', 'brs', 'lvt', 'ohv', 'ucb', 'lwd')
  isBig     <- c('rck', 'hum', 'ucb', 'lwd')
  
  x <- data.frame(uid, parameter, cover)
  x$presence <- x$cover > 0
  x$is.big <- x$parameter %in% isBig
  x$is.natural <- x$parameter %in% isNatural
  
  x <- ddply(x, .(uid, parameter), function(x){
    data.frame(xfc = mean(x$cover, na.rm = T),
               pfc = mean(x$presence, na.rm = T))
  })

  sum.calc <- function(x){
    c(pfc_all = sum(x$pfc, na.rm = T),
      pfc_big = sum(x$pfc[x$is.big], na.rm = T),
      pfc_nat = sum(x$pfc[x$is.natural], na.rm = T),
      xfc_all = sum(x$xfc, na.rm = T),
      xfc_big = sum(x$xfc[x$is.big], na.rm = T),
      xfc_nat = sum(x$xfc[x$is.natural], na.rm = T))
  }
  index.mets <- ddply(x, .(uid), sum.calc)
  index.mets <- meltMetrics(index.mets)
  x <- reshape2::melt(x, measure.vars = c('xfc', 'pfc'), variable.name = 'metric', value.name = 'result')
  x$metric   <- paste(x$metric, x$parameter, sep = '_')
  mets <- rbind(x[, c('uid', 'metric', 'result')],
                index.mets)
  progressReport("Fish cover means finished.")
  return(mets)
}

calculateBankCoverVar <- function(uid, parameter, cover){
  stopifnot(parameter %in% c('ohv', 'ucb'))
  x <- data.frame(uid = uid, parameter = parameter, cover = cover)
  spread.calc <- function(x){
    c(sdfc = sd(x$cover, na.rm = T), 
      idr  = idr(x$cover), 
      iqr  = iqr(x$cover))
  }
  mets <- plyr::ddply(x, c('uid', 'parameter'), spread.calc)
  mets <- reshape2::melt(mets, id.var = c('uid', 'parameter'), 
               value.name = 'result', 
               variable.name = 'metric')
  mets$metric <- ifelse(mets$metric == 'sdfc', 
                        paste(mets$metric, mets$parameter, sep = "_"), 
                        paste(mets$metric, mets$parameter, sep = ""))
  mets <- subset(mets, select = c('uid', 'metric', 'result'))
  progressReport('Bank cover variation metrics finished.')
  return(mets)
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
