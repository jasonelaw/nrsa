#' Calculates wadeable bank metrics
#' 
#' Calculates wadeable bank metrics:
#' n_ba, bka_q1, medbk_a, bka_q3, xbka, sdbk_a, intqbka, n_un, bkun_q1, medbkun, 
#' bkun_q3, xun, sdun, intqbkun.  The functions without uid arguments return
#' named vectors of metrics and operate on a single site of measurements.
#' @param uid a vector of site identifiers
#' @param angle a vector of bank angle measurements
#' @param undercut a vector of bank undercut measurements
#' @return a 'metric' data.frame
#' @export
#' @aliases calculateWadeAngleMetrics calculateWadeUndercutMetrics
#' @import plyr
calculateWadeBankMetrics <- function(uid, angle, undercut){
  if (!is.numeric(angle)){
    angle <- as.numeric(as.character(angle))
  }
  if (!is.numeric(undercut)){
    angle <- as.numeric(as.character(undercut))
  }
  x <- data.frame(uid, angle, undercut)
  ans <- ddply(x, 'uid', function(x){
    c(calculateWadeAngleMetrics(x$angle),
      calculateWadeUndercutMetrics(x$undercut))
  })
  ans <- arrange(allFacToChar(ans), uid, metric)
  progressReport("Finished with wadeable bank metrics.")
  return(ans)
}

#' @rdname calculateWadeBankMetrics
#' @export
calculateWadeUndercutMetrics <- function(undercut){
  n_un <- count.notna(undercut)
  mets <- summary.nrsa(uncercut, probs = c(0.25, 0.5, .75), na.rm = T)
  mets <- c(n_un, mets)
  names(mets) <- c('n_un', 'bkun_q1', 'medbkun', 'bkun_q3',  'xun', 
                   'sdun', 'intqbkun')
  return(mets)
}

#' @rdname calculateWadeBankMetrics
#' @export
calculateWadeAngleMetrics <- function(angle){
  n_ba <- count.notna(angle)
  mets <- summary.nrsa(angle, probs = c(0.25, 0.5, .75), na.rm = T)
  mets <- c(n_ba, mets)
  names(mets) <- c('n_ba', 'bka_q1','medbk_a', 'bka_q3', 'xbka', 'sdbk_a', 
                   'intqbka')
  return(mets)
}

#' Calculate boat protocol number of observed wetted widths
#' 
#' Calculate boat protocol number of observed wetted widths: n_w
#' @param uid a vector of site identifiers
#' @param wetwid a vector of wetted width measurements
#' @return a 'metric' data.frame
#' @import plyr
#' @export
calculateBoatNumberWettedWidth <- function(uid, wetwid){
  x   <- data.frame(as.character(uid), wetwid)
  ans <- ddply(x, "uid", summarize, result = count(wetwid))
  ans$metric <- "n_w"
  progressReport("Finished with boatable metric n_w.")
  ans
}

#' Calculate boat protocol bank angle metrics
#' 
#' Calculate boat protocol bank angle metrics: bangmode, bap_low, bap_med, bap_mis
#' ,bap_stp, bap_vst, n_ba.
#' @param uid a vector of site identifiers
#' @param angle a vector of bank angle categories: '0-5', '5-30', '30-75', '75-100'.
#' @return a 'metric' data.frame
#' @export
#' @import plyr
calculateBoatAngleMetrics <- function(uid, angle){
  kModalAngleMetricMap <- c(`0-5`           = "low", 
                            `5-30`          = "med", 
                            `30-75`         = "stp", 
                            `75-100`        = "vst", 
                            `0-5, 5-30`     = "low-med", 
                            `5-30, 30-75`   = "med-stp", 
                            `30-75, 75-100` = "stp-vst")
  kAngleMetricMap <- c(`0-5`    = "bap_low", 
                       `5-30`   = "bap_med", 
                       `30-75`  = "bap_stp",
                       `75-100` = "bap_vstp")
  kAllowedAngles <- c('0-5', '5-30', '30-75', '75-100')

  angle <- factor(as.character(angle), levels = kAllowedAngles)
  ang.counts    <- table(uid = uid, metric = angle)
  ang.prop      <- prop.table(ang.counts, 1) * 100
  ang.prop.list <- as.data.frame(unclass(ang.prop))
  ang.prop      <- as.data.frame(ang.prop, responseName = 'result')
  ang.prop$metric <- revalue(ang.prop$metric, kAngleMetricMap)
  
  n_ba <- tapply(angle, uid, count)
  n_ba <- convertNamedVectorToMetricDF(n_ba)
  
  # Modal bank angle
  bangmode <- modalClass2(ang.prop.list)
  bangmode <- data.frame(uid = rownames(ang.prop.list), metric = 'bangmode', 
                         result = bangmode)
  bangmode$metric <- revalue(bangmode$metric, kModalAngleMetricMap)
  bangmode$result <- addNA2(as.factor(bangmode$result), 'None')
  mets <- rbindMetrics(ang.prop, n_ba, bangmode)
  progressReport("Finished with boatable bank angle metrics.")
  return(mets)
}
