#'Calculates Canopy Densiometer metrics
#'
#'\code{calculateCanopyCover} calculates these canopy cover metrics from the densiometer readings:
#'Wadeable Protocal:
#'xcdenmid xcdenbk vcdenmid vcdenbk nbnk nmid.
#'
#'Boatable Protocal:
#'xcdenbk  vcdenbk nbnk
#'
#'@param uid a vector of site-visits
#'@param is.bank whether the measurement is a bank measurement or not
#'@param densiometer a vector of densiometer measurments from 0-17.
#'@importFrom plyr ddply mapvalues '.'
#'@importFrom reshape2 melt
#'@export
calculateCanopyCover <- function(uid, is.bank, densiometer){
  is.bank <- mapvalues(is.bank, c(F, T), c('mid', 'bnk'))
  x <- data.frame(uid = uid, is.bank = is.bank, result = densiometer)
  x$result <- x$result / 17 * 100
  calc <- function(x){
    c(vcden = sd(x$result, na.rm=T), 
      xcden = mean(x$result, na.rm=T), 
      n     = count(x$result))
  }
  mets <- ddply(x, .(uid, is.bank), calc)
  mets <- melt(mets, id.var = c('uid', 'is.bank'), 
               variable.name = 'metric', 
               value.name = 'result')
  
  # clean up to make it look pretty
  mets$metric <- as.factor(paste(mets$metric, mets$is.bank, sep = ''))
  mets$uid    <- as.factor(mets$uid)
  mets <- subset(mets, select = c('uid', 'metric', 'result'))
  is.na(mets$result) <- is.nan(mets$result)
  mets <- arrange(allFacToChar(mets), uid, metric)
  progressReport('Canopy cover metrics done')
  return(mets)
}

