
# Vegetative cover pars;
veg.cov <- c(xcl = "r_canbtre", 
             xcs = "r_canstre", 
             xmw = "r_undwdy", 
             xmh = "r_undnwdy", 
             xgw = "r_gcwdy", 
             xgh = "r_gcnwdy", 
             xgb = "r_bare")

setClassLevels <- function(from.classes, to.classes){
  f <- function(x){
    if(!is.subset(na.omit(x), from.classes)){
      warning(sprintf('Unexpected class(es) encountered: %s', 
                      toString(setdiff(x, from.classes))))
    }
    factor(as.character(x), 
           levels = from.classes, 
           labels = to.classes)
  }
}

setVegClassLevels <- setClassLevels(c('C', 'D', 'E', 'M', 'N'), c('c', 'd', 'e', 'm', 'n'))

#'Format Riparian Vegetation data for calculations
#'
#'In addition to transposing the data to a wide format, this function creates
#'two sets of parameters: 'p_' and 'r_'.  The former are 'presence' parameters and
#'are TRUE whenever for any parameter with a nonzero vegetation cover class.  The
#'latter parameters are the cover classes converted to the arithmetic means of 
#'end points of the cover classes to numerically characterize each class.
#'@param uid a vector of site visit indicators
#'@param transect a vector of transect indicators
#'@param transdir a vector of bank indicators
#'@param parameter the measured parameter code.  One of: "underveg", 
#'"canveg", "canbtre", "canstre", "undwdy", "undnwdy", 
#'"gcwdy", "gcnwdy", "bare".
#'@param result the measured result for each parameter.  A vegetation class for
#'"underveg" and "canveg" and 0-4 for the others.
#'@return a data frame in wide format with the parameters necessary for metric calculation
#'@import plyr
formatRiparianVegetation <- function(uid, transect, transdir, parameter, result){
  parameter <- tolower(parameter)
  visrip <- data.frame(uid, transect, transdir, parameter, result)
  p.pars  <- c("canbtre", "canstre", "undwdy", "undnwdy", "gcwdy", "gcnwdy")
  r.pars  <- c("underveg", "canveg", "canbtre", "canstre", "undwdy", "undnwdy", 
               "gcwdy", "gcnwdy", "bare")
  visrip$presence <- visrip$result != '0'
  visrip          <- rename(visrip, c('result' = 'r', 'presence' = 'p'))
  vm <- reshape2::melt(visrip, measure.var = c('r', 'p'))
  sub.expr <- .((parameter %in% r.pars & variable == 'r') |
                (parameter %in% p.pars & variable == 'p'))
  vc <- reshape2::dcast(vm, uid + transect + transdir ~ variable + parameter, subset = sub.expr)
  vc[veg.cov] <- lapply(vc[veg.cov], function(x){
    x <- as.factor(x)
    x <- revalue(x, c('0' = 0, '1' = 0.05, '2' = 0.25, '3' = 0.575, '4' = 0.875),
                 warn_missing = FALSE)
    convertFactorToNumeric(x)
  })
  vc[paste0('p_', p.pars)] <- lapply(vc[paste0('p_', p.pars)], as.logical)
  vc
}

calculateVegPresence <- function(p_canbtre, p_canstre, p_undwdy, p_undnwdy, p_gcwdy, p_gcnwdy){
  xpcan  <-  p_canbtre | p_canstre
  xpmid  <-  p_undwdy  | p_undnwdy
  xpgveg <-  p_gcwdy   | p_gcnwdy
  xpcm   <- (p_canbtre | p_canstre) & (p_undwdy | p_undnwdy)
  xpmg   <- (p_undwdy  | p_undnwdy) & (p_gcwdy  | p_gcnwdy)
  xpcmg  <- (p_canbtre | p_canstre) & (p_undwdy | p_undnwdy) & (p_gcwdy | p_gcnwdy)
  xpmgw  <- (p_undwdy %&% p_gcwdy)
  cbind(xpcan, xpmid, xpgveg, xpcm, xpmg, xpcmg, xpmgw)
}

calculateVegetationTypes <- function(r_underveg, r_canveg, uid){
  f <- function(x, uid, metric.prefix){
    x   <- setVegClassLevels(x)
    ans <- prop.table(table(uid = uid, metric = x), 1)
    ans <- as.data.frame(ans, responseName = 'result')
    ans$metric <- paste(metric.prefix, ans$metric, sep = '_')
    ans
  }
  pmid <- f(r_underveg, uid, 'pmid')
  pcan <- f(r_canveg,   uid, 'pcan')
  rbindMetrics(pmid, pcan)
}

#'Calculates visible riparian vegetation metrics.
#'
#'\code{calculateRiparianVegetation} is used to calculate riparian vegetation metrics
#'from the visual riparian estimates section of the NRSA forms.
#'@param uid a vector of site-visit indicators
#'@param transect a vector of transect indicators
#'@param transdir a vector of transect direction indicators which identify the bank
#'on which the visual riparian plot was located
#'@param parameter a vector of parameter names (the left column on the form)
#'@param result a vector of visual riparian estimates; 0-4 for the cover estimates and
#'(D, C, E, M, N) for the vegetation types.
#'@return a data frame of riparian vegetation metrics
#'@import plyr
#'@export
calculateRiparianVegetation <- function(uid, transect, transdir, parameter, result){
  vc <- formatRiparianVegetation(uid, transect, transdir, parameter, result)

  # Add combinations of classes as columns of vc.
  vcmat <- as.matrix(vc[, veg.cov[-7]])
  vcmat.cols <- list(xc = 1:2, xm = 3:4, xcmw = 1:3, xcm = 1:4, xg = 5:6, 
                     xcmgw = c(1:3,5), xcmg = 1:6)
  class.comb <- sapply(vcmat.cols, function(x) rowSumsProtectNA(vcmat[, x]))
  vc <- cbind(vc, class.comb)
  
  # Presence and vegetation class metrics
  pres.mets <- calculateVegPresence(vc$p_canbtre, vc$p_canstre, vc$p_undwdy, vc$p_undnwdy,
                                    vc$p_gcwdy, vc$p_gcnwdy)
  veg.types.mets <- calculateVegetationTypes(vc$r_underveg, vc$r_canveg, vc$uid)
  vc <- cbind(vc, pres.mets)
  
  # Rename veg.cov to metric names for those parameters                                      
  names(vc) <- mapvalues(names(vc), veg.cov, names(veg.cov))
  mean.mets <- c("xcl", "xcs", "xmw", "xmh", "xgw", "xgh", "xgb","xc", "xm", 
                 "xcmw", "xcm", "xg", "xcmgw", "xcmg","xpcan", "xpmid",
                 "xpgveg", "xpcm", "xpmg", "xpcmg", "xpmgw")
  vcmat     <- as.matrix(vc[mean.mets])
  mean.mets <- rowmean(vcmat, vc$uid, reorder = T, na.rm = T)
  #rownames(mean.mets) <- sort(unique(vc$uid))
  mean.mets   <- meltMetrics(mean.mets)
  
  mets <- rbindMetrics(mean.mets, veg.types.mets)
  is.na(mets$result) <- is.nan(mets$result)
  progressReport("Finished with riparian vegetation metrics.")
  return(mets)
}
