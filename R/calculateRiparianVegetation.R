formatRiparianVegetation <- function(uid, transect, transdir, parameter, result){
  visrip <- data.frame(uid, transect, transdir, parameter, result)
  # Vegetative cover pars; names used to rename later
  veg.cov <- c(xcl = 'CANBTRE', xcs = 'CANSTRE', xmw = 'UNDWDY', 
               xmh = 'UNDNWDY', xgw = 'GCWDY', xgh = 'GCNWDY',
               xgb = 'BARE')  
  # Parameters needed for presence versus category calculations
  p.pars <- c("CANBTRE", "CANSTRE", "UNDWDY", "UNDNWDY", "GCWDY", "GCNWDY")
  r.pars  <- c("UNDERVEG", "CANVEG", "CANBTRE", "CANSTRE", 
                 "UNDWDY", "UNDNWDY", "GCWDY", "GCNWDY", "BARE")
  # Create necessary presence (P_) and result (R_) variables
  visrip$presence <- visrip$result != '0'
  visrip          <- rename(visrip, c('result' = 'R', 'presence' = 'P'))
  vm <- melt(visrip, measure.var = c('R', 'P'))
  sub.expr <- (parameter %in% r.pars  & variable == 'R') |
                (parameter %in% p.pars & variable == 'P')
  vc <- dcast(vm, uid + transect + transdir ~ variable + parameter, subset = sub.expr)
  # Ensure that all the levels are present; in case some weren't used in the data.
  cov.cat <- c(C = 'c', D = 'd', E = 'e', M = 'm', N = 'n')
  vc$R_UNDERVEG <- factor(vc$R_UNDERVEG, levels = names(cov.cat), labels = cov.cat)
  vc$R_CANVEG   <- factor(vc$R_CANVEG,   levels = names(cov.cat), labels = cov.cat)
  # Vegetation class area cover characterizations -- individual classes
  # Use arithmetic means of end points to numerically characterize each cover
  # class.  This section just converts the 0:5 to the end points.
  veg.cov <- mapvalues(veg.cov, veg.cov, paste0('R_', veg.cov))
  p.pars  <- paste0('P_', p.pars, sep = '_')
  vc[veg.cov] <- lapply(vc[veg.cov], function(x){
    x <- as.factor(x)
    x <- revalue(x, c('0' = 0, '1' = 0.05, '2' = 0.25, '3' = 0.575, '4' = 0.875))
    convertFactorToNumeric(x)
  })
  #vc[p.pars] <- lapply(vc[p.pars], CharacterToLogical)
  vc
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
#'@importFrom reshape2 melt dcast
#'@importFrom plyr '.' split_indices id split_labels
#'@export
calculateRiparianVegetation <- function(uid, transect, transdir, parameter, result){
  vc <- formatRiparianVegetation(uid, transect, transdir, parameter, result)
  veg.cov <- structure(c("R_CANBTRE", "R_CANSTRE", "R_UNDWDY", "R_UNDNWDY", 
                         "R_GCWDY", "R_GCNWDY", "R_BARE"), 
                       .Names = c("xcl", "xcs", "xmw", "xmh", "xgw", "xgh", "xgb"))
  pres <- c("P_CANBTRE", "P_CANSTRE", "P_UNDWDY", "P_UNDNWDY", "P_GCWDY", 
            "P_GCNWDY")
  # Add combinations of classes as columns of vc.  Uses a named list of
  # columns, where each element is the columns needed for each combo metric.
  vcmat <- as.matrix(vc[, c('R_CANBTRE', 'R_CANSTRE', 'R_UNDWDY', 'R_UNDNWDY', 'R_GCWDY', 'R_GCNWDY')])
  vc$xc    <- rowSumsProtectNA(vcmat[, 1:2])
  vc$xm    <- rowSumsProtectNA(vcmat[, 3:4])
  vc$xcmw  <- rowSumsProtectNA(vcmat[, 1:3])
  vc$xcm   <- rowSumsProtectNA(vcmat[, 1:4])
  vc$xg    <- rowSumsProtectNA(vcmat[, 5:6])
  vc$xcmgw <- rowSumsProtectNA(vcmat[, c(1:3,5)])
  vc$xcmg  <- rowSumsProtectNA(vcmat)

  # Add presence metrics
  vc$xpcan  <-  vc$P_CANBTRE | vc$P_CANSTRE
  vc$xpmid  <-  vc$P_UNDWDY  | vc$P_UNDNWDY
  vc$xpgveg <-  vc$P_GCWDY   | vc$P_GCNWDY
  vc$xpcm   <- (vc$P_CANBTRE | vc$P_CANSTRE) & (vc$P_UNDWDY | vc$P_UNDNWDY)
  vc$xpmg   <- (vc$P_UNDWDY  | vc$P_UNDNWDY) & (vc$P_GCWDY  | vc$P_GCNWDY)
  vc$xpcmg  <- (vc$P_CANBTRE | vc$P_CANSTRE) & (vc$P_UNDWDY | vc$P_UNDNWDY) & 
               (vc$P_GCWDY   | vc$P_GCNWDY)
  vc$xpmgw  <- (vc$P_UNDWDY %&% vc$P_GCWDY)
  
  # Rename veg.cov to metric names for those parameters                                      
  names(vc) <- mapvalues(names(vc), veg.cov, names(veg.cov))
  
  # Calculate all of the metrics
  # List of vegetative cover and combo metrics that need means calculated.
   mean.mets <- c("xcl", "xcs", "xmw", "xmh", "xgw", "xgh", "xgb","xc", "xm", 
                  "xcmw", "xcm", "xg", "xcmgw", "xcmg","xpcan", "xpmid", 
                  "xpgveg", "xpcm", "xpmg", "xpcmg", "xpmgw")

  vcmat     <- as.matrix(vc[mean.mets])
  mean.mets <- rowmean(vcmat, vc$uid, reorder = T, na.rm = T)
  rownames(mean.mets) <- sort(unique(vc$uid))
  mean.mets   <- meltMetric(mean.mets)
  
  pmid <- prop.table(table(uid = vc$uid, metric = vc$R_UNDERVEG), 1)
  pcan <- prop.table(table(uid = vc$uid, metric = vc$R_CANVEG), 1)
  pmid <- as.data.frame(pmid, responseName = 'result')
  pcan <- as.data.frame(pcan, responseName = 'result')
  pcan$metric <- paste0('pcan_', pcan$metric)
  pmid$metric <- paste0('pmid_', pmid$metric)
  
  mets <- rbindMetrics(mean.mets, pmid, pcan)
  # Fix some names; convert NaN to NA; coerce some data types
  is.na(mets$result) <- is.nan(mets$RESULT)
  progressReport("Finished with riparian vegetation metrics.")
  return(out)
}

