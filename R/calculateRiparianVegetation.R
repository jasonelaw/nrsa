formatRiparianVegetation <- function(uid, transect, transdir, parameter, result){
  visrip <- data.frame(UID = uid, TRANSECT = transect, TRANSDIR = transdir, 
                       PARAMETER = parameter, RESULT = result)
  # Vegetative cover pars; names used to rename later
  veg.cov <- c(xcl = 'CANBTRE', xcs = 'CANSTRE', xmw = 'UNDWDY', 
               xmh = 'UNDNWDY', xgw = 'GCWDY', xgh = 'GCNWDY',
               xgb = 'BARE')  
  # Parameters needed for presence calculations
  pres <- c("CANBTRE", "CANSTRE", "UNDWDY", "UNDNWDY", "GCWDY", "GCNWDY")
  
  # Create necessary presence (P_) and result (R_) variables
  visrip$PRESENCE <- visrip$RESULT != '0'
  names(visrip) <- replace2(names(visrip), c('RESULT', 'PRESENCE'), c('R', 'P'))
  vm <- melt(visrip, measure.var = c('R', 'P'))
  sub.expr <- .((PARAMETER %in% c("UNDERVEG", "CANVEG", "CANBTRE", "CANSTRE", 
                                  "UNDWDY", "UNDNWDY", "GCWDY", "GCNWDY", "BARE") &
                   variable == 'R') |
                  (PARAMETER %in% c("CANBTRE", "CANSTRE", "UNDWDY",
                                    "UNDNWDY", "GCWDY", "GCNWDY") & 
                     variable == 'P'))
  vc <- dcast(vm, UID + TRANSECT + TRANSDIR ~ variable + PARAMETER, 
              subset = sub.expr)
  # Ensure that all the levels are present; in case some weren't used in the data.
  vc$R_UNDERVEG <- as.factor(vc$R_UNDERVEG)
  vc$R_CANVEG <- as.factor(vc$R_CANVEG)
  levels(vc$R_UNDERVEG) <- list(c = 'C', d = 'D', e = 'E', m = 'M', n = 'N')
  levels(vc$R_CANVEG)   <- list(c = 'C', d = 'D', e = 'E', m = 'M', n = 'N')
  # Vegetation class area cover characterizations -- individual classes
  # Use arithmetic means of end points to numerically characterize each cover
  # class.  This section just converts the 0:5 to the end points.
  veg.cov <- replace(veg.cov, seq_along(veg.cov), paste('R', veg.cov, sep = '_'))
  pres <- paste('P', pres, sep = '_')
  vc[veg.cov] <- lapply(vc[veg.cov], function(x){
    x <- as.factor(x)
    x <- revalue(x, c('0' = 0, '1' = 0.05, '2' = 0.25, '3' = 0.575, '4' = 0.875))
    FactorToNumeric(x)
  })
  vc[pres] <- lapply(vc[pres], CharacterToLogical)
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
#'@importFrom NARSShared FactorToNumeric CharacterToLogical rowSumsProtectNA quick.table '%&%'
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
  names(vc) <- replace2(names(vc), veg.cov, names(veg.cov))
  # Calculate all of the metrics
  calc <- function(rows){
    c(pmid = quick.table(vc$R_UNDERVEG[rows]),
      pcan = quick.table(vc$R_CANVEG[rows]),
      # calculate mean for vegetative cover and combinations of vegetative cover
      colMeans(vcmat[rows,], na.rm = T)
      )
  }
  # List of vegetative cover and combo metrics that need means calculated.
  mean.mets <- c("xcl", "xcs", "xmw", "xmh", "xgw", "xgh", "xgb","xc", "xm", 
                 "xcmw", "xcm", "xg", "xcmgw", "xcmg","xpcan", "xpmid", 
                 "xpgveg", "xpcm", "xpmg", "xpcmg", "xpmgw")
  # The next 6 lines do the same thing as the commented ddply(vc, .(UID), calc)
  # line below, but they do it faster because a copy is not made of vc.  I
  # converted the numeric columns to a matrix because the matrix subscripting is
  # faster than dataframe subscripting. Cuts calculation time by ~ 40%
  ids     <- id(vc[c('UID')], drop = TRUE)
  vcmat   <- as.matrix(vc[mean.mets])
  indices <- plyr:::split_indices(ids, n = attr(ids, 'n'))
  out     <- vapply(indices, calc, numeric(31))
  labels  <- split_labels(vc[c('UID')], drop = F, id = ids)
  out <- cbind(labels, t(out))
  # Uncomment the following lines (and comment lines 73-93 above) to try the plyr version
#  calc <- function(x){
#    mean.mets <- c("xcl", "xcs", "xmw", "xmh", "xgw", "xgh", "xgb","xc", "xm", 
#        "xcmw", "xcm", "xg", "xcmgw", "xcmg","xpcan", "xpmid", 
#        "xpgveg", "xpcm", "xpmg", "xpcmg", "xpmgw")
#    c(pmid = prop.table(table(x$R_UNDERVEG)),
#        pcan = prop.table(table(x$R_CANVEG)),
#        # calculate mean for vegetative cover and combinations of vegetative cover
#        colMeans(x[,mean.mets], na.rm = T)
#    )
#  }
#  out <- ddply(vc, .(UID), calc)
  # reshape data back to long format
  out <- melt(data = out, id.var = 'UID', variable.name = 'METRIC', 
              value.name = 'RESULT')
  # Fix some names; convert NaN to NA; coerce some data types
  levels(out$METRIC) <- gsub('.', '_', tolower(levels(out$METRIC)), fixed = T)
  is.na(out$RESULT) <- is.nan(out$RESULT)
  out$RESULT <- as.character(out$RESULT)
  out$METRIC <- as.factor(as.character(out$METRIC))
  progressReport("Finished with riparian vegetation metrics.")
  return(out)
}

