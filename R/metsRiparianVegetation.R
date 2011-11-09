# metsRiparianVegetation.r
# Author: JLAW, cws
# Date: Dec 6, 2010
# Purpose: Rewrite of the metsRiparianVegetation.1 function
#
#  2/17/10 cws Created
#  3/22/10 cws Added missing call to checkEquals!
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
#  3/31/10 cws Added on.exit() call.
###############################################################################

require(RODBC)
require(RUnit)

metsRiparianVegetation <- function()
# Calculate riparian vegetation metrics
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success, or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Read in thalweg data and retain only required parameters
  chan <- odbcConnect('NRSA2')
  on.exit(close(chan))
  visrip <- fetchNRSATable(chan, 'tblVISRIP2')
  if(is.character(visrip)) return(visrip)
  
  visrip <- subset(visrip
                  ,PARAMETER %in% c('CANBTRE','CANSTRE','CANVEG'
                                   ,'UNDWDY','UNDNWDY','UNDERVEG'
                                   ,'GCWDY','GCNWDY','BARE'
                                   )
                  )

  # Calculate rp mets and write them to file.
  mets <- metsRiparianVegetation.1(visrip)
  if(is.character(mets)) return(mets)

  rc <- writeNRSACalcResults(mets, 'metsRiparianVegetation.csv')
  return(rc)
}

metsRiparianVegetation.1 <- function(visrip){
  # Calculates visible riparian vegetation metrics.
  #
  # Args:
  #   visrip: dataframe with visible riparian vegetation data
  #
  # Returns:
  #   A data frame of riparian vegetation metrics
  veg.cov <- c(xcl = 'CANBTRE', xcs = 'CANSTRE', xmw = 'UNDWDY', 
               xmh = 'UNDNWDY', xgw = 'GCWDY', xgh = 'GCNWDY',
               xgb = 'BARE')  # Vegetative cover pars; names used to rename later
  # Parameters needed for presence calculations
  pres <- c("CANBTRE", "CANSTRE", "UNDWDY", "UNDNWDY", "GCWDY", "GCNWDY")
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
  veg.cov <- replace(veg.cov, seq_along(veg.cov), paste('R', veg.cov, sep = '_'))
  pres <- paste('P', pres, sep = '_')
  # Ensure that all the levels are present; in case some weren't used in the data.
  levels(vc$R_UNDERVEG) <- list(c = 'C', d = 'D', e = 'E', m = 'M', n = 'N')
  levels(vc$R_CANVEG)   <- list(c = 'C', d = 'D', e = 'E', m = 'M', n = 'N')
  # Vegetation class area cover characterizations -- individual classes
  # Use arithmetic means of end points to numerically characterize each cover
  # class.  This section just converts the 0:5 to the end points.
  new.lev <- list('0' = '0', '0.05' = '1', '0.25' = '2', '0.575' = '3', '0.875' = '4')
  for (i in veg.cov){
    levels(vc[,i]) <- new.lev
    vc[,i]         <- FactorToNumeric(vc[,i])
  }
  vc[pres] <- lapply(vc[pres], CharacterToLogical)
  # Add combinations of classes as columns of visripc.  Uses a named list of
  # columns, where each element is the columns needed for each combo metric.
  vcmat <- as.matrix(vc[, c('R_CANBTRE', 'R_CANSTRE', 'R_UNDWDY', 'R_UNDNWDY', 'R_GCWDY', 'R_GCNWDY')])
  vc$xc    <- rowSumsProtectNA(vcmat[,1:2])
  vc$xm    <- rowSumsProtectNA(vcmat[,3:4])
  vc$xcmw  <- rowSumsProtectNA(vcmat[,1:3])
  vc$xcm   <- rowSumsProtectNA(vcmat[,1:4])
  vc$xg    <- rowSumsProtectNA(vcmat[,5:6])
  vc$xcmgw <- rowSumsProtectNA(vcmat[,c(1:3,5)])
  vc$xcmg  <- rowSumsProtectNA(vcmat)

  # Add presence metrics
  vc$xpcan  <- vc$P_CANBTRE | vc$P_CANSTRE
  vc$xpmid  <- vc$P_UNDWDY | vc$P_UNDNWDY
  vc$xpgveg <- vc$P_GCWDY | vc$P_GCNWDY
  vc$xpcm   <- (vc$P_CANBTRE | vc$P_CANSTRE) & (vc$P_UNDWDY | vc$P_UNDNWDY)
  vc$xpmg   <- (vc$P_UNDWDY | vc$P_UNDNWDY) & (vc$P_GCWDY | vc$P_GCNWDY)
  vc$xpcmg  <- (vc$P_CANBTRE | vc$P_CANSTRE) & (vc$P_UNDWDY | vc$P_UNDNWDY) & 
               (vc$P_GCWDY | vc$P_GCNWDY)
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
  # converted the numeric columns to a matrix because the matrix subsctipting is
  # faster than dataframe subscripting. Cuts calculation time by ~ 40%
  ids <- id(vc[c('UID')], drop = TRUE)
  vcmat <- as.matrix(vc[mean.mets])
  indices <- plyr:::split_indices(seq_len(nrow(vc)), ids, n = attr(ids, 'n'))
  out <- vapply(indices, calc, numeric(31))
  labels <- vc[match(seq_len(attr(ids, 'n')), ids), c('UID'), drop = F]
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
  return(out)
}

