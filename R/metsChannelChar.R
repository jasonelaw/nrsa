# Author: JLAW
# Date: Dec 1, 2010
# Purpose: Refactor metsChannelChar.1 to show how plyr and reshape2 can
# be used to simplify a lot of the split-calculate-reshape code in the original
# EPA scripts.
# 
###############################################################################

# Library and source statements
#source('S:/jlaw/pawmap/source/NRSA/myVersions/JSharedCode.R')
require(RODBC)
require(RUnit)

metsChannelChar <- function()
# Calculates Channel Characteristic metrics:
# xshor2vg, mxshor, mnshor, pct_ovrb, pctch_b, pctch_c, pctch_n, pctch_u and
# conbankfull, confeatures, conpattern, conpercent, constraint, convalley,
# convalleybox.
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Channel Characteristic calculations', loc='start')
  intermediateMessage('.1 Read in data', loc='end')
  on.exit(odbcClose(con))
  # read in Channel Characteristic data from database
  con <- odbcConnect('NRSA2')
  bg <- fetchNRSATable(con, 'tblBankGeometry2')
  cc <- fetchNRSATable(con, 'tblChannelChar2')

  intermediateMessage('.2 call function metsChannelChar.1', loc='end')

  # calculate the calculations
  mets <- metsChannelChar.1(bg, cc)

  intermediateMessage('.3 Write results', loc='end')

  # write the results
  rc <- writeNRSACalcResults(mets, 'metsChannelChar.csv')
  
  intermediateMessage('  Done.', loc='end')

  return(rc)
}
# Function definitions
metsChannelChar.1 <- function(indat, chanCon){
  # Does all the real work for metsChannelChar.
  #
  # Args:
  #   indat: dataframe of bank geometry data taken at each transect.
  #   chanCon: dataframe with channel characteristics data taken for entire site.
  #
  # Returns:
  #   Returns a dataframe of calculations if successful.
  # Make the data wide
  mindat <- melt(indat, measure.var = 'RESULT')
  mindat$value <- as.character(mindat$value)
  cindat <- dcast(mindat, UID + TRANSECT ~ PARAMETER, 
                  subset = .(PARAMETER %in% c('CONSTRT', 'SHOR2RIP', 'SEEOVRBK')))
  cindat$SHOR2RIP <- FactorToNumeric(cindat$SHOR2RIP)
  levels(cindat$CONSTRT) <- list('pctch_b' = 'B', 'pctch_c' = 'C', 
                                 'pctch_n' = 'N', 'pctch_u' = 'U')
  # Calculate all of the metrics
  calc <- function(x){
    c(prop.table(table(x$CONSTRT)) * 100,
      "pct_ovrb" = sum(x$SEEOVRBK == 'YES', na.rm=T) / count(x$SEEOVRBK) * 100,
      "xshor2vg" = mean(x$SHOR2RIP, na.rm = T),
      "mxshor"   = max(x$SHOR2RIP, na.rm = T),
      "mnshor"   = min(x$SHOR2RIP, na.rm = T))
  }
  out    <- ddply(cindat, .(UID), calc)
  # Transform from wide to long metrics
  out    <- melt(out, id.var = 'UID', variable.name = 'METRIC',
                 value.name = 'RESULT')
  # Get channel constraint metrics and rename them           
  ccMets <- rename(chanCon[c('UID','PARAMETER','RESULT')], 'PARAMETER', 'METRIC')
  ccMets$METRIC <- 
    factor(x      = ccMets$METRIC, 
           levels = c("BANKFULL", "CONSTRNT", "FEATURES", "PATTERN", 
                      "PERCENT", "VALLEY", "VALLYBOX"),
           labels = c("conbankfull", "constraint", "confeatures", "conpattern",
                      "conpercent", "convalley", "convalleybox"))
  ccMets$METRIC <- as.character(ccMets$METRIC)
  ccMets$METRIC[is.na(ccMets$METRIC)] <- 'UNKNOWN!!'
  mets <- rbind(out, ccMets)
  return(mets)
}

