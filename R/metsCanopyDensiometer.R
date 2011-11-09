# Author: JLAW
# Date: Dec 2, 2010
# Purpose: Refactor metsCanopyDensiometer.1 to show how plyr and reshape2 can
# be used to simplify a lot of the split-calculate-reshape code in the original
# EPA scripts.
# 
###############################################################################

# Library and source statements
#source('S:/jlaw/pawmap/source/NRSA/myVersions/JSharedCode.R')
metsCanopyDensiometer <- function()
#Calculates Canopy Densiometer metrics:
# Wadeable Protocal:
#xcdenmid xcdenbk vcdenmid vcdenbk nbnk nmid.
#
#Boatable Protocal:
#xcdenbk  vcdenbk nbnk
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Canopy Densiometer calculations', loc='start')

  intermediateMessage('.1 Read in data', loc='end')
  # read in densiometer readings from database
  dens <- odbcConnect('NRSA2')
  on.exit(metsCanopyDensiometer.cleanup(dens))
  rawdat <- fetchNRSATable(dens, 'tblCHANCOV2')

  intermediateMessage('.2 call function metsCanopyDensiometer.1', loc='end')

  # calculate the calculations
  mets <- metsCanopyDensiometer.1(rawdat)

  intermediateMessage('.3 Write results', loc='end')

  # write the results
  rc <- writeNRSACalcResults(mets, 'metsCanopyDensiometer.csv')

  intermediateMessage('  Done.', loc='end')
  return(rc)
}

# Function definitions
metsCanopyDensiometer.1 <- function(indat){
# Does all the real work for metsCanopyDensiometer.
#
# Args:
#   indat: dataframe of canopy data.
# Returns:
#   A dataframe of canopy metrics 
#
  intermediateMessage('.1.0Canopy Densiometer mets', loc='end')
  # prepare data: get DENSIOM and create mid and bank factor
  cdData <- subset(indat, PARAMETER == 'DENSIOM')
  cdData$where <- as.factor(cdData$TRANSDIR)
  levels(cdData$where) <- list('mid' = c('CU', 'CL', 'CD', 'CR'),
                               'bnk' = c('DN', 'LF', 'RT', 'UP'))
   
  # calculate metrics by UID and where
  intermediateMessage('.1.1 split data and calculate metrics', loc='end')
  calc <- function(x){
    pct <- x$RESULT / 17 * 100
    c(vcden = sd(pct, na.rm=T), 
      xcden = mean(pct, na.rm=T), 
      n     = count(x$RESULT))
  }
  mets <- ddply(cdData, .(UID, where), calc)
  mets <- melt(mets, id.var = c('UID', 'where'), value.name = 'RESULT')
  
  # clean up to make it look pretty
  mets$METRIC <- as.factor(paste(mets$variable, mets$where, sep = ''))
  mets$UID <- as.factor(mets$UID)
  mets <- subset(mets, select = c('UID', 'METRIC', 'RESULT'))
  #mets <- allFacToChar(mets)
  is.na(mets$RESULT) <- is.nan(mets$RESULT)
  intermediateMessage('.1.2 Done with function metsCanopyDensiometer.1 ', loc='end')
  return(arrange(mets, UID, METRIC))
}

