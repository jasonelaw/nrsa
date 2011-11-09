# metsFishCover.r
#
# Calculates Fish Cover portion of the Physical Habitat metrics from
# validated NRSA data.
#
# 12/15/2009  mrc copied fcMets from NLA project

# 12/18/2009  mrc completed creating mets, now write tests
# 12/30/2009  tests complete
# 03/23/2010  cws moved creation of unit test dataframes to separate functions.
# 04/01/2010  ssr created only-boatable and only-wadeable tests 

#wrapper function

metsFishCover <- function ()

# calculate fish cover metrics and saves results to a file.
# Wadeable and Boatable protocol generate the same results:
#  Metrics: pfc_alg, pfc_rck, pfc_brs, pfc_lvt, pfc_aqm, pfc_ohv, pfc_hum, pfc_ucb, pfc_lwd 
#           xfc_alg, xfc_rck, xfc_brs, xfc_lvt, xfc_aqm, xfc_ohv, xfc_hum, xfc_ucb, xfc_lwd 
#           pfc_all, pfc_big, pfc_nat
#           xfc_all, xfc_big, xfc_nat
#           sdfc_ucb, sdfc_ohv
#           idrucb, idrohv, iqrucb, iqrohv
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Fish Cover calculations', loc='start')
  on.exit(odbcClose(chan))
  #read in the data from fishcover2
  
  chan <- odbcConnect ('NRSA2')
  fishcover <- fetchNRSATable (chan, 'tblFISHCOVER2')

  intermediateMessage ('fetch_data.1')
  #determine protocol used for each site
  protocols <- siteProtocol (unique(fishcover$UID))
  
  intermediateMessage ('set_protocols.2')
  
  #calculate the metrics
  mets <- metsFishCover.1 (fishcover, protocols)
   if(is.character(mets)) return (mets)
   
  #write the results
  rc <- writeNRSACalcResults (mets, 'metsFishCover.csv')
  intermediateMessage ( ' Done.', loc='end')
  
  return (rc)
}
  
  
  
metsFishCover.1 <- function (fishcover, protocols)
 
#Returns a dataframe of calculations if successful or a character string 
#describing the problem if one was encountered.
 
#ARGUMENTS:
#fishcover   dataframe of the fishcover data.
#protocols   dataframe relating UID to the sampling protocol used at that site
 
#Did the protocols for fun, but the mets for WADEABLE and BOATABLE are the same for fish cover
 
# ASSUMPTIONS:
# The parameter vector has the following values: ALGAE, BOULDR,
# BRUSH, LVTREE, MACPHY, OVRHNG, STRUCT, UNDCUT (Wadeable), UNDERCUT (Boatable), WOODY
# change the value of UNDCUT to UNDERCUT to make them compatible
{
  intermediateMessage('Fish Cover mets', loc='start')
  # Constants used
  cover <- c('0' = 0, '1' = 0.05, '2' = 0.25, '3' = 0.575, '4' = 0.875)
  isNatural <- c('rck', 'brs', 'lvt', 'ohv', 'ucb', 'lwd')
  isBig <- c('rck', 'hum', 'ucb', 'lwd')
  AcceptableParameters <- c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE', 'MACPHY', 
                            'OVRHNG', 'STRUCT', 'UNDCUT', 'UNDERCUT', 'WOODY')
  ParameterMetricMap <- list('alg' ='ALGAE','rck' = 'BOULDR', 'brs' = 'BRUSH', 
                             'lvt' = 'LVTREE', 'aqm' = 'MACPHY','ohv' = 'OVRHNG',
                             'hum' = 'STRUCT','ucb' = c('UNDCUT', 'UNDERCUT'),
                             'lwd' = 'WOODY')
  
  # Calculation functions
  mean.calc <- function(i){
    ans <- c(xfc = mean(fc.mat[i, 'cover'], na.rm = T), 
             pfc = mean(fc.mat[i, 'presence'], na.rm = T))
    return(ans)
  }
  sum.calc <- function(x){
    c(pfc_all = sum(x$pfc, na.rm = T),
      pfc_big = sum(x$pfc[x$isBig], na.rm = T),
      pfc_nat = sum(x$pfc[x$isNatural], na.rm = T),
      xfc_all = sum(x$xfc, na.rm = T),
      xfc_big = sum(x$xfc[x$isBig], na.rm = T),
      xfc_nat = sum(x$xfc[x$isNatural], na.rm = T))
  }
  spread.calc <- function(i){
    c(sdfc = sd(bc.mat[i, 'cover'], na.rm = T), 
      idr = idr(bc.mat[i, 'cover']), 
      iqr = iqr(bc.mat[i, 'cover']))
  }
  match.ids <- function(ids){
    n <- attr(ids, 'n')
    match(seq_len(n), ids)
  }
  
  # Modigy data to create calculation values from field values 
  # Add additional attributes needed
  fcData <- subset(fishcover, PARAMETER %in% AcceptableParameters)
  fcData$PARAMETER <- as.factor(fcData$PARAMETER)
  levels(fcData$PARAMETER) <- ParameterMetricMap
  fcData$cover <- cover[as.character(fcData$RESULT)]
  fcData$presence <- fcData$RESULT %in% 1:4
  fcData$isBig <- fcData$PARAMETER %in% isBig
  fcData$isNatural <- fcData$PARAMETER %in% isNatural
  fcData$isBankCover <- fcData$PARAMETER %in% c('ohv', 'ucb')
  # Create Bank Cover dataset for spread metrics
  bcData <- subset(fcData, isBankCover)
  
  # Set up data matrices, id variables, id lists for vapply functions
  # This is purely for speed on large data sets and could be replaced by very simple ddply calls
  fc.ids <- id(fcData[c("UID", "PARAMETER", "isBig", "isNatural")], drop = TRUE)
  fc.mat <- as.matrix(fcData[,c("cover", "presence")])
  fc.indices <- plyr:::split_indices(seq_len(nrow(fc.mat)), fc.ids, n = attr(fc.ids, "n"))
  fc.labels <- fcData[match.ids(fc.ids), c("UID", "PARAMETER", "isBig", "isNatural")]
  bc.ids <- id(bcData[c("UID", "PARAMETER")], drop = TRUE)
  bc.mat <- as.matrix(bcData[,c("cover", "presence")])
  bc.indices <- plyr:::split_indices(seq_len(nrow(bc.mat)), bc.ids, n = attr(bc.ids, "n"))
  bc.labels <- bcData[match.ids(bc.ids), c("UID", "PARAMETER")]
  intermediateMessage('create tables used in calculations.1', loc='start')
  
  # Do Calculations
  class.values <- vapply(fc.indices, mean.calc, numeric(2))
  class.mets <- cbind(fc.labels, t(class.values))
  intermediateMessage('complete Mean Presence mets (fp).2')
  intermediateMessage('Complete mean value mets (xp).3')
  index.mets <- ddply(class.mets, .(UID), sum.calc)
  intermediateMessage('complete type metrics.4', loc='start')
  spread.values <- vapply(bc.indices, spread.calc, numeric(3))
  spread.mets <- cbind(bc.labels, t(spread.values))
  intermediateMessage('complete sd calculations.5', loc='start')
  intermediateMessage('complete id and iq calculations.6', loc='start')

  # Melt to long format and remove extraneous variables
  index.mets <- melt(index.mets, id.var = c('UID'), 
                     variable.name = 'METRIC', 
                     value.name = 'RESULT')
  spread.mets <- melt(spread.mets, id.var = c('UID', 'PARAMETER'), 
                      variable.name = 'METRIC', 
                      value.name = 'RESULT')
  class.mets$isBig <- NULL
  class.mets$isNatural <- NULL
  class.mets <- melt(class.mets, id.var = c('UID', 'PARAMETER'), 
                     variable.name = 'METRIC', 
                     value.name = 'RESULT')
    
  # Fix variable names to match EPA original names
  spread.mets$METRIC <- paste(spread.mets$METRIC, spread.mets$PARAMETER, sep = '_')
  spread.mets$PARAMETER <- NULL
  class.mets$METRIC <- paste(class.mets$METRIC, class.mets$PARAMETER, sep = '_')
  class.mets$PARAMETER <- NULL
  mets <- rbind(class.mets, index.mets, spread.mets)
  mets$METRIC <- ifelse(grepl('idr|iqr', mets$METRIC), 
                        gsub('_', '', mets$METRIC), 
                        mets$METRIC)
  intermediateMessage(' Done.', loc='end')
  return(mets)
}


