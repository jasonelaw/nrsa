# metsBankMorphology.r
#
# Calculates Bank Morphology portion of the Physical Habitat metrics from
# validated NRSA data (tblBANKGEOMETRY2).
#
# 01/21/2010  mrc started
# 01/25/2010 mrc, metrics complete, write tests
# 03/16/2010 mrc, replace external text files with openTextConnection instructions
# 03/17/2010 mrc, replace upData calls with rename
# 03/18/2010 ssr, added boatable n_ba to data for export
# 03/22/10 ssr moved creation of unit test dataframes to separate functions.
#  3/25/10 cws Changed diff() calls to dfCompare().


#wrapper function

metsBankMorphology <- function ()

# calculate bank morphology  metrics and saves results to a file.

#  Wadeable metrics:  bka_q1, bka_q3, bkun_q1, bkun_q3, intqbka, intqbkun, medbkun, medbk_a
#  ,n_ba, n_un, sdbk_a, sdun, xbka, sdun, sbka, xun
#
#  Boatable metrics: bangmode, bap_low, bap_med, bap_mis
#  ,bap_stp, bap_vst, n_ba, n_w

#NOTE:  Removed undercut mets from calculations for RIVER data.
#   10/25/02 cws River bank angle mets changed.  Calculation of mean, median,
#            and quartile values based on arithmetic midpoints of bins is
#            arguably silly, and are replaced by percent of reach in each
#            category and the distributional mode as described by Zar
#            (2ed), p 23.  This makes the calculations completely different
#            for rivers vs. streams.

# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Bank Morphology calculations', loc='start')

  #read in the data from tblBANKGEOMETRY2

  chan <- odbcConnect ('NRSA2')
  df1 <- fetchNRSATable (chan, 'tblBANKGEOMETRY2')
  close(chan)
  intermediateMessage ('fetch_data.1', loc='start')


  #determine protocol used for each site
  protocols <- siteProtocol(unique(df1$UID))

  intermediateMessage ('set_protocols.2', loc='start')

    #calculate the metrics
  mets <- metsBankMorphology.1 (df1, protocols)
   if(is.character(mets)) return (mets)

  #write the results
  rc <- writeNRSACalcResults(mets, 'metsBankMorphology.csv')

  intermediateMessage ( ' Done.', loc='end')

  return (rc)
}


#sample_type PHAB_CHANW ..................wadeable
#sample_type CHANBANKB, CHANBFRONT........boatable

CalcWadeBankMets <- function(x){
  n_ba <- count(x$ANGLE)
  angle.mets <- summary.nrsa(x$ANGLE, probs = c(0.25, 0.5, .75), na.rm = T)
  n_un <- count(x$UNDERCUT)
  under.mets <- summary.nrsa(x$UNDERCUT, probs = c(0.25, 0.5, .75), na.rm = T)
  mets <- c(n_ba, angle.mets, n_un, under.mets)
  names(mets) <- c('n_ba', 'bka_q1','medbk_a', 'bka_q3', 'xbka', 'sdbk_a', 
                   'intqbka', 'n_un', 'bkun_q1', 'medbkun', 'bkun_q3',  'xun', 
                   'sdun', 'intqbkun')
  mets
}

CalcBoatNoWettedWidth <- function(uid, wetwid){
  ww <- aggregate(wetwid, list('UID' = uid), count)
  ww$METRIC <- 'n_w'
  names(ww)[2] <- 'RESULT'
  ww <- subset(ww, select = c('UID', 'METRIC', 'RESULT'))
  ww
}

CalcBoatAngleMets <- function(uid, angle){
  ang.counts <- table(UID = uid, METRIC = angle)
  ang.prop <- prop.table(ang.counts, 1) * 100
  ang.counts <- addmargins(ang.counts, margin = 2)
  
  # reformat results
  boatmets1 <- as.data.frame(ang.prop, responseName = 'RESULT')
  boatmets2 <- as.data.frame(ang.counts, responseName = 'RESULT')
  boatmets2 <- subset(boatmets2, METRIC == 'Sum')
  boatmets <- rbind(boatmets1, boatmets2)
  levels(boatmets$METRIC) <- list(bap_low = '0-5', bap_med = '5-30', 
                                  bap_stp = '30-75', bap_vstp = '75-100', 
                                  n_ba = 'Sum')
  
  # Modal bank angle
  ang.prop.list <- as.data.frame(unclass(ang.prop))
  bangmode <- modalClass2(ang.prop.list)
  bangmode <- data.frame(UID = dimnames(ang.prop)[[1]], METRIC = 'bangmode', RESULT = bangmode)
  bangmode$RESULT <- as.factor(bangmode$RESULT)
  levels(bangmode$RESULT) <- list('low' = '0-5', 'med' = '5-30', 'stp' = '30-75',
                                  'vst' = '75-100', 'low-med' = '0-5, 5-30', 
                                  'med-stp' = '5-30, 30-75', 
                                  'stp-vst' = '30-75, 75-100')
  bangmode$RESULT <- addNA2(bangmode$RESULT, 'None')
  rbind(boatmets, bangmode)
}


metsBankMorphology.1 <- function (df1, protocols)

#Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.

#ARGUMENTS:
#df1  dataframe of the bankmorphology data.
#protocols   dataframe relating UID to the sampling protocol used at that site

{
  #  
  intermediateMessage('BankMorphology mets', loc='start')
  
  #initialize datasets for final rbind
  boatmets<-NULL
  streammets<-NULL

  #start with splitting up streams and rivers.
  bank <- merge(df1, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  bank <- subset(bank, (PROTOCOL == 'WADEABLE' & 
                        PARAMETER %in% c('ANGLE', 'UNDERCUT')) | 
                       (PROTOCOL == 'BOATABLE' & 
                        PARAMETER %in% c('ANGLE', 'WETWID')))

  wade.bank <- subset(bank, PROTOCOL == 'WADEABLE')
  if (nrow(wade.bank)>0) {
    wade.bank$RESULT <- as.numeric(wade.bank$RESULT)
    wade.bank.melt <- melt(wade.bank, measure.var = 'RESULT')
    wade.bank <- dcast(wade.bank.melt, UID + TRANSECT + TRANSDIR ~ PARAMETER)
    streammets <- ddply(wade.bank, .(UID), CalcWadeBankMets)
    streammets <- melt(streammets, id.var = 'UID', variable.name = 'METRIC', value.name = 'RESULT')
    intermediateMessage('.3')
  }

#Rivers (bank angle and other mets.)

#bang_mode, bap_low, bap_med, bap_mis, bap_stp, bap_vst, n_ba, n_w

  boat.bank <- subset (bank, PROTOCOL=='BOATABLE')
  if (nrow(boat.bank) > 0){
    boat.bank.melt <- melt(boat.bank, measure.var = 'RESULT')
    boat.bank <- dcast(boat.bank.melt, UID + TRANSECT ~ PARAMETER)
    
    # convert to factor to ensure all options present in table
    boat.bank$ANGLE <- as.factor(boat.bank$ANGLE)
    levels(boat.bank$ANGLE) <- c('0-5', '5-30', '30-75', '75-100')
    
    # calculate boatable metrics
    boatmets <- CalcBoatAngleMets(boat.bank$UID, boat.bank$ANGLE)
    ww <- CalcBoatNoWettedWidth(boat.bank$UID, boat.bank$WETWID)
    boatmets <- rbind(boatmets, ww)
    boatmets$UID <- as.integer(levels(boatmets$UID))[boatmets$UID]
  }
  mets <- rbind(boatmets, streammets)
 
  intermediateMessage('  Done.', loc='end')
  return(mets)
  }


