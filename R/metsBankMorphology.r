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
  protocols <- siteProtocol (unique(df1$UID))

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

metsBankMorphology.1 <- function (df1, protocols)

#Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.

#ARGUMENTS:
#df1  dataframe of the bankmorphology data.
#protocols   dataframe relating UID to the sampling protocol used at that site

{

  intermediateMessage('BankMorphology mets', loc='start')
  #initialize datasets for final rbind
  boatmets<-NULL
  stream<-NULL

  #start with splitting up streams and rivers.

  mm <- merge(df1, protocols, by='UID', all.x=TRUE, all.y=FALSE)
   
  mmstr <- subset (mm, mm$PROTOCOL=='WADEABLE')


  #angle (must reshape to do these calculations)

  if (nrow(mmstr)>0) {
    ang <- subset (mmstr, mmstr$PARAMETER=='ANGLE')
    ang$RESULT <- as.numeric(ang$RESULT)  ## NAs by coersion

    ct <- aggregate(ang$RESULT
                       ,list('UID'=ang$UID
                       )
                       ,count
                       )
                       
    ct$METRIC <- 'n_ba'
    ct <- rename(ct, 'x', 'RESULT')

    mn <- aggregate(ang$RESULT
                       ,list('UID'=ang$UID
                       )
                       ,mean , na.rm=TRUE
                       )
    mn$METRIC <- 'xbka'
    mn <- rename(mn, 'x', 'RESULT')

    stt <- aggregate(ang$RESULT
                       ,list('UID'=ang$UID
                       )
                       ,sd  , na.rm=TRUE
                       )
    stt$METRIC <- 'sdbk_a'
    stt <- rename(stt, 'x', 'RESULT')
                       
    iqq <- aggregate(ang$RESULT
                       ,list('UID'=ang$UID
                       )
                       ,iqr
                       )
    iqq$METRIC <- 'intqbka'
    iqq <- rename(iqq, 'x', 'RESULT')

    med <- aggregate(ang$RESULT
                       ,list('UID'=ang$UID
                       )
                       ,median , na.rm=TRUE
                       )
                       
    med$METRIC <- 'medbk_a'
    med <- rename(med,'x', 'RESULT')
    
    upq1 <- aggregate(ang$RESULT
                       ,list ('UID'=ang$UID
                       )
                       ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                       )
                       
    upq1$METRIC <- 'bka_q1'
    upq1 <- rename(upq1, 'x', 'RESULT')

    upq3 <- aggregate(ang$RESULT
                     ,list ('UID'=ang$UID)
                     ,quantile, .75, na.rm=TRUE, names=FALSE, type=2
                     )
    upq3$METRIC <- 'bka_q3'
    upq3 <- rename(upq3, 'x', 'RESULT')

    #put these together

    bkangle <- rbind (ct, mn, stt, iqq, med, upq1, upq3)
    intermediateMessage('.1')

    #do this same step for undercut (streams)

    unct <- subset (mmstr, mmstr$PARAMETER=='UNDERCUT')
    unct$RESULT <- as.numeric(unct$RESULT)

    ct <- aggregate(unct$RESULT
                       ,list('UID'=unct$UID
                       )
                       ,count
                       )

    ct$METRIC <- 'n_un'
    ct <- rename(ct, 'x', 'RESULT')

    mn <- aggregate(unct$RESULT
                       ,list('UID'=unct$UID
                       )
                       ,mean , na.rm=TRUE
                       )
    mn$METRIC <- 'xun'
    mn <- rename(mn,'x', 'RESULT')
    
    
    
    

    stt <- aggregate(unct$RESULT
                       ,list('UID'=unct$UID
                       )
                       ,sd , na.rm=TRUE
                       )
    stt$METRIC <- 'sdun'
    stt <- rename(stt, 'x', 'RESULT')

    iqq <- aggregate(unct$RESULT
                       ,list('UID'=unct$UID
                       )
                       ,iqr
                       )
    iqq$METRIC <- 'intqbkun'
    iqq <- rename(iqq, 'x', 'RESULT')

    med <- aggregate(unct$RESULT
                       ,list('UID'=unct$UID
                       )
                       ,median , na.rm=TRUE
                       )

    med$METRIC <- 'medbkun'
    med <- rename(med, 'x', 'RESULT')

    upq1 <- aggregate(unct$RESULT
                       ,list ('UID'=unct$UID
                       )
                       ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                       )

    upq1$METRIC <- 'bkun_q1'
    upq1 <- rename(upq1, 'x', 'RESULT')

    upq3 <- aggregate(unct$RESULT
                       ,list ('UID'=unct$UID
                       )
                       ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                       )
    upq3$METRIC <- 'bkun_q3'
    upq3 <- rename(upq3, 'x', 'RESULT')

    #put these together

    undercut <- rbind (ct, mn, stt, iqq, med, upq1, upq3)
    intermediateMessage('.2')


    #put together undercut and angle

    stream <- rbind(bkangle, undercut)
    intermediateMessage('.3')
  }
#   iq4step  <- quantile(ang$RESULT, type=2, na.rm=TRUE, names=FALSE)
   
#   mmstrw <- reshape(mmstr, idvar='UID', direction='wide', timevar='METRIC')
#  names(mmstr) <- gsub('RESULT\\.', '', names(mmstr))


#Rivers (bank angle and other mets.)

#bang_mode, bap_low, bap_med, bap_mis, bap_stp, bap_vst, n_ba, n_w

  mmboat <- subset (mm, mm$PROTOCOL=='BOATABLE')
  
  angb <- subset (mmboat, mmboat$PARAMETER=='ANGLE')

  if(nrow(angb)>0) {
    ct <- aggregate(angb$RESULT
                   ,list('UID'=angb$UID)
                   ,count
                   )
    ct$METRIC <- 'n_ba'
    ct <- rename(ct, 'x', 'n_ba')
    intermediateMessage('.4')


    #use the total ang count to get other counts

    bap_low <- subset (mmboat, mmboat$PARAMETER=='ANGLE' )
    bap_low <- aggregate(bap_low$RESULT
                        ,list('UID'=bap_low$UID)
                        ,function (RESULT) {sum(RESULT=='0-5')}
                        )
    bap_low <- rename(bap_low, 'x', 'low')
    
    
    bb <- merge(ct, bap_low,  by='UID', all.x=TRUE, all.y=FALSE)
    intermediateMessage('.5')


    bb$RESULT <- (bb$low/bb$n_ba) * 100
    bb$low <- NULL
    bb$n_ba <- NULL
    bb$METRIC <- NULL
    bb$METRIC <- 'bap_low'

    bap_med <- subset (mmboat, mmboat$PARAMETER=='ANGLE')
    bap_med <- aggregate(bap_med$RESULT
                        ,list('UID'=bap_med$UID)
                        ,function (RESULT) {sum(RESULT=='5-30')}
                        )
    bap_med <- rename(bap_med, 'x', 'med')
    bb1 <- merge(ct, bap_med,  by='UID', all.x=TRUE, all.y=FALSE)
    
    intermediateMessage('.6')

    if(nrow(bb1)>0) {
      bb1$RESULT <- (bb1$med/bb1$n_ba) *100
      bb1$med <- NULL
      bb1$n_ba <- NULL
      bb1$METRIC <- NULL
      bb1$METRIC <- 'bap_med'
    }
 
    bap_stp <- subset (mmboat, mmboat$PARAMETER=='ANGLE' )
    bap_stp <- aggregate(bap_stp$RESULT
                        ,list('UID'=bap_stp$UID)
                        , function (RESULT) {sum(RESULT=='30-75')}
                        )
    bap_stp <- rename(bap_stp, 'x', 'stp')
      
    bb2 <- merge(ct, bap_stp,  by='UID', all.x=TRUE, all.y=FALSE)
    
    if(nrow(bb2)>0) {
      bb2$RESULT <- (bb2$stp/bb2$n_ba) *100
      bb2$stp <- NULL
      bb2$n_ba <- NULL
      bb2$METRIC <- NULL
      bb2$METRIC <- 'bap_stp'
    }
 
    bap_vst <- subset (mmboat, mmboat$PARAMETER=='ANGLE' )
    bap_vst <- aggregate(bap_vst$RESULT
                        ,list('UID'=bap_vst$UID)
                        ,function (RESULT) {sum(RESULT=='75-100')}
                        )
    bap_vst<- rename(bap_vst, 'x', 'vst')
    bb3 <- merge(ct, bap_vst,  by='UID', all.x=TRUE, all.y=FALSE)
    
   intermediateMessage('.7')

   if(nrow(bb3)>0) {
     bb3$RESULT <- (bb3$vst/bb3$n_ba) *100
     bb3$vst <- NULL
     bb3$n_ba <- NULL
     bb3$METRIC <- NULL
     bb3$METRIC <- 'bap_vst'
   }
 
  # Configuring ct for rbind
  ct <- rename(ct,'n_ba','RESULT')
  ct <- subset(ct, select=c('UID','METRIC','RESULT'))

  intermediateMessage('.8')

  bb4 <- rbind (bb, bb1,bb2,bb3,ct)
    
#    bb4 <- merge(bb, bap_med,  by='UID', all.x=TRUE, all.y=FALSE)
#    bb2 <- merge(bb1, bap_stp,  by='UID', all.x=TRUE, all.y=FALSE)
#    bb3 <- merge(bb2, bap_vst,  by='UID', all.x=TRUE, all.y=FALSE)
   

#calculate the 'n' for wetted width  
    
  angb <- subset (mmboat, mmboat$PARAMETER=='WETWID')
  angb$RESULT <- as.numeric(angb$RESULT)

  ww <- aggregate(angb$RESULT
                 ,list('UID'=angb$UID)
                 ,count
                 )
                       
  ww$METRIC <- 'n_w'
  ww <- rename(ww, 'x', 'RESULT')
    
  boat <- rbind (bb4, ww)
    
  
  intermediateMessage('.9')

  #work on the BANGMODE code for rivers.
  bang <- subset(df1
                ,PARAMETER=='ANGLE' &
                 RESULT %in% c('0-5','5-30','30-75','75-100')
                )

  tt <- aggregate(bang$RESULT=='0-5'
                 ,list('UID'=bang$UID)
                 ,mean, na.rm=TRUE
                 )
  lowbap <- rename(tt, 'x', 'xlow')

  tt <- aggregate(bang$RESULT=='5-30'
                 ,list('UID'=bang$UID)
                 ,mean, na.rm=TRUE
                 )
  medbap <- rename(tt, 'x', 'xmed')

  tt <- aggregate(bang$RESULT=='30-75'
                 ,list('UID'=bang$UID)
                 ,mean, na.rm=TRUE
                 )
  stpbap <- rename(tt, 'x', 'xstp')

  tt <- aggregate(bang$RESULT=='75-100'
                 ,list('UID'=bang$UID)
                 ,mean, na.rm=TRUE
                 )
  vstbap <- rename(tt, 'x', 'xvst')

 
  intermediateMessage('.10')
  
  # Determine color mode (most common bank angle)
  fracbangmode<-merge(lowbap, medbap
                   ,by='UID'
                   ,all=TRUE
                   ,sort=FALSE
                   )
  fracbangmode<-merge(fracbangmode, stpbap
                   ,by='UID'
                   ,all=TRUE
                   ,sort=FALSE
                   )
  fracbangmode<-merge(fracbangmode, vstbap
                   ,by='UID'
                   ,all=TRUE
                   ,sort=FALSE
                   )

  intermediateMessage('.11')


  modebang <- subset(fracbangmode, select='UID')
  modebang$bsobang <- NA
  for(i in 1:nrow(modebang)) {
      modebang$bsobang[i] <- modalClass(fracbangmode[i,]
                                         ,c('xlow','xmed','xstp','xvst'
                                           )
                                         ,c('0-5','5-30','30-75','75-100'
                                         )
                                         )
  }

  intermediateMessage('.12')

  modebang$METRIC <- 'bangmode'
  modebang <- rename(modebang, 'bsobang', 'RESULT')
    
  modebang$RESULT <- ifelse(modebang$RESULT =='0-5', 'low', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='5-30', 'med', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='30-75', 'stp', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='75-100', 'vst', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='0-5, 5-30', 'low-med', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='5-30, 30-75', 'med-stp', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT =='30-75, 75-100', 'stp-vst', modebang$RESULT)
  modebang$RESULT <- ifelse(modebang$RESULT %in% c('low', 'med','stp', 'vst'
                                                  ,'low-med','med-stp', 'stp-vst'
                                                  )
                           ,modebang$RESULT, 'None'
                           )

  boatmets <- rbind (modebang, boat)
 
}

 metsfakeo <- rbind (boatmets, stream)
 
  intermediateMessage('  Done.', loc='end')
  return(metsfakeo)

  }
  




 #test of metsBankMorphology function

