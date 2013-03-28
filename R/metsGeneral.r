# metsGeneral.r
#  
#  01/04/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r,NA_filler.r and summaryby.r
#  03/22/10 cws Added all=TRUE argument to merge() of expected and actual values
#           in unit test.
#  04/02/10 cws Modified unit test and metrics code to handle data with just
#           one protocol.  Added comment about change in sidecnt value in unit
#           test.
#  06/03/10 cws Modified reachlen calculation to work with single incremnt value
#           at A 0 instead of one for each transect.
#

require(RODBC)
require(RUnit)

metsGeneral <- function()
#Calculates General metrics:
# Wadeable Protocal:
#
#
#Boatable Protocal:
#
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('General calculations', loc='start')
     
  intermediateMessage('.1 Read in data', loc='end')
  # read in data from database
  indb <- odbcConnect('NRSA2')
  on.exit(odbcClose(indb))

  rawdat <- fetchNRSATable(indb, 'tblThalweg2')
  rawdat <-rawdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]

  bdat <- fetchNRSATable(indb, 'tblChannelGeometry2')
  bdat$STATION =0
  bdat <-bdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]
  bdat <- subset(bdat,PARAMETER == 'ACTRANSP')

  vdat <- fetchNRSATable(indb, 'tblVISITS2')
  vdat$TRANSECT = 'A'
  vdat$STATION = 0
  vdat$PARAMETER = 'PROTOCOL'
  vdat$SAMPLE_TYPE = 'PHAB_THALW'
  vdat$RESULT = vdat$VALXSITE
  vdat <-vdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]

  rawdat<- rbind (rawdat,bdat,vdat)

  intermediateMessage('.2 call function metsGeneral.1', loc='end')

  # calculate the calculations
  mets <- metsGeneral.1(rawdat)
     
  intermediateMessage('.3 Write results', loc='end')
  # write the results
  rc <- writeNRSACalcResults(mets, 'metsGeneral.csv')

  intermediateMessage('  Done.', loc='end')

  return(rc)
}


metsGeneral.1 <- function(indat)
# Does all the real work for metsGeneral.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of channel data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('.1.0General mets', loc='end')
   
  intermediateMessage('.1.1 subset data and check RESULTS for wadeable and boatable', loc='end')
  cdData <- subset(indat
                  ,PARAMETER %in% c('ACTRANSP','INCREMNT','SIDCHN','OFF_CHAN'
                                   ,'REACHLENGTH','PROTOCOL'
                                   )
                  )

  # calculate PCT_SIDE (side channels) for wadeable(SIDCHN) and boatable (OFF_CHAN)
  sidec<-subset(cdData
               ,PARAMETER %in% c('SIDCHN','OFF_CHAN') &
                RESULT %in% c('Y','N',NA) &
                !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                 ,'XG','XH','XI','XJ','XK'
                                 )
                 )
               )
 
  intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')

  ps <- summaryby(sidec,'count',"pct_side")
  tyout<-aggregate( list(typesum=sidec$RESULT),list(UID=sidec$UID),function(x){sum(x=='Y',na.rm=TRUE)})
  ps<-merge(ps,tyout,by='UID',all.x=TRUE)
  ps$RESULT <- (ps$typesum/ps$RESULT)*100
  ps<-ps[c('UID','METRIC','RESULT')]

  # subset data for wadeable only and count side channel transect types (SIDECNT)
  wdata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THALW' & PARAMETER == 'INCREMNT')
  sc <- NULL
  if(nrow(wdata)>0) {
      wdata  <-unique(wdata[c('UID','TRANSECT')])
      wdata$RESULT<-ifelse(wdata$TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                                ,'XG','XH','XI','XJ','XK'
                                                )
                          ,1
                          ,0
                          )
      sc<-summaryby(wdata,'sum','sidecnt')
   }

  #calculate REACHLEN using parameter values REACHLENGTH and ACTRANSP

  incr  <- subset(cdData
                 ,SAMPLE_TYPE == 'PHAB_THALW' &
                  PARAMETER == 'INCREMNT' &
                  TRANSECT == 'A' & STATION == 0
                 ,select=c(UID,RESULT)
                 )
  rlw <- NULL
  if(nrow(incr)>0) {
      w2<- nWadeableStationsPerTransect(cdData)
      transpc <- merge(incr,w2,by=c('UID'),all.x=TRUE)
      transpc$transpc<-(as.numeric(transpc$RESULT) * transpc$nSta)
      transpc$RESULT<- ifelse(transpc$transpc<=0,NA,transpc$transpc)
      rlw <- summaryby(transpc,'sum',"reachlen")

      # need to subtract one increment from each UID, since the last transect has 1 less increment than the others.
      rlw<-merge(rlw,incr,by='UID',all.x=TRUE, suffix=c('.transpcTot','.incremnt'))
      rlw$RESULT<-rlw$RESULT.transpcTot - as.numeric(rlw$RESULT.incremnt)
      rlw<-rlw[c('UID','METRIC','RESULT')]
  }

  bdata  <- subset(cdData,PARAMETER =='ACTRANSP')
  rl <- NULL
  if(nrow(bdata)>0) {
      bdata$RESULT<-as.numeric( bdata$RESULT)
      bdata$nSta=1
      bdata$RESULT<- ifelse(bdata$RESULT<=0,NA,bdata$RESULT)
      rl <- summaryby(bdata,'sum',"reachlen")
  }

  #Determine if site SAMPLED is 'Y' or 'N' from tblVISIT2 data
  #NOTE: there is one site, UID=12475, that has a NULL value for samp$RESULT and
  #the site has been sampled.

  samp <- subset(cdData,PARAMETER == 'PROTOCOL')
  samp$RESULT <- ifelse(samp$RESULT %in% c('BOATABLE','PARBYBOAT','ALTERED',
     'INTWADE','PARBYWADE','WADEABLE'),'Y','N')
  samp$METRIC='sampled'
  samp <- unique(samp[c('UID','METRIC','RESULT')]);

  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')

  mets <- rbind(ps,sc,rlw,rl,samp)
  mets$RESULT<-ifelse(mets$RESULT=='NaN',NA,mets$RESULT)

  intermediateMessage('.1.4 Done with function metsGeneral.1 ', loc='end')
   
  return(mets)
}



