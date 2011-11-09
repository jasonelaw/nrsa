#  metsChannelHabitat.r
#  
#  01/04/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r, NA_filler.r and summaryby.r
#  03/23/10 ssr moved creation of unit test dataframes to separate functions.
# 06/03/10 cws Removing pct_sb from calculations.  Is not legal channel unit
#          code, and does not occur in the 2008-2009 field data, and it collides
#          with the substrate metric pct_sb.
#
#  

require(RODBC)
require(RUnit) 

metsChannelHabitat <- function()
  #Calculates Channel Habitat metrics:
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
      intermediateMessage('Channel Habitat calculations', loc='start')
 
      intermediateMessage('.1 Read in data', loc='end')
     # read in densiometer readings from database
        indb <- odbcConnect('NRSA2')
        rawdat <- fetchNRSATable(indb, 'tblThalweg2')
   
      intermediateMessage('.2 call function metsChannelHabitat.1', loc='end')
  
     # calculate the calculations
        mets <- metsChannelHabitat.1(rawdat)
 
      intermediateMessage('.3 Write results', loc='end')
      # write the results
        rc <- writeNRSACalcResults(mets, 'metsChannelHabitat.csv')
        on.exit(odbcClose(indb))

      intermediateMessage('  Done.', loc='end')
        return(rc)
  }


metsChannelHabitat.1 <- function(indat)
   # Does all the real work for metsChannelHabitat.
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
 
      intermediateMessage('.1.0Channel Habitat mets', loc='end')

      intermediateMessage('.1.1 subset data and check RESULTS for wadeable and boatable', loc='end')
        cdData <- subset(indat,PARAMETER == 'CHANUNCD' & ((RESULT %in% c('FA','CA','RA','RI','GL',
              'PB','PP','PD','PL','PT','P','DR','SB')  & SAMPLE_TYPE == 'PHAB_THALW') | (RESULT %in%
                   c('FA','RA','RI','GL','PO','CA','DR')  & SAMPLE_TYPE == 'PHAB_THAL')))

      #remove sidechannels (xa,xb etc.)

       cdData <- subset(cdData,TRANSECT %in% LETTERS[1:11])

      intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')

           tgl <- summaryby(cdData,'count',"pct_gl")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='GL',na.rm=TRUE)})
           tgl<-merge(tgl,tyout,by='UID',all.x=TRUE)
           #tgl$typesum<- ifelse( is.na(tgl$yessum),0,tgl$yessum)
           tgl$RESULT <- (tgl$typesum/tgl$RESULT)*100
           tgl<-tgl[c('UID','METRIC','RESULT')]

          tri <- summaryby(cdData,'count',"pct_ri")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='RI',na.rm=TRUE)})
           tri<-merge(tri,tyout,by='UID',all.x=TRUE)
           #tri$typesum<- ifelse( is.na(tri$yessum),0,tri$yessum)
           tri$RESULT <- (tri$typesum/tri$RESULT)*100
           tri<-tri[c('UID','METRIC','RESULT')]

            tra <- summaryby(cdData,'count',"pct_ra")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='RA',na.rm=TRUE)})
           tra<-merge(tra,tyout,by='UID',all.x=TRUE)
           #tra$typesum<- ifelse( is.na(tra$yessum),0,tra$yessum)
           tra$RESULT <- (tra$typesum/tra$RESULT)*100
           tra<-tra[c('UID','METRIC','RESULT')]

           tca <- summaryby(cdData,'count',"pct_ca")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='CA',na.rm=TRUE)})
           tca<-merge(tca,tyout,by='UID',all.x=TRUE)
           #tca$typesum<- ifelse( is.na(tca$yessum),0,tca$yessum)
           tca$RESULT <- (tca$typesum/tca$RESULT)*100
           tca<-tca[c('UID','METRIC','RESULT')]

           tfa <- summaryby(cdData,'count',"pct_fa")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='FA',na.rm=TRUE)})
           tfa<-merge(tfa,tyout,by='UID',all.x=TRUE)
           #tfa$typesum<- ifelse( is.na(tfa$yessum),0,tfa$yessum)
           tfa$RESULT <- (tfa$typesum/tfa$RESULT)*100
           tfa<-tfa[c('UID','METRIC','RESULT')]

          tdr <- summaryby(cdData,'count',"pct_dr")
           tyout<-aggregate( list(typesum=cdData$RESULT),list(UID=cdData$UID),function(x){sum(x=='DR',na.rm=TRUE)})
           tdr<-merge(tdr,tyout,by='UID',all.x=TRUE)
           #tdr$typesum<- ifelse( is.na(tdr$yessum),0,tdr$yessum)
           tdr$RESULT <- (tdr$typesum/tdr$RESULT)*100
           tdr<-tdr[c('UID','METRIC','RESULT')]

      #  temp1<-NA_filler(indat,cdData,list('pct_dr','pct_fa','pct_ca','pct_ra','pct_ri','pct_gl'))

           #subset data for boat only RESULT value 'po'

          btdata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THAL')

           tpo <- summaryby(btdata,'count',"pct_po")
           tyout<-aggregate( list(typesum=btdata$RESULT),list(UID=btdata$UID),function(x){sum(x=='PO',na.rm=TRUE)})
           tpo<-merge(tpo,tyout,by='UID',all.x=TRUE)
           #tpo$typesum<- ifelse( is.na(tpo$yessum),0,tpo$yessum)
           tpo$RESULT <- (tpo$typesum/tpo$RESULT)*100
           tpo<-tpo[c('UID','METRIC','RESULT')]

           #subset data for wadeable  only RESULT values

          wddata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THALW')

           tpp <- summaryby(wddata,'count',"pct_pp")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='PP',na.rm=TRUE)})
           tpp<-merge(tpp,tyout,by='UID',all.x=TRUE)
           #tpp$typesum<- ifelse( is.na(tpp$yessum),0,tpp$yessum)
           tpp$RESULT <- (tpp$typesum/tpp$RESULT)*100
           tpp<-tpp[c('UID','METRIC','RESULT')]

           tpd <- summaryby(wddata,'count',"pct_pd")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='PD',na.rm=TRUE)})
           tpd<-merge(tpd,tyout,by='UID',all.x=TRUE)
           #tpd$typesum<- ifelse( is.na(tpd$yessum),0,tpd$yessum)
           tpd$RESULT <- (tpd$typesum/tpd$RESULT)*100
           tpd<-tpd[c('UID','METRIC','RESULT')]


           tpb <- summaryby(wddata,'count',"pct_pb")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='PB',na.rm=TRUE)})
           tpb<-merge(tpb,tyout,by='UID',all.x=TRUE)
           #tpb$typesum<- ifelse( is.na(tpb$yessum),0,tpb$yessum)
           tpb$RESULT <- (tpb$typesum/tpb$RESULT)*100
           tpb<-tpb[c('UID','METRIC','RESULT')]


           tpl <- summaryby(wddata,'count',"pct_pl")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='PL',na.rm=TRUE)})
           tpl<-merge(tpl,tyout,by='UID',all.x=TRUE)
           #tpl$typesum<- ifelse( is.na(tpl$yessum),0,tpl$yessum)
           tpl$RESULT <- (tpl$typesum/tpl$RESULT)*100
           tpl<-tpl[c('UID','METRIC','RESULT')]


           tpt <- summaryby(wddata,'count',"pct_pt")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='PT',na.rm=TRUE)})
           tpt<-merge(tpt,tyout,by='UID',all.x=TRUE)
           #tpt$typesum<- ifelse( is.na(tpt$yessum),0,tpt$yessum)
           tpt$RESULT <- (tpt$typesum/tpt$RESULT)*100
           tpt<-tpt[c('UID','METRIC','RESULT')]


           tp <- summaryby(wddata,'count',"pct_p")
           tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='P',na.rm=TRUE)})
           tp<-merge(tp,tyout,by='UID',all.x=TRUE)
           #tp$typesum<- ifelse( is.na(tp$yessum),0,tp$yessum)
           tp$RESULT <- (tp$typesum/tp$RESULT)*100
           tp<-tp[c('UID','METRIC','RESULT')]


#               tsb <- summaryby(wddata,'count',"pct_sb")
#               tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='SB',na.rm=TRUE)})
#               tsb<-merge(tsb,tyout,by='UID',all.x=TRUE)
#               #tsb$typesum<- ifelse( is.na(tsb$yessum),0,tsb$yessum)
#               tsb$RESULT <- (tsb$typesum/tsb$RESULT)*100
#               tsb<-tsb[c('UID','METRIC','RESULT')]

          #compute summed metrics

         pfast<-rbind(tfa,tca,tra,tri)
         tfast<-summaryby(pfast,'sum','pct_fast') 

        pslow<-rbind(tpp,tpd,tpb,tpl,tpt,tp,tgl,tpo)
         tslow<-summaryby(pslow,'sum','pct_slow') 

        ppool<-rbind(tpp,tpd,tpb,tpl,tpt,tp,tpo)
         tpool<-summaryby(ppool,'sum','pct_pool') 



      intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')
        mets <- rbind(tdr,tp,tpt,tpl,tpb,tpd,tpp,tgl,tri,tra,tca,tfa,tfast,tslow,tpool)


      intermediateMessage('.1.4 Done with function metsChannelHabitat.1 ', loc='end')

        return(mets) 

}

# metsChannelHabitat()


