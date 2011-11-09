# valLogicThalweg.r
#
# 12/21/09 cws created
#  3/30/10 cws corrected on.exit() function call.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valLogicThalweg <- function()
# Performs logic checks on the NRSA thalweg data.
#
# ASSUMPTIONS:
# Values for WETWIDTH in tblBANKGEOMETRY2 are accurate.
#
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valLogicThalweg.cleanup(chan))

  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(!is.data.frame(thal)) return(thal)

  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create range validation results
  validationResults <- valLogicThalweg.1(thal, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d values with illogical relations", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLogicThalweg.csv'
                                        ,sep=''
                                        )
                                  )
}

valLogicThalweg.1 <- function(df, meta.df, siteInfo)
# Does all the work for valLogicThalweg()
#
# ARGUMENTS:
# df      dataframe with NRSA Thalweg data
# meta.df   dataframe with parameter and form information from
#             tblPARAMETERDESCRIPTIONS2
# siteInfo  dataframe with site information from tblVISITS2
#
# ASSUMPTIONS:
#
{
  intermediateMessage('Thalweg logic checks.', loc='start')
  
  # wadeable: bar width < wetted width by definition
  # Collect both values on the same row and make comparison.  All units are
  # meters.
  bw <- subset(df, PARAMETER=='BARWIDTH', select=c(UID,TRANSECT,STATION,RESULT))
  bw$RESULT <- as.numeric(bw$RESULT)
  bw <- rename(bw, 'RESULT','BARWIDTH')
  intermediateMessage('.1')

  ww <- subset(df, PARAMETER=='WETWIDTH', select=c(UID,TRANSECT,STATION,RESULT))
  ww$RESULT <- as.numeric(ww$RESULT)
  ww <- rename(ww, 'RESULT','WETWIDTH')
  intermediateMessage('.2')

  tt <- merge(bw,ww, by=c('UID','TRANSECT','STATION'), all.x=TRUE, all.y=FALSE)
  tt <- subset(tt, (BARWIDTH >= WETWIDTH) & (WETWIDTH > 0) )

  # Select rows with incorrect values and add test description
  if(nrow(tt) == 0) {
      rr <- NULL
  } else {
      rr <- merge(subset(df, PARAMETER %in% c('BARWIDTH','WETWIDTH'))
                 ,subset(tt, select=c(UID,TRANSECT,STATION))
                 ,by=c('UID','TRANSECT','STATION')
                 ,all.x=FALSE, all.y=TRUE
                 )
      rr$TESTDESCRIPTION <- 'Bar width MUST be less than wetted width'
  }
  logicResults <- rr
  rm(bw,ww,tt, rr)
  intermediateMessage('.3')


  # wadeable: station increment > reach length / 100 (only checked at A 0)
  # Collect both values on the same row and make comparison.  All units are
  # meters.
  incr <- subset(df, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION=='0'
                ,select=c(UID,RESULT)
                )
  incr$RESULT <- as.numeric(incr$RESULT)
  incr <- rename(incr, 'RESULT','INCREMNT')
  intermediateMessage('.4')

  rl <- subset(df, PARAMETER=='REACHLENGTH' & TRANSECT=='A' & STATION=='0'
              ,select=c(UID,RESULT)
              )
  rl$RESULT <- as.numeric(rl$RESULT)
  rl <- rename(rl, 'RESULT','REACHLENGTH')
  intermediateMessage('.5')

  tt <- merge(incr, rl, by='UID', all.x=TRUE, all.y=TRUE)
  tt <- subset(tt, INCREMNT > REACHLENGTH/100)
  
  # Select rows with incorrect values and add test description
  if(nrow(tt) == 0) {
      rr <- NULL
  } else {
      rr <- merge(subset(df
                        ,PARAMETER %in% c('INCREMNT','REACHLENGTH') &
                         TRANSECT=='A' & STATION=='0'
                        )
                 ,subset(tt, select=UID)
                 ,by='UID'
                 ,all.x=FALSE, all.y=TRUE
                 )
      rr$TESTDESCRIPTION <- 'Station increment value MUST agree with reach length'
  }
  logicResults <- rbind(logicResults, rr)
  intermediateMessage('.6')
  rm(incr,rl,tt,rr)


  # Construct validation results
  if(is.null(logicResults)) {
      vv <- "There were zero logical errors detected in the thalweg data."
  } else {
      vv <- constructNRSAValidationResults(logicResults, meta.df, siteInfo)
  }
  intermediateMessage('. Done.', loc='end')

  return(vv)
}




valLogicThalweg.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

# end of file