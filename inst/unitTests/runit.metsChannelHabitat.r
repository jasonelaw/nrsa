  metsChannelHabitatTest <- function()
       # Unit test for metsChannelHabitat.1
       # IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
       #has only wadable sites.  The  metsChannelHabitat.1 function needs data for
       #both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
       #were set to zero.  
       {

          intermediateMessage('.2.0Channel Habitat test of data', loc='end')
          intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
          # Create correctly formated test data, and run data through metsChannelHabitat.1
  testData <- metsChannelHabitat.inputData()
            
  #          testData$RESULT <- rep(as.character(0:4), length.out=nrow(testData))
           
    intermediateMessage('.2.3 Call metsChannelHabitat.1', loc='end')
           
  testDataResult<- metsChannelHabitat.1(testData)
           
    intermediateMessage('.2.4 Create Expected Data', loc='end')
           
  metsExpected <- metsChannelHabitat.testResults()

          intermediateMessage('.2.5 Merge Expected data and results, then compare', loc='end')
          #compare results from baseData (testDataResult) with expectedResults  (metsExpected)

            metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
            rr <- testDataResult

          # Calculated values should be within 10E-7 of expected values, should
          # only be missing where they are supposed to be missing and nonmissing where
          # they are supposed to be nonmissing.
          # Note: the errs dataframe can be printed to show where the errors occur when
          # debugging.

            tt <- merge(rr, metsExpected, by=c('UID','METRIC'), all=T)
            tt$diff <- tt$RESULT - tt$EXPECTED

            errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
            checkEquals(0, nrow(errs)
                           ,"Error: Channel Habitat  metrics are broken"
            )
 

          intermediateMessage(' .2.6 Done with Testing.', loc='end')

     }


 metsChannelHabitat.cleanup <- function(indb)
# Clean up when metsChannelHabitat() terminates
{
  odbcClose(indb)
}

metsChannelHabitat.inputData <- function()
# creates dataframe of channel habitat data for unit test
{
          # Create correctly formated test data, and run data through metsChannelHabitat.1
 testData<-    rbind(data.frame(UID ='WAZP99-0591',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THAL',
                TRANSECT='A',
                STA_NUM=c("1"),
                RESULT=c("GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"RI",
 NA,"RA",NA,"GL",NA,"PO",NA,"GL",NA,"PO",NA,"PO",NA,"GL",NA,"GL",NA,"RI",NA,"RI",NA,
 "RA",NA,"RA",NA,"RA",NA,"RA",NA,"PO",NA,"GL",NA,"GL",NA,"RI",NA,"GL",NA,"RI",NA,"GL",
 NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"PO",NA,"GL",NA,"GL",NA,"GL",
 NA,"RI",NA,"RI",NA,"RA",NA,"RA",NA,"RI",NA,"PO",NA,"PO",NA,"GL",NA,"RI",NA,"RI",NA,
 "RI",NA,"GL",NA,"GL",NA,"GL",NA,"PO",NA,"GL",NA,NA,NA,"GL",NA,"GL",NA,"GL",NA,"GL",
 NA,"PO",NA,"GL",NA,"GL",NA,"GL",NA,"RI",NA,"RI",NA,"RI",NA,"RA",NA,"GL",NA,"GL",NA,
 "GL",NA,"PO",NA,"PO",NA,"GL",NA,"RI",NA,"RI",NA,"GL",NA,"RI",NA,"RI",NA,"RI",NA,"RA",
 NA,"CA",NA,"RA",NA,"RI",NA,"RI",NA,"RA",NA,"GL",NA,"PO",NA,"PO",NA,"RI",NA,"RI",NA,
 "GL",NA,"RI",NA,"RI",NA,"RA",NA,"RA",NA,NA)
 ),
     data.frame(UID ='WCAP99-0587',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT='A',
                STA_NUM=c("1"),
                RESULT=c("PP","RA","RA","GL","GL","GL","RI","RI","RI","PP","PP","RA","RA","PP","RI","PB","PP","PP","CA",
"RA","RA","PB","PP","RA","RI","GL","GL","PP","RA","RI","PB","PP","PB","RA","RI","RA","GL","PB",
"PB","PB","PB","RI","RI","RI","RI","RA","PB","PB","RI","RI","RA","PB","RI","GL","RI","RA","CA",
"PB","GL","PP","RI","RI","RI","RI","RA","CA","PB","CA","RI","RI","PP","RI","GL","PP","PP","RA",
"CA","RA","CA","PB","PP","PB","PP","CA","PB","CA","PB","PB","GL","PB","CA","CA","PB","PB","FA",
"PB","RA","PB","PB","GL","CA","PB","PB")
 ),
     data.frame(UID ='WCAP99-0905',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT='A',
                STA_NUM=c("1"),
                RESULT=c("GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","PT","PT","PT","PT",
"PT","PT","GL","GL","GL","GL","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT",
"PT","PT","PT","GL","GL","GL","GL","GL","RI","GL","GL","GL","GL","PL","PL","PL","PL","RI","RI",
"GL","GL","GL","GL","GL","GL","GL","RI","RI","RI","RI","RI","RI","RI","GL","GL","GL","PT","PT",
"RI","RI","GL","GL","GL","GL","RI","RI","RI","GL","GL","GL","GL","GL","PT","PT","PT","RI","PT",
"PT","GL","RI","PL","PL")
 ),
     data.frame(UID ='WCOP99-0563',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT=c('A'),
                STA_NUM=c("1"),
                RESULT=c("GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","PD","PD","PD","PD","PD","PD","PD",
"PD","PD","PD","PD","PD","PD","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","PD","PD","PD","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL")
 ),
     data.frame(UID ='WCAP99-0585',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT=c("A","A","A","A","B","B","B","B","C","C","C","C"),
                STA_NUM=c("1","2","3","4","1","2","3","4","1","2","3","4"),
                RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
 )
 )
 
     testData$UID <- as.character(testData$UID)
     testData$TRANSECT <- as.character(testData$TRANSECT)
     testData$STA_NUM <- as.character(testData$STA_NUM)
     testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
     testData$PARAMETER <- as.character(testData$PARAMETER)
     testData$RESULT <- as.character(testData$RESULT)

return(testData)
}


metsChannelHabitat.testResults <- function()
# creates dataframe of channel habitat metrics calculation results for unit test

{
  metsExpected <- rbind(data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_fa',
                        RESULT=c( 0,0.9708737864,0,0,NA )
               ) 
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ca',
                        RESULT=c( 1.0101010101,10.67961165,0,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ra',
                        RESULT=c( 13.131313131,16.504854369,0,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ri',
                        RESULT=c( 25.252525253,21.359223301,17,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_gl',
                        RESULT=c( 47.474747475,10.67961165,48,89.333333333,NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_fast',
                        RESULT=c( 39.393939394,49.514563107,17,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_slow',
                        RESULT=c( 60.6060606,50.485436893,83,100, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pool',
                        RESULT=c( 13.131313131,39.805825243,35,10.666666667, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pp',
                        RESULT=c( NA,14.563106796,0,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pd',
                        RESULT=c( NA,0,0,10.666666667, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pb',
                        RESULT=c( NA,25.242718447,0,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pt',
                        RESULT=c( NA,0,29,0, NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pl',
                        RESULT=c(NA,0,6,0,NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_p',
                        RESULT=c(NA,0,0,0,NA) 
               )
                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_dr',
                        RESULT=c(0,0,0,0,NA) 
               )
#                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
#                        METRIC='pct_sb',
#                        RESULT=c( NA,0,0,0, NA)
#               )
           )
return(metsExpected)
}
