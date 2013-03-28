metsChannelCharTest <- function()
# Unit test for metsChannelChar.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsChannelChar.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
#were set to zero.
{

  intermediateMessage('.2.0Channel Characteristic test of data', loc='end')
  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
  
  # Create correctly formated test data, and run data through metsChannelChar.1
  testData <- metsChannelChar.inputData()
  testCCData <- metsChannelchar.createChanChar()

  #  testData$RESULT <- rep(as.character(0:4), length.out=nrow(testData))

  intermediateMessage('.2.3 Call metsChannelChar.1', loc='end')

  metsResult <- metsChannelChar.1(testData, testCCData)

  intermediateMessage('.2.4 Create Expected Data', loc='end')

  metsExpected <- metsCanopyDensiometer.testResults()
#  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')

  intermediateMessage('.2.5 Merge Expected data and results, then compare', loc='end')
  # Check character mets separately from numeric mets to allow zeriFudge to
  # have an effect.
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.

  errC <- dfCompare(subset(metsExpected
                          ,METRIC %in% c('constraint','confeatures','conpattern','convalleybox')
                          )
                   ,subset(metsResult
                          ,METRIC %in% c('constraint','confeatures','conpattern','convalleybox')
                          )
                   ,c('UID','METRIC'), zeroFudge=1e-7
                   )
  ee <- subset(metsExpected
              ,!(METRIC %in% c('constraint','confeatures','conpattern','convalleybox'))
              )
  ee$RESULT <- as.numeric(ee$RESULT)
  rr <- subset(metsResult
              ,!(METRIC %in% c('constraint','confeatures','conpattern','convalleybox'))
              )
  rr$RESULT <- as.numeric(rr$RESULT)
  errN <- dfCompare(ee, rr, c('UID','METRIC'), zeroFudge=1e-7)
  errs <- rbind(errC, errN)
  checkEquals(NULL, errs
             ,"Error: Channel Characteristic metrics are broken"
  )

}




metsChannelChar.inputData <- function()
# creates dataframe of channel characteristics data for unit test
{
  # Create correctly formated test data, and run data through metsChannelChar.1
  testData <- rbind(expand.grid(PARAMETER=c('CONSTRT','SEEOVRBK','SHOR2RIP')
                   ,TRANSECT = LETTERS[1:11]
                   ,UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576')
                   ,SAMPLE_TYPE = 'PHAB_CHANB'
                   )
     )

  testData$RESULT <- c("N","YES",0,"N","YES",0,"N","YES",0,"C",NA,0,"C","YES",0,"N","YES",
              0,"C","YES",0,"N","YES",0,"N",NA,0,"N","YES",0,"C","YES",0,"B","N",0.3,
              "U","N",30,"B","N",2,"U","N",1,"U","N",2,"U","N",10,"B","N",15,"B","N",3,
              "B","N",1,"C","N",5,"B","N",2,"C","YES",0,"B","N",0,"B","N",0,"B","YES",
              0,"B","YES",0,"B","N",0,"B","N",0,"B",NA,0,"U","YES",0,"U","YES",1,NA,
              NA,NA,"C","YES",2.7,"C","YES",1.5,NA,NA,NA,"C","YES",1.3,"C","YES",
              1.1,"C","N",0.5,"C","N",0,"C","N",1.5,"C","YES",22,"C","N",3,"C","N",30,
              "B","YES",15,"U","YES",5,"B","YES",2,"C","N",10,"B","N",1,"U","N",1,
              "B","N",2,"B","N",1,"B","N",1,"B","N",2,"B","N",0.3)

  testData$UID <- as.character(testData$UID)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)

  return(testData)
}


metsCanopyDensiometer.testResults <- function()
# creates dataframe of channel characteristics metrics calculation results for unit test
{
  metsExpected <- rbind(data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='xshor2vg',
                                   RESULT=c(0,6.4818181818,0.1,6.36,3.6636363636 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mxshor',
                                   RESULT=c(0,30,1,30,15 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mnshor',
                                   RESULT=c(0,0.3,0,0,0.3 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pct_ovrb',
                                   RESULT=c(100,0,55.555555556,50,27.272727273 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_b',
                                   RESULT=c( 0,54.545454545,70,0,72.727272727)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_c',
                                   RESULT=c( 36.363636364,9.0909090909,10,100,9.0909090909)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                               METRIC='pctch_n',
                               RESULT=c( 63.636363636,0,0,0,0)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_u',
                                   RESULT=c( 0,36.363636364,20,0,18.181818182)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conbankfull',
                                   RESULT=rep('5.5', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='constraint',
                                   RESULT=rep('CON_BROAD', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='confeatures',
                                   RESULT=rep('HILLSLOPE', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conpattern',
                                   RESULT=rep('SINGLE', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conpercent',
                                   RESULT=rep('100', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='convalley',
                                   RESULT=rep('1500', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='convalleybox',
                                   RESULT=rep('Y', 5)
                                  )
                       )

  return(metsExpected)
}

metsChannelchar.createChanChar <- function()
# Create and return a dataframe of simulated channel constraint data from
# tblCHANNELCHAR2.
{
  testData <- expand.grid(PARAMETER=c('BANKFULL','CONSTRNT','FEATURES'
                                     ,'PATTERN','PERCENT','VALLEY','VALLYBOX'
                                     )
                         ,UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528'
                                 ,'WUTP99-0553','WWYP99-0576'
                                 )
                         )
  testData$PARAMETER <- as.character(testData$PARAMETER)
  testData$UID <- as.character(testData$UID)
  testData$RESULT <- rep(c('5.5','CON_BROAD','HILLSLOPE','SINGLE','100','1500','Y'),5)
  testData$SAMPLE_TYPE <- 'PHAB_CHCON'
  testData$TRANSECT <- 'NONE'
  testData$FLAG <- ''
  
  return(testData)
}
