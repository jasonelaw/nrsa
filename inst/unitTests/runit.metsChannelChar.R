


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
