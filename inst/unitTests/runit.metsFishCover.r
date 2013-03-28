#unit tests for metsFishCover

metsFishCoverTest <- function ()

{
  # Create correctly formated test data, and run data through metsFishCover.1
  testData <- metsFishCover.createData()
#    testData <- expand.grid(UID = 1:2000, TRANSECT = LETTERS[1:11], SAMPLE_TYPE = 'PHAB_CHANB',
#                PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE', 'MACPHY', 'OVRHNG', 'STRUCT', 'UNDCUT'
#                              , 'WOODY'))
#    testData$RESULT <- sample(0:4, size = nrow(testData), replace = T)
#    testData$FLAG <- NA
  testData.River <- subset(testData, UID %in% c('1','2','3','4','5'))
  testData.Stream <- subset(testData, UID %in% c('6','7','8','9','10'))

  testDataResult <- metsFishCover.1(testData)
  testDataResult.River <- metsFishCover.1(testData.River)
  testDataResult.Stream <- metsFishCover.1(testData.Stream)
  
  #create the expected results (mets) for the test data using outside calculations
  metsExpected <- metsFishCover.createResults()
  
  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  tt <- merge(rr, metsExpected, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream and river) are broken"
             )
             
  metsExpected.River <- subset(metsExpected, UID %in% c('1','2','3','4','5'))
  rr <- testDataResult.River

  tt <- merge(rr, metsExpected.River, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (river) are broken"
             )

  metsExpected.Stream <- subset(metsExpected, UID %in% c('6','7','8','9','10'))
  rr <- testDataResult.Stream

  tt <- merge(rr, metsExpected.Stream, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream) are broken"
             )
}


metsFishCover.createData <- function()
#
{
  testData('FishCover.testData.Rdata')
}


metsFishCover.createResults <- function()
#
{
  testData('FishCover.expectedResults.Rdata')
}

# end of file


                                  
