metsCanopyDensiometerTest <- function()
# Unit test for metsCanopyDensiometer.1

{
  intermediateMessage('.2.0 Canopy Densiometer test of data', loc='end')
  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
   testData <- metsCanopyDensiometer.testData()

  intermediateMessage ('.2.2 Create expected dataset', loc='end')
   metsExpected <- metsCanopyDensiometer.expectedMets ()
   
#in this dataset, the protoccol is determined by SAMPLE_TYPE.  Here where
#SAMPLE_TYPE == PHAB_CHANW, the protocol is WADEABLE
#SAMPLE_TYPE == PHAB_CHANB the protocol is BOATABLE   
#to test for one/both protocols, subset the data by SAMPLE_TYPE as a proxy for protocol

  intermediateMessage ('.2.3 Test with both protocols', loc='end')
  
  metsCanopyDensiometerTest.process (testData, metsExpected)

 intermediateMessage ('.2.4 Test with wadeable protocol', loc='end')
  test.w <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANW')
  expected.w <- subset (metsExpected, UID %in%  c('WAZP99-0545','WAZP99-0551','WCAP99-0534',
                                           'WCAP99-0535','WCAP99-0536','WCAP99-0539',
                                           'WMTP99-0549'))
  metsCanopyDensiometerTest.process (test.w, expected.w)
  
  intermediateMessage ('.2.5 Test with boatable protocol', loc='end')
  test.b <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANB')
  expected.b <- subset (metsExpected, UID %in%  c('WMTP99-0525','WMTP99-0587'))
  metsCanopyDensiometerTest.process (test.b, expected.b)
 } 
 
  metsCanopyDensiometerTest.process <- function (testData, metsExpected)

  {

 
  testDataResult<- metsCanopyDensiometer.1(testData)
  
 

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(rr, metsExpected, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-5 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Canopy Densiometer metrics are broken"
             )
}

 
metsCanopyDensiometer.cleanup <- function(indb)
# Clean up when metsCanopyDensiometer() terminates
{
  odbcClose(indb)
}

metsCanopyDensiometer.testData <- function()
# creates dataframe of canopy densiometer data for unit test
{
  testData('CanopyDensiometer.testData.Rdata')
}


metsCanopyDensiometer.expectedMets <- function()
# creates dataframe of canopy densiometer metrics calculation results for unit test

{
  testData('CanopyDensiometer.expectedResults.Rdata')
}
