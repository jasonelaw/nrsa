metsSubstrateEmbedTest <- function()
# Unit test for metsSubstrateEmbed.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsSubstrateEmbed.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
#were set to zero.
{
  intermediateMessage('.2.0Substrate Embeddedness test of data', loc='end')
  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')

  # Create correctly formated test data, and run data through metsSubstrateEmbed.1
  testData <- metsSubstrateEmbed.createTestData()

  intermediateMessage('.2.3 Call metsSubstrateEmbed.1', loc='end')

  testDataResult<- metsSubstrateEmbed.1(testData)

  intermediateMessage('.2.4 Create Expected Data', loc='end')

  metsExpected <- metsSubstrateEmbed.createExpectedResults()


  intermediateMessage('.2.5 Merge Expected data and results, then compare', loc='end')
  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)

  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.

  tt <- merge(rr, metsExpected, by=c('UID','METRIC'), all=TRUE)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  intermediateMessage(' .2.6 Done with Testing.', loc='end')
  checkEquals(0, nrow(errs)
             ,"Error: Substrate Embeddedness metrics are broken"
             )

}

metsSubstrateEmbed.createTestData <- function()
# Creates test data for unit test.
{
  testData <- rbind(data.frame(UID ='WCAP99-0780',
                     PARAMETER='EMBED',
                     SAMPLE_TYPE='PHAB_CHANW',
                     TRANSECT='A',
                     TRANSDIR=c("CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT"
                               ),
                     RESULT=c( 15, 50, 50, 50,100, 50, 50, 50, 10,100
                             ,100, 50, 50, 50,100,100,100,100,100,100
                             , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                             ,100,100, 50, 85, 50, NA, NA, NA, NA, NA
                             , 90, 50, 50, 50, 50,100,100,100,100,100
                             , 50, 50,100,100,100
                             )
                     ),
                    data.frame(UID ='WCAP99-0785',
                     PARAMETER='EMBED',
                     SAMPLE_TYPE='PHAB_CHANW',
                     TRANSECT='A',
                     TRANSDIR=c("CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT"
                               ),
                     RESULT=c(100,100,100,100,100,100, 80,100, 70,100
                             ,100,100,100, 90,100,100,100,100,100,100
                             ,100,100,100,100,100,  0,  0,100,  0,  0
                             ,  0,100,100,  0,100,  0,  0,100,  0,  0
                             ,  0,  0,100,  0,  0,100,  0,100,100,  0
                             ,  0,100,100,  0,  0
                             )
                     ),
                    data.frame(UID ='WCAP99-0787',
                     PARAMETER='EMBED',
                     SAMPLE_TYPE='PHAB_CHANW',
                     TRANSECT='A',
                     TRANSDIR=c("CT","LC","LF","RC","RT"),
                     RESULT=c(NA,NA,NA,NA,NA)
                     ),
                    data.frame(UID ='WIDP99-0550',
                     PARAMETER='EMBED',
                     SAMPLE_TYPE='PHAB_CHANW',
                     TRANSECT='A',
                     TRANSDIR=c("CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT"
                               ),
                     RESULT=c(100,100,100,100,100, NA, NA, NA, NA, NA
                             , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                             , NA, NA, NA, NA, NA,100,100,100,100,100
                             , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                             , NA, NA, NA, NA, NA
                             )
                     ),
                    data.frame(UID ='WIDP99-0603',
                     PARAMETER='EMBED',
                     SAMPLE_TYPE='PHAB_CHANW',
                     TRANSECT='A',
                     TRANSDIR=c("CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT","CT","LC","LF","RC","RT"
                               ,"CT","LC","LF","RC","RT"
                               ),
                     RESULT=c(100,100,100,100,100,100,100,100,100,100
                             ,100,100,100,100,100,100,100,100,100,100
                             ,100,100,100,100,100,100,100,100,100,100
                             ,100,100,100,100,100,100,100,100,100,100
                             ,100,100,100,100,100,100,100,100,100,100
                             ,100,100,100,100,100
                             )
                     )
                   )

  testData$UID <- as.character(testData$UID)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)
  testData$RESULT<- as.numeric( testData$RESULT)

  return(testData)
}


metsSubstrateEmbed.createExpectedResults <- function()
# Create dataframe of expected results for unit test
{
  metsExpected <- rbind(data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='n55',
                                   RESULT=c(40,55,0,10,55 )
                                  )
                       ,data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='xembed',
                                   RESULT=c(73.75,64.363636364,NA,100,100)
                                   )
                       ,data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='vembed',
                                   RESULT=c(28.005265073,47.444456272,NA,0,0)
                                   )
                       ,data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='n33',
                                   RESULT=c(24,33,0,6,33 )
                                  )
                       ,data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='xcembed',
                                   RESULT=c(70.833333333,55.757575758,NA,100,100)
                                   )
                       ,data.frame(UID = c('WCAP99-0780','WCAP99-0785'
                                          ,'WCAP99-0787','WIDP99-0550'
                                          ,'WIDP99-0603'),
                                   METRIC='vcembed',
                                   RESULT=c(29.65990804,48.991727811,NA,0,0)
                                   )
                       )

  return(metsExpected)
}

metsSubstrateEmbed.cleanup <- function(indb)
# Clean up when metsSubstrateEmbed() terminates
{
  odbcClose(indb)
}