metsRiparianVegetationTest <- function()
# Unit test for metsRiparianVegetation().
{
  ripData <- metsRiparianVegetation.makeData()

  # Create dataframe with expected results
  expected <- metsRiparianVegetation.makeExpected()

  # Compare expected and calculated results
  results <- metsRiparianVegetation.1(ripData)
  
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-4)
#  return(errs)
  checkEquals(NULL, errs, "Error: metsRiparianVegetation is broken.")
}

metsRiparianVegetation.makeData <- function()
# Create dataframe of fake riparian vegetation data. Uses data from the
# following WEMAP sites:
# 2000 WAZP99-0512 1 - site with no missing values
# 2000 WAZP99-0545 1 - site with many missing values, and all missing values
#                      for CANVEG, UNDERVEG
# 2000 WCAP99-0592 1 - site with side channels, one of which is entirely missing
#                      data.
# 2002 WUTP02-R003 1   site with all missing values for BARE
#
# Expected values for metrics were obtained from the wemap calculations and
# were changed only as follows:
#   2000 WAZP99-0545 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n, pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were changed from 0 to NA since
#                      all the values are missing, and NA is thus more correct.
#   2000 WCAP99-0592 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n , pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were not handling cases with some
#                      missing values correctly (the missing values were
#                      included in the sample size).  pcan_c changed from
#                      0.76923077 to 0.83333333333, pcan_m changed from
#                      0.11538462 to 0.125, pcan_n from 0.03846154 to
#                      0.0416666666666667, and pmind_m from 0.92307692 to 1.
{
  testData('RiparianVegetation.testData.Rdata')
}


metsRiparianVegetation.makeExpected <- function()
# Create dataframe of expected metrics calculations for unit test
{
  testData('RiparianVegetation.expectedResults.Rdata')
}

# Benchmark results 
# benchmark({source("C:/Documents and Settings/jlaw/My Documents/mercurial/nrsa-jason/metsRiparianVegetation.R")
#            metsRiparianVegetationTest()},
#           {source("C:/Documents and Settings/jlaw/My Documents/mercurial/nrsa/metsRiparianVegetation.R")
#            metsRiparianVegetationTest()},
#           replications = 10)
#   test replications elapsed relative user.self sys.self user.child sys.child
# 1    {           10    0.86 1.000000      0.85     0.01         NA        NA
# 2    {           10    4.31 5.011628      4.29     0.00         NA        NA

# end of file
