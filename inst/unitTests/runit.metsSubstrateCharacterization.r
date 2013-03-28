metsSubstrateCharacterizationTest <- function ()

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Substrate Characterization calculations', loc='start')

  df1 <- metsSubstrateCharacterization.wadeableSubstrateData()
  df2 <- metsSubstrateCharacterization.boatableThalwegSubstrateData()
  df3 <- metsSubstrateCharacterization.boatableLittoralSubstrateData()
  expected <- metsSubstrateCharacterization.expectedResults()

  intermediateMessage ('fetch_test_data.1', loc='start')

    #calculate the metrics

  rr <- metsSubstrateCharacterization.1 (df1, df2, df3)
  checkTrue(is.data.frame(rr)
           ,paste("Error: metsSubstrateCharacterization returns error message:"
                 ,rr
                 )
           )
   if(is.character(rr)) return (rr)
   
#  rr$RESULT <- as.numeric (rr$RESULT)
#  expected$RESULT <- as.numeric (expected$RESULT)

  intermediateMessage ( ' Done.', loc='end')


  tt <- merge(expected, rr, by=c('UID','METRIC'),all=TRUE
             ,suffixes=c('.expected', '.actual')
             )
 
#  zz <- subset(tt, is.na(RESULT.x) | is.na(RESULT.y))
#
#  xx <- sort(unique(rr$METRIC))
#  yy <- sort(unique(expected$METRIC))
#
#  xxOut <- subset(xx, !(xx %in% metrics$METRIC))
#  yyOut <- subset(xx, !(yy %in% metrics$METRIC))
 
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  errs <- subset(tt
                ,abs(diff) > 10^-3 |
                 is.na(RESULT.expected) != is.na(RESULT.actual)
                )
  checkEquals(0, nrow(errs)
            ,"Error: substrateCharacterization metrics are broken"
            )
            
# return (list (a=tt, b=errs))
}
  
metsSubstrateCharacterization.wadeableSubstrateData <- function()
# Creates dataframe of wadeable protocol substrate data for unit test
{
  testData('SubstrateCharacterization.wadeableSubstrate.Rdata')
}

metsSubstrateCharacterization.boatableThalwegSubstrateData <- function()
# Creates dataframe of boatable protocol substrate data for unit test
{
  testData("SubstrateCharacterization.boatableThalwegSubstrate.Rdata")
}

metsSubstrateCharacterization.boatableLittoralSubstrateData <- function()
# Creates dataframe of boatable protocol substrate data for unit test
{
  testData("SubstrateCharacterization.boatableLittoralSubstrate.Rdata")
}

metsSubstrateCharacterization.expectedResults <- function()
# returns dataframe of expected metrics calculation results for unit test
# UID 7 is 2004  WAZP99-0504 v1
# UID 8 is 2004  WCAP99-0817 v1
# UID 9 is 2004  WCAP99-0817 v2
{
  testData('SubstrateCharacterization.expectedResults.Rdata')
}

# end of file
