metsHumanInfluenceTest <- function(){
  testData    <- testData("HumanInfluence.testData.Rdata")
  testResults <-  testData("HumanInfluence.expectedResults.Rdata")
  revalueHumanInfluence <- function(x) tolower(x)
  mets   <- with(testData, calculateHumanInfluence(UID, PARAMETER, RESULT))
  sdmets <- with(testData, calculateHumanInfluenceSD(UID, TRANSECT, RESULT))
  mets   <- rbind(mets, sdmets)
  dfCompare2(mets, testResults)
}
