context('Large Wood Functions')

test_that("Large wood returns identical values as EPA test code.", {
  library(nrsa)
  load(system.file('tests', 'data', 'LargeWoody.expectedResults.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'LargeWoody.testData.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'LargeWoody.bankwidData.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'LargeWoody.protocolData.Rdata', package = 'nrsa'))
  
  LargeWoody.testData <- merge(LargeWoody.testData, LargeWoody.protocolData, by = 'UID', all.x = T)
  names(LargeWoody.bankwidData)[5] <- 'BANKWID'
  LargeWoody.testData <- merge(LargeWoody.testData, LargeWoody.bankwidData[,c(1,5)], by = 'UID', all.x = T)
  d <- LargeWoody.testData
  d$in.bankfull <- substr(d$PARAMETER, 1, 1) == 'W'
  d$diam        <- tolower(substr(d$PARAMETER, 2, 2))
  calculateLWDCounts(d$UID, d$PARAMETER, d$RESULT, d$PROTOCOL == 'WADEABLE')
})