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
  ans <- calculateLWDCountMetrics(d$UID, d$PARAMETER, d$RESULT, d$PROTOCOL == 'WADEABLE')
  names(ans) <- toupper(names(ans))
  tmp <- merge(ans, LargeWoody.expectedResults, by = c('UID', 'METRIC'), all.x = TRUE)
  expect_equal(tmp$RESULT.x, tmp$RESULT.y)
})

test_that("", {
  params <- c("DLDLL", "DLDML", "DLDSL", "DMDLL", "DMDML", "DMDSL", "DSDLL", 
              "DSDML", "DSDSL", "DXDLL", "DXDML", "DXDSL", "WLDLL", "WLDML", 
              "WLDSL", "WMDLL", "WMDML", "WMDSL", "WSDLL", "WSDML", "WSDSL", 
              "WXDLL", "WXDML", "WXDSL")
  vals <- rep(1, 24)
  uid <- rep(1, 24)
  d <- expand.grid(uid = 1:2, params = params, vals = 1)
  ans <- calculateLWDCountMetrics(d$uid, d$params, d$vals, rep(T, 48), F)
  exp <- as.vector(addmargins(array(1, dim = c(3,4,2,2)), 1:3))
  expect_equal(as.vector(ans), exp)
})

# test_that("", {
#   params <- c("DLDLL", "DLDML", "DLDSL", "DMDLL", "DMDML", "DMDSL", "DSDLL", 
#               "DSDML", "DSDSL", "DXDLL", "DXDML", "DXDSL", "WLDLL", "WLDML", 
#               "WLDSL", "WMDLL", "WMDML", "WMDSL", "WSDLL", "WSDML", "WSDSL", 
#               "WXDLL", "WXDML", "WXDSL")
#   vals <- rep(1, 24)
#   uid <- rep(1, 24)
#   ans <- calculateLWDCumulativeMetrics(uid, params, vals, rep(T, 24), 1, 1)
# 
# })
