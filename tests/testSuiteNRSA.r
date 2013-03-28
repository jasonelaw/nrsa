# testSuiteNRSAMetrics.r
#
# Defines and runs the test suite for individual NRSA metrics calculation functions
#
# 02/18/10 cws Created.
#
require(RUnit)
require(nrsa)
test.dir <- system.file('unitTests', package = 'nrsa')
testSuite <- defineTestSuite('nrsa', 
                             dirs           = test.dir, 
                             testFileRegexp = "^runit.+\\.[rR]$",
                             testFuncRegexp = "^.+Test$")
testResult <- runTestSuite(testSuite)
testResultFile <- system.file('tests','nrsa.html', package = 'nrsa')
printTextProtocol(testResult, showDetails = F)
