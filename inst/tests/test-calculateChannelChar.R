context('Channel Characteristics')
source(system.file('tests/expect_metric_equal.R', package = 'nrsa'))
uid           <- rep(1, 10)
shor2rip      <- 1:10
see.over.bank <- rep(c(F,T), each = 5)
constraint    <- rep(c('B','C', 'N', 'U'), length.out = 10)

test_that("Channel Characteristics return correct results", {
  
  expect_that(calculateShoreToVegDistance(uid, shor2rip)$result, 
              equals(c(1, 10, 5.5)))
  
  expect_that(calculateProportionSeeOverBank(uid, see.over.bank)$result,
              equals(as.numeric(50)))
  
  expect_that(calculateChannelConstraint(uid, constraint)$result,
              equals(c(30,30,20,20)))
  
})

test_that("Channel characteristics return correct metrics as character vectors", {
  
  expect_that(calculateShoreToVegDistance(uid, shor2rip)$metric,
              is_identical_to(c('mnshor', 'mxshor', 'xshor2vg')))
  expect_that(calculateProportionSeeOverBank(uid, see.over.bank)$metric,
              is_identical_to("pct_ovrb"))
  expect_that(calculateChannelConstraint(uid, constraint)$metric,
              is_identical_to(c("pctch_b", "pctch_c", "pctch_n", "pctch_u")))
  
})

test_that("Channel characteristics return correct values for EPA test data", {
  metrics <- c("conbankfull", "confeatures", "conpattern", "conpercent", 
               "constraint", "convalley", "convalleybox", "mnshor", "mxshor", 
               "pct_ovrb", "pctch_b", "pctch_c", "pctch_n", "pctch_u", "xshor2vg")
  load( file = system.file('tests', 'data', 'ChannelChar.testData.Rdata', package = 'nrsa'))
  m1 <- calculateShoreToVegDistance(test$UID, test$SHOR2RIP)
  m2 <- calculateProportionSeeOverBank(test$UID, test$SEEOVRBK)
  m3 <- calculateChannelConstraint(test$UID, test$CONSTRT)
  
  load( file = system.file('tests', 'data', "ChannelConstraint.testData.Rdata", package = 'nrsa'))
  m4 <- with(ChannelConstraint.testData, nrsa:::getChannelConstraint(UID, PARAMETER, RESULT))
  
  load( file = system.file('tests', 'data', 'ChannelChar.expectedResults.Rdata', package = 'nrsa'))
  expected <- castMetrics(expected)
  m  <- castMetrics(rbind(m1, m2, m3, m4))
  
  expect_true(setequal(m$uid, expected$uid))
  expected <- expected[match(m$uid, expected$uid),]
  
  for (i in metrics) expect_metric_equal(m, expected, i)
})