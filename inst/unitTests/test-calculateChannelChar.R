context('Channel Characteristics')

uid           <- rep(1, 10)
shor2rip      <- 1:10
see.over.bank <- rep(c(F,T), each = 5)
constraint    <- rep(c('B','C', 'N', 'U'), length.out = 10)

test_that("Channel Characteristics return correct results", {
  
  expect_that(calculateShoreToVegDistance(uid, shor2rip)$result, 
              equals(c(1, 10, 5.5)))
  
  expect_that(calculateProportionSeeOverBank(uid, see.over.bank)$result,
              equals(as.numeric(0.5)))
  
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
