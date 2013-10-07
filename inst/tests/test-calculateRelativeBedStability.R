context("Relative Bed Stability")

test_that("calculateRefinedCriticalDiameter returns correct values with missing data", {
  
  d <- expand.grid(xdepth = c(1,NA), xslope = c(1,NA), rp100 = c(1,NA),
                   xbkf_h = c(1,NA), v1w_msq = c(1,NA))
  expect_that(do.call('calculateRefinedCriticalDiameter', d),
              equals(c(log10(50 * 1/100 * 13.7), rep(NA, 31))))
  
})

test_that("calculateCrudeCriticalDiameter returns correct values for simple data", {
  
  expect_that(calculateCrudeCriticalDiameter(0,0), equals(as.numeric(NA)))
  expect_that(calculateCrudeCriticalDiameter(2,10), equals(log10(13.7)))
  
})

test_that("calculateRefinedCriticalDiameter returns correct values for simple data", {
  
  expect_that(calculateRefinedCriticalDiameter(1, 1, 1, 1, 1, old.version = F),
              equals(log10(50 * 1/100 * 13.7)))
  expect_that(calculateRefinedCriticalDiameter(1, 1, 1, 1, 1, old.version = T, 1, 1),
              equals(log10(50 * 1/100 * 13.7)))
  
})

test_that("calculateRelativeBedStability functions return correct values for EPA test data: should fail until EPA explains bugs...", {
  
  load(system.file('tests', 'data', 'RelativeBedStability.expectedResults.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'RelativeBedStability.testData.Rdata', package = 'nrsa'))
  
  test <- within(test, {
    xslope  <- forceSlopeGt0(xslope)
    xdepth  <- fixBoatableDepthUnits(xdepth, protocol == 'WADEABLE')
    sddepth <- fixBoatableDepthUnits(sddepth, protocol == 'WADEABLE')
    s_rp100 <- calculateS_RP100(xslope, sddepth)
    v1w_msq <- fillMissingLWD(v1w_msq, xfc_lwd)
    ltest      <- calculateCrudeCriticalDiameter(xdepth, xslope)
    ldmb_bw5   <- calculateRefinedCriticalDiameter(xdepth, xslope, rp100, xbkf_h, v1w_msq)
    s_ldmb_bw5 <- calculateRefinedCriticalDiameter(xdepth, xslope, s_rp100, xbkf_h, v1w_msq)
    ldmb_bw4   <- calculateRefinedCriticalDiameter(xdepth, xslope, rp100, xbkf_h, v1w_msq, T, xbkf_w, xwidth)
  })
  
  test <- cbind(test, with(test, calculateCriticalDiameterKaufmann(xdepth, xslope, rp100, xbkf_h, v1w_msq, lsub_dmm)))
  test <- cbind(test, with(test, calculateRelativeBedStability(lsub_dmm, lsub2dmm, ltest, ldmb_bw4, ldmb_bw5, s_ldmb_bw5, ldcbf_g08)))
  met.names <- c("ltest", "lrbs_tst", "ldmb_bw5", "s_ldmb_bw5", "ldmb_bw4", 
                 "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", "lrbs_bw6", "s_lrbs_bw6", 
                 "Dcbf_g08", "ldcbf_g08", "lrbs_g08", "s_rp100")
  mets <- meltMetrics(test[, c('uid', met.names)])
  check  <- merge(mets, expected, by = c('uid', 'metric'), all = T)

  expect_that(check$result.x, equals(check$result.y))  
  
  is.na(check$result.x) <- is.na(check$result.y)
  
  expect_that(check$result.x, equals(check$result.y))
  
})

test_that("fillMissingLWD returns correct values", {
  
  expect_that(fillMissingLWD(NA, 1), equals(as.numeric(NA)))
  expect_that(fillMissingLWD(NA, NA), equals(as.numeric(NA)))
  expect_that(fillMissingLWD(NA, 0), equals(0))
  expect_that(fillMissingLWD(1.1, 1), equals(1.1))

})

test_that("forceSlopeGt0 returns correct values", {
  
  expect_that(forceSlopeGt0(-1), equals(0.01))
  expect_that(forceSlopeGt0(NA), equals(as.numeric(NA)))
  expect_that(forceSlopeGt0(0.001), equals(0.01))
  expect_that(forceSlopeGt0(0.5), equals(0.5))
  
})

test_that("fixBoatableDepthUnits returns correct values", {
  
  expect_that(fixBoatableDepthUnits(c(1, 1), c(T, F)), equals(c(1, 100)))
  expect_that(fixBoatableDepthUnits(c(NA, NA), c(T, F)), equals(as.numeric(c(NA, NA))))
  expect_that(fixBoatableDepthUnits(c(NA, 1), c(T, F)), equals(c(NA, 100)))
  expect_that(fixBoatableDepthUnits(c(1, NA), c(T, F)), equals(c(1, NA)))
  
})

test_that("calculateS_RP100 returns correct values", {
  
  expect_that(calculateS_RP100(1, 1), equals(10^-0.44767))
  expect_that(calculateS_RP100(1, 10), equals(10^(-0.44767 + 1.25381)))
  expect_that(calculateS_RP100(10, 1), equals(10^(-0.44767 + -0.20675)))
  expect_that(calculateS_RP100(10, 10), equals(10^(-0.44767 + 1.25381 + -0.20675)))
  expect_that(calculateS_RP100(0, 1), equals(Inf))
  expect_that(calculateS_RP100(1, 0), equals(0))
  expect_that(calculateS_RP100(10, NA), equals(as.numeric(NA)))
  expect_that(calculateS_RP100(NA, 10), equals(as.numeric(NA)))
  
})

test_that("calculateRelativeBedStability returns correct values", {

  expect_that(calculateRelativeBedStability(2,2,1,1,1,1,1), 
              is_equivalent_to(matrix(1, nrow = 1, ncol = 7)))
  expect_that(calculateRelativeBedStability(NA,2,1,1,1,1,1), 
              is_equivalent_to(matrix(rep(c(NA, 1), times = c(5,2)), nrow = 1, ncol = 7)))
  expect_that(calculateRelativeBedStability(2,NA,1,1,1,1,1), 
              is_equivalent_to(matrix(rep(c(1, NA), times = c(5,2)), nrow = 1, ncol = 7)))
  
})
           
test_that("Metric functions return correct metric names.", {
  expect_that(colnames(calculateRelativeBedStability(2,2,1,1,1,1,1)),
              equals(c("lrbs_tst", "lrbs_bw4", "lrbs_bw5", "s_lrbs_bw5", 
                       "lrbs_g08", "lrbs_bw6", "s_lrbs_bw6")))
})
