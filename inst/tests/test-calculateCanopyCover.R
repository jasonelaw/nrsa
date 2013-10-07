context("Canopy Cover")

uid       <- rep(1, 10)
is.bank   <- rep(c(T,F), each = 5)
densiom   <- rep(17, 10)
densiomNA <- rep(NA, 10)
densiom1NA <- c(rep(17, 9), NA)

test_that("Canopy cover functions return correct values", {
  
  expect_that(calculateCanopyCover(uid, is.bank, densiom)$result,
              equals(c(5,5,0,0,100,100)))
  
})

test_that("Canopy cover functions handle missing values", {
  
  expect_that(calculateCanopyCover(uid, is.bank, densiomNA)$result,
              equals(c(0,0,NA,NA,NA,NA)))
  expect_that(calculateCanopyCover(uid, is.bank, densiom1NA)$result,
              equals(c(5,4,0,0,100,100)))
  
})

test_that("Canopy cover functions return correct metrics", {
  
  expect_that(calculateCanopyCover(uid, is.bank, densiom)$metric,
              is_identical_to(c("nbnk", "nmid", "vcdenbnk", 
                                "vcdenmid", "xcdenbnk", "xcdenmid")))
  
})

test_that("calculateChannelHabitatProportions results are of right type", {
  
  expect_that(calculateCanopyCover(uid, is.bank, densiom)$uid,
              is_a('character'))
  
  expect_that(calculateCanopyCover(uid, is.bank, densiom)$metric,
              is_a('character'))
  
  expect_that(calculateCanopyCover(uid, is.bank, densiom)$result,
              is_a('numeric'))
  
})
