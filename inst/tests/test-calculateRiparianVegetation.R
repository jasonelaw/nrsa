context("Riparian Vegetation")

test_that("Data formatted correctly", {
  pars <- c("BARE", "CANBTRE", "CANSTRE", "CANVEG", "GCNWDY", "GCWDY", 
            "UNDERVEG", "UNDNWDY", "UNDWDY")
  result <- c(3,3,3,'D',3,3,'D',3,3)
  x <- data.frame(uid = 1, transect = 1, transdir = 1, parameter = pars, result = result)
  ans <- formatRiparianVegetation(x$uid, x$transect, x$transdir, x$parameter, x$result)
  
  expect_that(ans$r_bare, equals(0.575))
  expect_that(ans$r_canbtre, equals(0.575))
  expect_that(ans$r_canstre, equals(0.575))
  expect_that(ans$r_gcnwdy, equals(0.575))
  expect_that(ans$r_gcwdy, equals(0.575))
  expect_that(ans$r_undnwdy, equals(0.575))
  expect_that(ans$r_undwdy, equals(0.575))
  
  expect_that(ans$p_canbtre, is_true())
  expect_that(ans$p_canstre, is_true())
  expect_that(ans$p_gcnwdy, is_true())
  expect_that(ans$p_gcwdy, is_true())
  expect_that(ans$p_undnwdy, is_true())
  expect_that(ans$p_undwdy, is_true())
  
})

test_that("calculateVegetationPresence works correctly", {
  
  expect_that(calculateVegPresence(NA, NA, NA, NA, NA, NA), 
              is_equivalent_to(matrix(NA, 1, 7)))
  expect_that(calculateVegPresence(T, T, T, T, T, T), 
              is_equivalent_to(matrix(T, 1, 7)))
  expect_that(calculateVegPresence(F, F, F, F, F, F), 
              is_equivalent_to(matrix(F, 1, 7)))
  expect_that(calculateVegPresence(F, F, F, F, F, F), 
              is_equivalent_to(matrix(F, 1, 7)))
  expect_that(calculateVegPresence(T, F, T, F, T, F), 
              is_equivalent_to(matrix(T, 1, 7)))
  expect_that(calculateVegPresence(F, T, F, T, F, T), 
              is_equivalent_to(matrix(c(rep(T,6),F), 1, 7)))
  
})

test_that("calculateVegetationTypes works correctly", {
  
  expect_that(calculateVegetationTypes(c('C', 'D'), c('E', 'M'), c(1,1))$result,
              equals(c(0,0,0.5,0.5,0,0.5,0.5,0,0,0)))
  
})