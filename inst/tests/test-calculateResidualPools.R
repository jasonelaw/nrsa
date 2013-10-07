context("Residual pools")

dim <- calculatePoolDimensions(c(0,1), depth = c(1,5), slope = 0.1)

test_that("calculatePoolDimensions works correctly", {
  
  expect_that(diag(dim[,,'depth']), equals(c(0,0))) # residual depth at base point is 0
  expect_that(diag(dim[,,'length']), equals(c(0,0))) # residual length at base point is 0
  
  expect_that(dim[2, 1, 'depth'], is_equivalent_to(5 - (1 + 0.1))) # residual depth is 3.9
  expect_that(dim[2, 1, 'length'], is_equivalent_to(1)) # residual length is 1
  
})

test_that("calculatePool works correctly", {
  
  ans <- calculatePool(c(0,1), depth = c(1, 5), slope = c(0.1, 0.1))
  
  expect_that(ans$is.pool, equals(c(F, T)))
  expect_that(ans$base.pt, equals(c(T, F)))
  expect_that(ans$depth, equals(c(NA, 5 - (1 + 0.1))))
  expect_that(ans$length, equals(c(NA, 1)))
  expect_that(ans$area, equals(ans$depth * ans$length))
  
})

test_that("cutDimensions works correctly", {
  
  dimens <- cbind(depth = c(0, 1, 1, 1, 0, -1),
                  length = 1:6)
  expected <- matrix(c(rep(1,3), 0, rep(1,4)), ncol = 2)
  expect_that(cutDimensions(dimens), is_equivalent_to(expected))
              
})
