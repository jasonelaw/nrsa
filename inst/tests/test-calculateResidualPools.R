library(plyr)
context("Residual pools")

dim <- nrsa:::calculatePoolDimensions(c(0,1), depth = c(1,5), slope = 0.1)

test_that("calculatePoolDimensions works correctly", {
  
  expect_that(diag(dim[,,'depth']), equals(c(0,0))) # residual depth at base point is 0
  expect_that(diag(dim[,,'length']), equals(c(0,0))) # residual length at base point is 0
  
  expect_that(dim[2, 1, 'depth'], is_equivalent_to(5 - (1 + 0.1))) # residual depth is 3.9
  expect_that(dim[2, 1, 'length'], is_equivalent_to(1)) # residual length is 1
  
})

test_that("calculatePool works correctly", {
  
  ans <- calculatePool(c(0,1), depth = c(1, 5), slope = c(0.1, 0.1))
  
  expect_that(ans$is.pool, equals(c(F, T)))
  expect_that(ans$base.point, equals(c(T, F)))
  expect_that(ans$depth, equals(c(NA, 5 - (1 + 0.1))))
  expect_that(ans$length, equals(c(NA, 1)))
  expect_that(ans$area, equals(ans$depth * ans$length))
  
})

test_that("createSiteSequence works correctly", {
  f <- function(uid, n.station){ arrange(expand.grid(uid = uid, transect = LETTERS[1:10], station = 0:(n.station-1)), uid, transect, station)}
  x <- mdply(data.frame(uid = 1:5, n.station = c(10,10,15,15,10)), f)
  x$is.wadeable <- c(F,F,T,T,T)[x$uid]
  
  
})


