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
  randomMissing <- function(x, percent = 0.1){
    is.na(x) <- sample(c(TRUE, FALSE), size = length(x), replace = T, prob = c(percent, 1-percent))
    x
  }
  f <- function(n.station, is.wadeable = T){
    tlev <- factor(LETTERS[1:10])
    slev <- factor(0:(n.station-1))
    if (!is.wadeable){
      tlev <- factor(tlev, rev(levels(tlev)))
      slev <- factor(slev, rev(levels(slev)))
    }
    ret <- arrange(expand.grid(uid = 1, transect = tlev, station = slev), transect, station)
    ret$depth <- runif(nrow(ret), 0, 10)
    ret
  }
  d <- f(15)
  # Problem: differentiate between structural missing data (data padded with extra unobserved stations) and actual missing data.
  # Solution: filter all unobserved boatable stations where station number > 9
  # Situtations:
  #   Boatable with unequal # of stations on each transect, but no 'extra' unobserved stations. Trimmed data.
  d <- f(12)
  used <- data.frame(transect = LETTERS[1:10], last.station = sample(9:11, size = 10, replace = T))
  d <- subset(join(d, used, by = 'transect'), as.numeric(as.character(station)) <= last.station, select = names(d))
  
  #   Boatable with 'extra' stations on every transect where depth unobserved. Padded data with constant 10 stations.
  d <- f(12)
  is.na(d$depth) <- d$station %in% 10:11
  filterBoatable(d)
  d$depth <- randomMissing(d$depth)
  filterBoatable(d)
  #   Boatable with 'extra' stations on every transect, but sometimes observed when extra stations are needed. Padded data with variable stations.
  d <- f(12)
  used <- data.frame(transect = LETTERS[1:10], last.station = sample(9:11, size = 10, replace = T))
  d <- join(d, used, by = 'transect')
  is.na(d$depth) <- as.numeric(as.character(d$station)) > d$last.station
  d$last.station <- NULL
  filterBoatable(d)
  
  
})

