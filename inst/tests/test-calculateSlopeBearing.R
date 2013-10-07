context("Slope and Bearing")

test_that("calculateAngleMetrics returns returns correct values for U shaped reach", {
  
  # a U shaped reach with three bearings
  ans <- calculateAngleMetrics(c(1,1,1), c(180,90,0), c(1,1,1))
  ans <- castMetrics(ans)
  expect_that(ans$sinu, equals(3))
  expect_that(ans$xbearing, equals(90))
  
})

test_that("calculateSlopeMetrics returns returns correct values for a test transect", {
  
  ans <- calculateSlopeMetrics(rep(1,6), rep(c('a','b'), each = 3), c(0,45,0, 10,10,10), c(1,2,1,1,2,1)/4)
  ans <- castMetrics(ans)
  expect_that(ans$nslp, equals(2))
  expect_that(ans$xslope, equals(16.25))
  expect_that(ans$vslope, equals(sqrt((22.5 - 10)^2 / 2)))
  
})

test_that("calculateSlopeMetrics returns correct values for EPA data", {
  load(system.file('tests', 'data', 'SlopeBearing.expectedResults.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'SlopeBearing.depthData.Rdata', package = 'nrsa'))
  depth$STATION <- as.numeric(as.character(depth$STATION))
  n.station <- nWadeableStationsPerTransect(depth$UID, depth$TRANSECT, depth$STATION)
  
  load(system.file('tests', 'data', 'SlopeBearing.testData.Rdata', package = 'nrsa'))
  test[] <- lapply(test, function(x) type.convert(as.character(x)))
  
  test <- merge(test, n.station, by = c('uid', 'transect'), all.x = T)
  test$slope_percent <- with(test, ifelse(is.na(slope_percent), slope_none, slope_percent))
  test$proportion <- test[,'proportion_%'] / 100
  
  test1 <- with(test,
                fillSlopeBearingData(uid, transect, protocol == 'WADEABLE', increment_m, 
                                     n.station, distance_m, proportion, slope_percent, 
                                     slope_cm, azimuth_degrees))
 
  amet <- with(subset(test1, !is.na(azimuth) & !is.na(distance)),
               calculateAngleMetrics(uid, azimuth, distance))

  test2 <- na.omit(subset(test1, select = c(uid, transect, slope, proportion)))
  smet <- with(test1,
               calculateSlopeMetrics(uid[,drop = T], transect, slope, proportion))
  mets <- rbindMetrics(amet, smet, unique(meltMetrics(test1[,c('uid', 'transpc')])))
  check <- merge(mets, expected, by = c('uid', 'metric'), all = T)
  expect_that(check$result.x, equals(check$result.y))
})


