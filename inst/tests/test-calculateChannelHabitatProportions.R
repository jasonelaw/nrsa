context("Channel Habitat Proportions")

uid     <- rep(1,26)
habitat <- rep(c('FA','CA','RA','RI','GL','PB', 'PP','PD','PL','PT','P','DR', 'PO'), each = 2)
all.wade <- rep(T, 26)
all.boat <- rep(F, 26)
mets <- sort(c("pct_ca", "pct_dr", "pct_fa", "pct_fast", "pct_gl", "pct_p", 
               "pct_pb", "pct_pd", "pct_pl", "pct_pool", "pct_pp", "pct_pt", 
               "pct_ra", "pct_ri", "pct_slow"))

test_that("calculateChannelHabitatProportions returns correct values and metrics", {
  
  expect_that(calculateChannelHabitatProportions(uid, habitat, all.wade),
              equals(data.frame(uid   = '1', 
                                metric = mets[], 
                                result = c(1,1,1,4,1,1,1,1,1,7,1,1,1,1,8)/.13, 
                                stringsAsFactors = F)))
  
  expect_that(calculateChannelHabitatProportions(uid, habitat, all.boat),
              equals(data.frame(uid   = '1',
                                metric = mets[c(1L, 2L, 3L, 4L, 5L, 10L, 13L, 14L, 15L)],
                                result =  c(1,1,1,4,1,7,1,1,8)/.13,
                                stringsAsFactors = F)))
  
})


test_that("calculateChannelHabitatProportions results are of right type", {
  
  expect_that(calculateChannelHabitatProportions(uid, habitat, all.wade)$uid,
              is_a('character'))
  
  expect_that(calculateChannelHabitatProportions(uid, habitat, all.wade)$metric,
              is_a('character'))
  
  expect_that(calculateChannelHabitatProportions(uid, habitat, all.wade)$result,
              is_a('numeric'))
  
})

test_that("calculateChannelHabitatProportions returns correct results for EPA test data", {
  
  load(system.file('tests', 'data', 'ChannelHabitat.testData.Rdata', package = 'nrsa'))
  load(system.file('tests', 'data', 'ChannelHabitat.expectedResults.Rdata', package = 'nrsa'))
  mets <- calculateChannelHabitatProportions(test$UID, test$RESULT, test$SAMPLE_TYPE == 'PHAB_THALW')
  check <- merge(mets, expected, by = c('uid', 'metric'), all = T)
  
  expect_that(check$result.x, equals(check$result.y))
  
})

test_that("calculateChannelHabitatProportions returns same results as aquamet package", {
  library(aquamet)
  data(thalwegEx)
  testChanHab <- suppressCat(metsChannelHabitat(thalwegEx))
  d <- dcast(thalwegEx, UID + TRANSECT + STATION ~ PARAMETER, value.var = 'RESULT')
  ans <- calculateChannelHabitatProportions(d$UID, d$CHANUNCD, !is.na(d$DEPTH))
  names(ans) <- toupper(names(ans))
  ans <- merge(ans, testChanHab, by = c('UID', 'METRIC'), all = T)
  expect_equal(ans$RESULT.x, ans$RESULT.y)
})
