context("Channel Morphology")

test_that("calculateWettedWidthMetrics", {
  expect_error(calculateWettedWidthMetrics(NA, 1L))
  expect_equal(calculateWettedWidthMetrics(1L, NA)$result, NA_real_)
})

# test_that("Channel Morphology return correct metrics for EPA test data: should fail until EPA explains why they don't use transect K", {
#   
#   load(system.file('tests', 'data', 'ChannelMorphology.testData.Rdata', package = 'nrsa'))
#   load(system.file('tests', 'data', 'ChannelMorphology.expectedResults.Rdata', package = 'nrsa'))
#   
#   tr <- with(subset(test, !is.na(wetwid)), 
#              calculateThalwegRatios(uid, wetwid, depth))
#   td <- with(subset(test, !is.na(depth)), 
#              calculateThalwegDepthMetrics(uid, protocol == 'WADEABLE', depth, units = 'mixed'))
#   
#   dcj <- with(test, 
#               joinExtraTransects(uid, transect, wetwid, bankwid, bankhgt, incishgt))
#   cm <- with(dcj, 
#              calculateChannelMetrics(uid, bankwid.sm, incishgt.mx, bankhgt.mx))
#   ww <- with(subset(dcj, !is.na(wetwid.mx)), 
#              calculateWettedWidthMetrics(uid, wetwid.mx))
#   
#   dcj2 <- merge(dcj, subset(test, select = c('uid', 'transect', 'station', 'depth')), all.x = T)
#   trr <- with(subset(dcj2, !(is.na(bankwid.sm) | is.na(bankhgt.mn) | is.na(depth))), 
#               calculateTransectRatios(uid, bankwid.sm, bankhgt.mn, depth))
#   
#   mets <- rbindMetrics(tr, cm, ww, trr, td)
#   check <- merge(mets, expected, by = c('uid', 'metric'), all = T)
#   expect_that(check$result.x, equals(check$result.y))
# 
# })

test_that("calculateWettedWidthMetrics")

# test_that("Channel Morphology return correct metrics for EPA test data: should fail until EPA explains why they don't use transect K", {
#   library(aquamet)
#   data("bankgeomEx")
#   data("thalwegEx")
#   data("visitsEx")
#   testChanMorph <- metsChannelMorphology(bankgeomEx,thalwegEx,visitsEx)
#   
#   
#   })

# d1 <- metsChannelMorphology.createBankGeometryData()
# d2 <- metsChannelMorphology.createThalwegData()
# protocols <- metsChannelMorphology.createProtocolData()
# expected <- metsChannelMorphology.createExpectedResults()
# 
# df1 <- d1
# df2 <- d2
# 
# names(d1)        <- tolower(names(d1))
# names(d2)        <- tolower(names(d2))
# names(protocols) <- tolower(names(protocols))
# names(expected)  <- tolower(names(expected))
# expected$result  <- as.numeric(expected$result)
# d1$station       <- 0
# d2$result        <- as.numeric(d2$result)
# d2$result        <- ifelse(d2$units == 'CM', d2$result / 100, d2$result)
# d2 <- subset(d2, select = c(uid, transect, station, parameter, result))
# d1 <- subset(d1, select = c(uid, transect, station, parameter, result))
# d  <- rbind(d1, d2)
# d  <- merge(d, protocols, by = 'uid')
# dm <- melt(d, id.var = c('uid', 'transect', 'station', 'parameter', 'protocol'), 
#            measure.var = 'result')
# dc <- dcast(dm, uid + transect + station  + protocol ~ parameter, 
#             fun.aggregate = function(x) unique(x)[1])
# dc[] <- lapply(dc, function(x) type.convert(as.character(x)))
# names(dc) <- tolower(names(dc))
# 
# 
# dcj <- joinExtraTransects(dc$uid, dc$transect, dc$station, dc$wetwid, dc$bankwid, dc$bankhgt, dc$incishgt)
# 
# tr <- with(subset(dc, !is.na(wetwid)), 
#            calculateThalwegRatios(uid, wetwid, depth))
# 
# cm <- with(dcj, 
#            calculateChannelMetrics(uid, bankwid.sm, incishgt.mx, bankhgt.mx))
# 
# ww <- with(subset(dcj, !is.na(wetwid.mx)), 
#            calculateWettedWidthMetrics(uid, wetwid.mx))
# 
# dcj2 <- merge(dcj, subset(dc, select = c('uid', 'transect', 'station', 'depth')), all.x = T)
# trr <- with(subset(dcj2, !(is.na(bankwid.sm) | is.na(bankhgt.mn) | is.na(depth))), 
#             calculateTransectRatios(uid, bankwid.sm, bankhgt.mn, depth))
# 
# td <- with(subset(dc, !is.na(depth)), 
#            calculateThalwegDepthMetrics(uid, protocol == 'WADEABLE', depth, units = 'mixed'))
# 
# mets <- rbindMetrics(tr, cm, ww, trr, td)
# check <- merge(mets, expected, by = c('uid', 'metric'), all = T)
# expect_that(check$result.x, equals(check$result.y))
