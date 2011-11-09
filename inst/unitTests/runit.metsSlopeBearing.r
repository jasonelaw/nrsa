metsSlopeBearingTest <- function()
# Unit test for metsSlopeBearing. Test data taken from WEMAP data on 2-Feb-2010,
# and is then transformed into the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
# The expected metrics are obtained for these reaches as well, and modified as
# follows:
#   '2000 WAZP99-0569 1' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 only 2 slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no incremnt' xbearing changed from 42.435 to NA and
#       sinu changed from 1.1251 to NA since lack of distance between stations
#       means the distances on which these metrics rely on are unavailable.
#   '2003 WWYP99-0659 1' xslope changed from 0.86733 to 1.026666666 and vslope
#       changed from 0.73286 to 0.6480359691 to account for error in previous
#       calculations of slopes at transects with supplemental slopes measured as
#       elevation change.  These should be calculated as elev/(transpc * prop)
#       but instead were calculated as (elev/(transpc * prop))*prop, which were
#       then multiplied by the proportion again and summed to determine the
#       slope of the transect.  This lead to diminished mean slopes at each
#       transect.
#
{
  # Create fake input data.  Thalweg data initially has 1 incremnt at
  # each station, instead of once per site.
  ll <- metsSlopeBearing.makeTestData()
  fakeThal_IncremntsAtEachUID <- ll[[1]]
  fakeThal <- subset(fakeThal_IncremntsAtEachUID
                    ,!(PARAMETER=='INCREMNT' & TRANSECT !='A' & STATION !=0)
                    )
#  fakeThal <- fakeThal_IncremntsAtEachUID
  fakeChanGeom <- ll[[2]]

  fakeProtocol <- metsSlopeBearing.makeProtocols()
  # thal <- fakeThal; chanGeom <- fakeChanGeom; protocols <- fakeProtocol
  # Create expected results
  expected <- metsSlopeBearing.makeExpectedResults()


  # Calculate actual results using thalweg data with extra incremnt values.
  intermediateMessage('.  Testing streams and rivers data, thalweg table has extra incremnt data.', loc='end')
  results <- metsSlopeBearing.1(fakeThal_IncremntsAtEachUID, fakeChanGeom, fakeProtocol)

  # Compare expected and actual results
  expected <- expected[order(expected$UID, expected$METRIC),]
  results <- results[order(results$UID, results$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols and extra incremnt values in thalweg table"
             )


  # Compare expected and actual results
  intermediateMessage('.  Testing streams and rivers data, thalweg table normal.', loc='end')
  results <- metsSlopeBearing.1(fakeThal, fakeChanGeom, fakeProtocol)

  expected <- expected[order(expected$UID, expected$METRIC),]
  results <- results[order(results$UID, results$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols using normal thalweg data"
             )
             
  # Compare expected and actual results
#  expected <- expected[order(expected$UID, expected$METRIC),]
#  results <- results[order(results$UID, results$METRIC),]
#  expected$RESULT <- as.numeric(expected$RESULT)
#  results$RESULT <- as.numeric(results$RESULT)
##  Testing for only streams
  fakeProtocol.s <- subset(fakeProtocol, PROTOCOL=='WADEABLE')
  fakeThal.s <- subset(fakeThal, UID %in% fakeProtocol.s$UID)
  fakeChanGeom.s <- subset(fakeChanGeom, UID %in% fakeProtocol.s$UID)
  expected.s <- subset(expected, UID %in% fakeProtocol.s$UID)

  # Calculate actual results
  intermediateMessage('.  Testing streams data.', loc='end')
  results.s <- metsSlopeBearing.1(fakeThal.s, fakeChanGeom.s, fakeProtocol.s)
  results.s$RESULT <- as.numeric(results.s$RESULT)
  errs.s <- dfCompare(expected.s, results.s, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.s)
  checkEquals(NULL, errs.s
             ,"Error: Slope and bearing metrics are broken with wadeable data"
             )

  fakeProtocol.r <- subset(fakeProtocol, PROTOCOL=='BOATABLE')
  fakeThal.r <- subset(fakeThal, UID %in% fakeProtocol.r$UID)
  fakeChanGeom.r <- subset(fakeChanGeom, UID %in% fakeProtocol.r$UID)
  expected.r <- subset(expected, UID %in% fakeProtocol.r$UID)

  # Calculate actual results
  intermediateMessage('.  Testing rivers data.', loc='end')
  results.r <- metsSlopeBearing.1(fakeThal.r, fakeChanGeom.r, fakeProtocol.r)
  results.r$RESULT <- as.numeric(results.r$RESULT)

  errs.r <- dfCompare(expected.r, results.r, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.r)
  checkEquals(NULL, errs.r
             ,"Error: Slope and bearing metrics are broken with boatable data"
             )


}

metsSlopeBearing.makeTestData <- function()
# Creates thalweg and channel geometry data used in testing metsSlopeBearing().
# Returns a list of dataframes, element 1 is thalweg, element 2 is chanGeom.
#
# Test data taken from WEMAP data on 2-Feb-2010, and is then transformed into
# the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes taken in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
{
#234567890123456789012345678901234567890
  wemapThal <- data.frame(matrix(
                  c('2000 WAZP99-0505 1', 'A', 1.5, 7, 354, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'B', 1.5, 6, 357, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'C', 1.5, 4, 11, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'D', 1.5, 7.5, 3, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'E', 1.5, 12.5, 9, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'F', 1.5, 5, 17, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'G', 1.5, 3.5, 5, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'H', 1.5, 1.5, 57, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'I', 1.5, 4, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'J', 1.5, 6.5, 53, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1', 'A', 1.5, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'C', 1.5, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'D', 1.5, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'E', 1.5, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'F', 1.5, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'G', 1.5, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'H', 1.5, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'I', 1.5, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'J', 1.5, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2003 WWYP99-0659 1', 'A', 1.5, 36, 161, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'B', 1.5, 12, 110, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'C', 1.5, 12, 193,  20
                   ,4,  75, 30,  0, 124, 50, 'CM'
                   ,'2003 WWYP99-0659 1', 'D', 1.5, 26, 230, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'E', 1.5,  8, 193, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'F', 1.5, 18, 120, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'G', 1.5,  9, 210,  50
                   ,2, 108, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'H', 1.5, 14, 246, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'I', 1.5,  2, 157,  50
                   ,10, 238, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'J', 1.5,  1, 100, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'

                   ,'2000 WAZP99-0569 1 no incremnt', 'A', NA, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'B', NA, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'C', NA, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'D', NA, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'E', NA, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'F', NA, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'G', NA, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'H', NA, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'I', NA, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'J', NA, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 no slopes', 'A', 1.5, NA, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'B', 1.5, NA, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 only 2 slopes', 'A', 1.5, 6, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WIDP99-0556 1', 'A', 30, 0.2, 150, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'B', 30, 0.2, 50, 40
                   ,0.2, 140, 60, NA, NA, NA , 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'C', 33.3333333, 0.2, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'D', 30, 0.2, 50, 28.5714286
                   ,0.2, 40, 28.5714286, 0.2, 20, 42.8571429, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'E', 30, 0.2, 70, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'F', 30, 0.2, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'G', 30, 0.1, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'H', 30, 0.2, 50, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'I', 30, 0.1, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'J', 30, 0.2, 30, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WSDP99-0531 1', 'A', 20, 0.1, 80, 45.7142857
                   ,0.1, 340, 40, 0.1, 20, 14.2857143, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'B', 20, 0.1, 50, 50
                   ,0.1, 10, 25, 0.1, 100, 25, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'C', 20, 0.1, 360, 22.5
                   ,0.1, 350, 42.5, 0.1, 60, 35, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'D', 20, 0.1, 40, 25
                   ,0.1, 40, 12.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'E', 20, 0.1, 330, 12.5
                   ,0.1, 20, 87.5, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'F', 20, 0.1, 120, 15
                   ,0.1, 330, 22.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'G', 20, 0.1, 50, 75
                   ,0.1, 140, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'H', 20, 0.1, 90, 40.7407407
                   ,0.1, 340, 59.2592593, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'I', 20, 0.1, 200, 30
                   ,0.1, 200, 32.5, 0.1, 220, 37.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'J', 20, 0.1, 180, 12.5
                   ,0.1, 160, 52.5, 0.1, 240, 35, 'PERCENT'
                   )
                   ,ncol=13, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapThal) <- c('UID','TRANSECT','incremnt', 'slopet','beart','proportt'
                        ,'slope1','bear1','proport1','slope2','bear2','proport2'
                        ,'units'
                        )

  wemapRiverStations <- data.frame('UID'=c(rep('2000 WIDP99-0556 1', 10)
                                          ,rep('2000 WSDP99-0531 1', 10)
                                          )
                                  ,'TRANSECT'=rep(LETTERS[1:10], 2)
                                  ,'nsta'=c(20,20,18,20,20, 20,20,20,20,20
                                           ,20,20,20,20,20, 20,20,20,20,20
                                           )
                                  )

  # Create fakeThal from transposed wemap thalweg data.  This holds the wadeable
  # reach incremnt values; everything else is in the channel geometry.  Transect
  # C of WAZP99-0505 ended at station 8, so that is removed explicitly.  The
  # UNITS information is handled separately.
  # To allow proper counting of stations per transect during metrics calculation,
  # DEPTH is added at each station, though these values will not be used.
  units <- rename(wemapThal[c('UID','TRANSECT','units')], 'units', 'UNITS')
  
  ss <- subset(wemapThal, select=-units)
  tt <- dfLengthen(ss
                  ,c('UID','TRANSECT')
                  ,'PARAMETER'
                  ,'RESULT'
                  ,names(ss)[!(names(ss) %in%
                                       c('UID','TRANSECT')
                                     )]
                        )

  fakeThal <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               ) &
                     PARAMETER == 'incremnt'
                    )
  fakeThal <- merge(fakeThal, list('STATION'=0:9), all=TRUE)
  fakeThal <- subset(fakeThal
                    ,!(UID!='2000 WAZP99-0505 1' & TRANSECT=='C' & STATION=='9')
                    )
  fakeThal$PARAMETER <- 'INCREMNT'
  fakeThal$SAMPLE_TYPE <- 'PHAB_THALW'
  fakeThal$FLAG <- NA
  fakeThal$UNITS <- 'M'

  addDepths<-fakeThal
  addDepths$PARAMETER='DEPTH'
  addDepths$UNITS<-'CM'
  fakeThal<-rbind(fakeThal,addDepths)

  # Create fakeChanGeom from transposed wemap thalweg data.  River and stream
  # data are slightly different, and thus are assembled separately.  Rows with
  # missing values are removed.
  wadeable <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               )
                     & PARAMETER != 'incremnt'
                    )
  wadeable$LINE <- NA
  wadeable$PARAMETER <- ifelse(wadeable$PARAMETER == 'slopet', 'SLOPE'
                       ,ifelse(wadeable$PARAMETER == 'beart', 'BEARING'
                       ,ifelse(wadeable$PARAMETER == 'proportt', 'PROP'
                       ,ifelse(wadeable$PARAMETER == 'slope1', 'SLOPE2'
                       ,ifelse(wadeable$PARAMETER == 'bear1', 'BEARING2'
                       ,ifelse(wadeable$PARAMETER == 'proport1', 'PROP2'
                       ,ifelse(wadeable$PARAMETER == 'slope2', 'SLOPE3'
                       ,ifelse(wadeable$PARAMETER == 'bear2', 'BEARING3'
                       ,ifelse(wadeable$PARAMETER == 'proport2', 'PROP3', NA
                       )))))))))
  wadeable <- merge(wadeable, units, by=c('UID','TRANSECT'))
  wadeable$UNITS <- ifelse(wadeable$PARAMETER %in% c('SLOPE','SLOPE2','SLOPE3')
                          ,wadeable$UNITS
                          ,'NONE'
                          )

  bb <- subset(wemapThal
              ,!(UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           )
                )
              )
  bb <- merge(bb, wemapRiverStations, by=c('UID','TRANSECT'), all.x=TRUE)
  bb$distancet <- (as.numeric(bb$proportt)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance1 <- (as.numeric(bb$proport1)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance2 <- (as.numeric(bb$proport2)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$incremnt <- NULL
  bb$proportt <- NULL
  bb$proport1 <- NULL
  bb$proport2 <- NULL
  bb$nsta <- NULL

  boatable <- dfLengthen(bb, c('UID','TRANSECT'), 'PARAMETER', 'RESULT'
                        ,names(bb)[!(names(bb) %in% c('UID','TRANSECT'))]
                        )

  boatable$LINE <- ifelse(boatable$PARAMETER %in% c('slopet','beart','distancet'), 1
                  ,ifelse(boatable$PARAMETER %in% c('slope1','bear1','distance1'), 2
                  ,ifelse(boatable$PARAMETER %in% c('slope2','bear2','distance2'), 3, NA
                  )))

  boatable$PARAMETER <-
        ifelse(boatable$PARAMETER %in% c('slopet','slope1','slope2'), 'SLOPE'
       ,ifelse(boatable$PARAMETER %in% c('beart','bear1','bear2'), 'BEAR'
       ,ifelse(boatable$PARAMETER %in% c('distancet','distance1','distance2'), 'DISTANCE', NA
       )))
   boatable$UNITS <- 'NONE'

  fakeChanGeom <- subset(rbind(boatable, wadeable), !is.na(RESULT))
  fakeChanGeom$TRANLINE <- 'NONE'
  fakeChanGeom$BANK <- 'NONE'
  fakeChanGeom$SAMPLE_TYPE <- ifelse(fakeChanGeom$UID %in%
                                       c('2000 WAZP99-0505 1'
                                        ,'2000 WAZP99-0569 1'
                                        ,'2003 WWYP99-0659 1'
                                        ,'2000 WAZP99-0569 1 no incremnt'
                                        ,'2000 WAZP99-0569 1 no slopes'
                                        ,'2000 WAZP99-0569 1 only 2 slopes'
                                        )
                                    ,'PHAB_SLOPE'
                                    ,'PHAB_CHANBFRONT'
                                    )
  fakeChanGeom$FLAG <- as.character(NA)

  return(list(fakeThal, fakeChanGeom))
}


metsSlopeBearing.makeProtocols <- function()
# Create dataframe of protocol information for unit test
{
  return(data.frame('UID'=c('2000 WAZP99-0505 1'
                           ,'2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           ,'2000 WIDP99-0556 1'
                           ,'2000 WSDP99-0531 1'
                           )
                   ,'PROTOCOL'=c('WADEABLE','WADEABLE','WADEABLE','WADEABLE'
                                ,'WADEABLE','WADEABLE','BOATABLE'
                                ,'BOATABLE'
                                )
                   ,stringsAsFactors=FALSE
                   )
        )
}


metsSlopeBearing.makeExpectedResults <- function()
# Create dataframe of calculation results for unit test
{
  expected <- rbind(data.frame('UID'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('1.026666666','0.6480359691','10','15.000'
                                         ,'161.842','1.66858'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('10.8300','3.5006','10',NA
                                         ,NA,NA
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c(NA,NA,'0','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c(NA,NA,'2','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583'
                                         )
                              )
                   ,data.frame('UID'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470'
                                         )
                              )
                   )
  expected$UID <- as.character(expected$UID)
  expected$METRIC <- as.character(expected$METRIC)
  expected$RESULT <- as.character(expected$RESULT)

  return(expected)
}


# end of file