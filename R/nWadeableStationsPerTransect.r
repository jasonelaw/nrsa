# nWadeableStationsPerTransect.r
#
# Estimates the intended number of wadeable thalweg stations to be visited at
# an NRSA study site.
#
#  2/11/10 cws created
#  3/09/10 cws intended number of stations is based on most common last station
#          at a reach instead of maximum last station at a reach.  Thus if a
#          crew samples 11 stations at a single transect and 9 everywhere else,
#          the expected number of transects is still 9.
#  3/10/10 cws Handling case of no clear mode; correcting overall logic of test.
#
require(RUnit) 

nWadeableStationsPerTransect <- function(thal)
# Estimates the intended number of wadeable thalweg stations at each transect
# which are considered sampled (even if there is no data) for the purposes of
# calculating residual pools and channel lengths.  The number of stations at
# a transect is calculated as the greater of either the number of stations
# occuring in the dataframe for that transect, or the most common count of
# stations (i.e. station mode) occuring at that site.
#
# Currently, side-channel transects are ignored.
#
# Returns a dataframe with UID, TRANSECT and nSta=number of stations which
# 'should be' at the transect.
#
# ARGUMENTS:
# thal      dataframe of thalweg data for wadeable reaches
#
# ASSUMPTIONS:
# At most 1 station was sampled at any K transect.
#
{
  thal <- subset(thal, TRANSECT %in% LETTERS[1:11])
  
  staLast <- aggregate(list('staLast'=thal$STATION)
                      ,list('UID'=thal$UID, 'TRANSECT'=thal$TRANSECT)
                      ,max, na.rm=TRUE
                      )
  staMode <-aggregate(list('staLastMode'=staLast$staLast)
                     ,list('UID'=staLast$UID)
                     ,modalvalue
                     )
  staModeCount <-aggregate(list('staModeCount'=staLast$staLast)
                          ,list('UID'=staLast$UID)
                          ,modalCount
                          )

  tt <- merge(staLast, staMode, by='UID')
  tt <- merge(tt, staModeCount, by='UID')

  # calculate nSta at each transect, taking advantage of the fact that stations
  # are numeric, and thus that the last station to be expected for a transect
  # is the number of transects to be expected at that transect, adding 1 to
  # the station to account for station numbering starting at 0.
  tt$lastSta <-  ifelse(tt$staModeCount > 6
                       # if a clear mode exists, 'top off' the transect to at
                       # least the most common last station of transects A-J
                       ,ifelse(tt$TRANSECT %in% LETTERS[1:10]
                              ,ifelse(tt$staLast < tt$staLastMode
                                     ,tt$staLastMode
                                     ,tt$staLast
                                     )
                              # if it exists, transect K should only have station 0
                              ,0
                              )
                        # do not change last station if no clear mode
                       ,tt$staLast
                       )
  tt$nSta <- tt$lastSta+1
  
  tt<-subset(tt, select=c(UID,TRANSECT,nSta))                           

  return(tt)
}

