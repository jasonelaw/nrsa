# metsSlopeBearing.r
#
# 02/01/10 cws created.
# 03/10/10 cws Updated as needed for change in nWadeableStationsPerTransect().
# 03/11/10 cws call to writeNRSACalcResults() corrected.
# 03/22/10 cws Moved unit test dataframes to separate functions
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
# 04/09/10 ssr Modified unit test to include stream only and boatable only options.
#              Modified metsSlopeBearing.1 to allow stream or river only options.
#  4/22/10 cws cleaned up unit test and added case for site with slopes measured
#          as changes in elevation.  Updated calculations to handle these sites.
#  5/27/10 cws Updated code to use INCREMNT occuring only at A 0, reflecting
#              the current data organization.  Modified unit test accordingly.
#
require(RODBC)
require(RUnit)

metsSlopeBearing <- function()
# Calculates NRSA channel slope and bearing metrics:
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  chan <- odbcConnect('NRSA2')
  on.exit(close(chan))
  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(is.character(thal)) return(thal)

  chanGeom <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if(is.character(chanGeom)) return(chanGeom)

  protocols <- siteProtocol(c(unique(chanGeom$UID), unique(thal$UID)))

  mets <- metsSlopeBearing.1(thal, chanGeom, protocols)
  if(is.character(mets)) return(mets)

  rc <- writeNRSACalcResults(mets, 'metsSlopeBearing.csv')
  return(rc)
}

metsSlopeBearing.1 <- function(thal, chanGeom, protocols)
# Does the work for for metsSlopeBearing
#
# ARGUMENTS:
# thal        dataframe with thalweg data table
# chanGeom    dataframe with channel geometry table
# protocols   dataframe with protocol (WADEABLE, BOATABLE) used for each UID
#
# rm(dists,sbWadeable,sbBoatable,sb,nsta,tt,sumDists,tsb,transpc,tranEast, tranNorth, tranSlope, tran,totEast, totNorth, fishDist, nEast, nNorth, reach, xslope, vslope, nslp, reach, xbearing, sinu, mets)
{
  intermediateMessage('Slope and bearing calculations', loc='start')

  # Get expected parameters from each dataframe for main channel only.  Calculate
  # transect spacing in wadeables (like ACTRANSP in boatable reaches) as the
  # expected number of stations in the transect times INCREMNT (the distance
  # between adjacent stations) in that transect.  Calculate backsighting
  # percentages for the boatable reaches based on the backsighted distances.
  # The boatable parameter ACTRANSP is ignored in favour of the distances over
  # which backsightings are made.
  dists <- subset(thal
                 ,PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0
                  & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                 ,select=c(UID,PARAMETER,RESULT)
                 )

  sbWadeable <- subset(chanGeom
                      ,PARAMETER %in% c('BEARING','BEARING2','BEARING3'
                                       ,'PROP','PROP2','PROP3'
                                       ,'SLOPE','SLOPE2','SLOPE3'
                                       )
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                      ,select=c(UID,TRANSECT,PARAMETER,RESULT,UNITS)
                      )

  sbBoatable <- subset(chanGeom
                      ,PARAMETER %in% c('BEAR','SLOPE','DISTANCE')
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID
                      ,select=c(UID,TRANSECT,LINE,PARAMETER,RESULT,UNITS)
                      )
  sbBoatable$LINE <- as.numeric(as.character(sbBoatable$LINE))
  sbBoatable$RESULT <- as.numeric(as.character(sbBoatable$RESULT))


  intermediateMessage('.1')

  #########################################################################
  # Organize for wadeable and boatable reaches into a single structure with the
  # following columns:
  # UID         Unique Identifier
  # TRANSECT    A-K
  # LINE        a numeric value = 0 for the main reading, 1, 2 or 3 thereafter.
  #               This value will be NA for TRANSPC, which is not associated
  #               with a specific line on the form.
  # PARAMETER   with values BEARING, SLOPE, PROPORTION, TRANSPC
  # RESULT      The numeric value of the measured parameter.
  # UNITS       The measurement units, CM or PERCENT for slopes, NONE for others.
  
  # Calculate transect spacing TRANSPC in wadeable reaches.  Include TRANSPC
  # as a parameter in the wadeable data. Fill in LINE and standardize PARAMETER
  # values.
  dists$RESULT <- as.numeric(as.character(dists$RESULT))
  newDists <- NULL
  if (nrow(dists)>0){
      # Calculate transect spacing for wadeable sites
      nsta <- nWadeableStationsPerTransect(thal)
#      newDists <- merge(subset(dists, STATION==0, select=-STATION)
#                       ,nsta
#                       ,by=c('UID','TRANSECT')
#                       )
      newDists <- merge(dists, nsta, by='UID', all.x=TRUE)

      newDists$TRANSPC <- as.character(newDists$nSta * as.numeric(newDists$RESULT))
      newDists <- newDists[c('UID','TRANSECT','TRANSPC')]
  }
  
  # Create LINE value based on PARAMETER, then standardize PARAMETER values
  sbWadeable$LINE <- ifelse(sbWadeable$PARAMETER %in% c('PROP','SLOPE','BEARING'), 0
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP2','SLOPE2','BEARING2'), 1
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP3','SLOPE3','BEARING3'), 2
                    ,NA
                    )))
  sbWadeable$PARAMETER <- ifelse(substr(sbWadeable$PARAMETER,1,4) == 'PROP', 'PROPORTION'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'SLOP', 'SLOPE'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'BEAR', 'BEARING'
                         ,NA
                         )))
  intermediateMessage('.2')
  

  # Calculate transect spacing TRANSPC from incremental DISTANCE values.
  # Calculate incremental proportion values from DISTANCE and TRANSPC.
  # Handle TRANSPC as a parameter in a separate dataframe.
  tt <- subset(sbBoatable, PARAMETER=='DISTANCE')

  sumDists <- NULL
  if (nrow(tt)>0){
      sumDists <- aggregate(list('transpc'=tt$RESULT)
                           ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT)
                           ,sum, na.rm=TRUE
                           )

      sbBoatable <- merge(sbBoatable, sumDists, c('UID', 'TRANSECT'))

      sbBoatable$RESULT <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                 ,100 * sbBoatable$RESULT/sbBoatable$transpc
                                 ,sbBoatable$RESULT
                                 )
      sbBoatable$transpc <- NULL

      sumDists$TRANSPC <- as.character(sumDists$transpc)
      sumDists$transpc <- NULL

      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                ,'PROPORTION'
                                ,sbBoatable$PARAMETER
                                )
      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='BEAR'
                                    ,'BEARING'
                                    ,sbBoatable$PARAMETER
                                    )
      sbBoatable$LINE <- ifelse(is.na(sbBoatable$LINE), 0, sbBoatable$LINE - 1 )

      sumDists <- sumDists[c('UID','TRANSECT','TRANSPC')]
  }

  tsb <- rbind(sbWadeable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              ,sbBoatable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              )

  transpc <- rbind(newDists, sumDists)

  intermediateMessage('.3')

  # Transpose to wide format for intermediate calculations at each transect
  # Put TRANSPC values on every row, regardless of LINE value.
  # NOTE: Wadeable slopes (SLOPE_ND at least) have units = 'NONE', but are
  # expressed in percent, and this is reflected in the code below.
  sb <- reshape(tsb
               ,idvar=c('UID','TRANSECT','LINE')
               ,direction='wide'
               ,timevar='PARAMETER'
               )
  sb <- rename(sb, names(sb), sub('RESULT\\.(\\1)', '\\1', names(sb)))
  sb <- merge(sb, transpc, by=c('UID','TRANSECT'), all.x=TRUE)
  sb$BEARING <- as.numeric(sb$BEARING)
  sb$PROPORTION <- as.numeric(sb$PROPORTION)
  sb$TRANSPC <- as.numeric(sb$TRANSPC)
  sb$SLOPE <-  ifelse(sb$UNITS.SLOPE %in% c('PERCENT','NONE')
                     ,as.numeric(sb$SLOPE)
                     ,ifelse(sb$UNITS.SLOPE == 'CM'
                            ,100*as.numeric(sb$SLOPE)/(sb$TRANSPC * sb$PROPORTION)
                            ,NA
                            )
                     )
  sb$UNITS.SLOPE <- ifelse(sb$UNITS.SLOPE == 'CM', 'PERCENT', sb$UNITS.SLOPE)
  intermediateMessage('.4')

  #########################################################################
  # Intermediate calculations over single transect
  # tranEast   distance East traveled from this transect to the next
  #              = sum( sin(BEARINGInRadians) * DISTANCE )
  # tranNorth  distance North traveled from this transect to the next
  #              = sum( cos(BEARINGInRadians) * DISTANCE )
  # transpc    distance along channel from this transect to the next one
  # tranSlope  mean slope between this transect and the next one, weighted by
  #              the proportions of the total distance between adjacent
  #              transects over which each slope measurement was taken.
  #              Transects which have no slope data have tranSlope set to NA;
  #              otherwise the NAs would sum to 0, dangitall.
  sb$lineEast <- sin(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineNorth <- cos(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineSlope <- sb$SLOPE * sb$PROPORTION/100

  tranEast <- aggregate(list('tranEast'=sb$lineEast)
                       ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                       ,sum, na.rm=TRUE
                       )
  tranNorth <- aggregate(list('tranNorth'=sb$lineNorth)
                        ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                        ,sum, na.rm=TRUE
                        )
  tranSlope <- merge(aggregate(list('tranSlope'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,sum, na.rm=TRUE
                              )
                    ,aggregate(list('n'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,count
                              )
                    ,c('UID','TRANSECT')
                    )
  tranSlope$tranSlope <- ifelse(tranSlope$n==0, NA, tranSlope$tranSlope)
  tranSlope$n <- NULL

  transpc <- rename(subset(sb, LINE==0, select=c(UID, TRANSECT, TRANSPC))
                   ,'TRANSPC', 'transpc'
                   )
  intermediateMessage('.5')

  tran <- merge(merge(tranEast, tranNorth, c('UID','TRANSECT'), all=TRUE)
               ,merge(tranSlope, transpc, c('UID','TRANSECT'), all=TRUE)
               ,c('UID','TRANSECT'), all=TRUE
               )

   intermediateMessage('.6')

  #########################################################################
  # Intermediate calculations over entire reach
  # totEast   distance East travelled from start to end of reach
  #
  # totNorth  distance North travelled from start to end of reach
  # fishDist  distance along channel from start to end of reach
  #             = sum(transect spacing)
  #             = sum(actransp)
  # crowDist  straight line distance from start to end of reach
  #             = sqrt(totEast^2 + totNorth^2)
  # nEast     count of tranEast values used to remove meaningless values of
  #             fishDist and crowDist
  # nNorth    count of tranNorth values used to remove meaningless values of
  #             fishDist and crowDist
  totEast <- aggregate(list('totEast'=tran$tranEast)
                      ,list('UID'=tran$UID)
                      ,sum, na.rm=TRUE
                      )
  totNorth <- aggregate(list('totNorth'=tran$tranNorth)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  fishDist <- aggregate(list('fishDist'=tran$transpc)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  nEast <- aggregate(list('nEast'=tran$tranEast)
                    ,list('UID'=tran$UID)
                    ,count
                    )
  nNorth <- aggregate(list('nNorth'=tran$tranNorth)
                     ,list('UID'=tran$UID)
                     ,count
                     )
  intermediateMessage('.7')

  reach <- merge(totEast
                ,merge(totNorth, fishDist, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach <- merge(reach
                ,merge(nEast, nNorth, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach$crowDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,sqrt(reach$totEast^2 + reach$totNorth^2)
                          ,NA
                          )
  reach$fishDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,reach$fishDist
                          ,NA
                          )
  intermediateMessage('.8')

  #########################################################################
  # Metrics calculations
  # xslope    mean of tranSlope
  # vslope    std deviation of tranSlope
  # nslp      count of tranSlope
  # transpc   mean distance between transects
  # xbearing  overall bearing of reach based the inverse cosine of the ratio
  #             totNorth/crowDist
  # sinu      sinuosity as the ratio fishDist/crowDist
  nslp <- aggregate(list('RESULT'=tran$tranSlope)
                   ,list('UID'=tran$UID)
                   ,count
                   )
  nslp$METRIC <- 'nslp'

  xslope <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,mean, na.rm=TRUE
                     )
  xslope$METRIC <- 'xslope'

  vslope <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,sd, na.rm=TRUE
                     )
  vslope$METRIC <- 'vslope'

  transpc <- aggregate(list('RESULT'=tran$transpc)
                      ,list('UID'=tran$UID)
                      ,mean, na.rm=TRUE
                      )
  transpc$METRIC <- 'transpc'

  reach$RESULT <- ifelse(reach$totEast > 0
                          ,(360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          ,360 - (360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          )
  xbearing <- reach[c('UID','RESULT')]
  xbearing$METRIC <- 'xbearing'

  reach$RESULT <- ifelse(reach$crowDist == 0
                        ,NA
                        ,reach$fishDist / reach$crowDist
                        )
  sinu <- reach[c('UID','RESULT')]
  sinu$METRIC <- 'sinu'

  mets <- rbind(xslope, vslope, nslp, transpc, xbearing, sinu)
  intermediateMessage('.9')



  #########################################################################
  # Clean up and convert NaN values to NA
  mets <- mets[c('UID','METRIC','RESULT')]
  mets$RESULT <- as.character(mets$RESULT)

  mets$RESULT <- ifelse(is.nan(as.numeric(mets$RESULT)), NA, mets$RESULT)

  # Filter out calculated values with too small of a sample size to be reliable.
  badSlopeUIDs <- unique(nslp[as.numeric(nslp$RESULT)<=2,]$UID)
  if(length(badSlopeUIDs)>0) {
      mets$RESULT <- ifelse(mets$METRIC=='xslope' & mets$UID %in% badSlopeUIDs
                           ,NA
                           ,mets$RESULT
                           )
      mets$RESULT <- ifelse(mets$METRIC=='vslope' & mets$UID %in% badSlopeUIDs
                           ,NA
                           ,mets$RESULT
                           )
  }

  intermediateMessage('.  Done.', loc='end')
  return(mets)
}


