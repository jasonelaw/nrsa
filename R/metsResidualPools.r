# metsResidualPools.r
#
#  1/29/10 cws Created
#  2/11/10 cws Using nWadeableStationsPerTransect() to estimate the number of
#          stations expected in each transect at a wadeable reach.  This small
#          function was created to allow use in slope and bearing metrics.
# 03/10/10 cws Updated as needed for change in nWadeableStationsPerTransect().
# 03/11/10 cws Using readNRSACalculationResults() to read metsSlopeBearing.csv
# 03/22/10 cws Moved all unit test dataframes to separate functions
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
# 04/01/10 cws Removing extra print() statements and commented-out code.
#          Modified unit test to try data with just one protocol instead of both.
#  4/13/10 cws Converting calculation to use xdepth and sddepth values in cm
#          instead of m, as requested, adding protocols argument to
#          metsResidualPools.siteSummaries(); modified unit test accordingly.
#
# Note: A value for reachlen is calculated based on the sum of INCREMNT values
#       in stationInfo, which is a dataframe created and returned by
#       metsResidualPools.dimensions().  That function uses the output of
#       metsResidualPools.dataOrganization(), which takes the value of INCREMNT
#       at the first station in the reach and propagates it to all stations,
#       hence the result is independent of whether INCREMNT occurs more than
#       once in the thalweg table.
#
#
require(RODBC)
require(RUnit)

metsResidualPools <- function()
# Calculate residual pool metrics
#
# These metrics are saved to a csv file inthe directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success, or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Read in thalweg data and retain only required parameters
  chan <- odbcConnect('NRSA2')
  on.exit(close(chan))
  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(is.character(thal)) return(thal)
  
  thal <- subset(thal
                ,PARAMETER %in% c('BARWIDTH', 'BAR_PRES', 'CHANUNCD', 'DEPTH'
                                 ,'DEP_POLE', 'DEP_SONR', 'INCREMNT'
                                 ,'POOLFMCD', 'REACHLENGTH', 'SEDIMENT'
                                 ,'OFF_CHAN', 'WETWIDTH'
                                 )
                )

  # Read in channel geometry data and retain only required parameters
  actransp <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if(is.character(actransp)) return(actransp)

  actransp <- subset(actransp, PARAMETER %in% c('ACTRANSP'))

  # Read in slope metrics and retain only required calculations
  slopes <- readNRSACalculationResults('metsSlopeBearing.csv')
  if(is.character(slopes)) return(slopes)
  
  slopes <- subset(slopes, METRIC %in% c('xslope','vslope'))

  # Determine protocols used for each site
  protocols <- siteProtocol(unique(thal$UID))
  
  # Calculate rp mets and write them to file.
  mets <- metsResidualPools.1(thal, actransp, slopes, protocols)
  if(is.character(mets)) return(mets)
  
  rc <- writeNRSACalcResults(mets, 'metsResidualPools.csv')
  return(rc)
}

metsResidualPools.1 <- function(thal, actransp, slopes, protocols)
# Does the actual calculations for metsResidualPools()
#
# ARGUMENTS:
# thal      dataframe with thalweg parameters
# actransp  dataframe with actual transect space values (ACTRANSP)
# slopes    dataframe with reach slope mean and stdev values (xslope, vslope)
# protocols dataframe with sampling protocol used at each site
{

  # Remove side channel transects until we know how to deal with them
  thal <- subset(thal, TRANSECT %in% LETTERS)

  # Organize the data for both protocols in a single and simple manner.
  thalSeries <- metsResidualPools.dataOrganization(thal, actransp, slopes)

  # Determine residual depths and number pools
  residualSeries <- metsResidualPools.dimensions(thalSeries, protocols)

  # Characterize individual residual pools
  poolSeries <- metsResidualPools.poolCharacteristics(residualSeries)
  
  # Summarize pool structure for each UID
  wide <- metsResidualPools.siteSummaries(poolSeries, residualSeries
                                         ,protocols
                                         )

  # transpose to long format
  mets <- dfLengthen(wide, 'UID', 'METRIC', 'RESULT'
                    ,names(wide)[!(names(wide) %in% 'UID')]
                    )
  
  return(mets)
  
}

metsResidualPools.dataOrganization <- function(thal, actransp, slopes)
# Reformats wadeable and boatable reach data into the following format, which
# is returned as a dataframe.  If an error occurs, the return value will be a
# character string describing the problem.
#
# a) columns are UID, TRANSECT, STATION, LOC, DEPTH, INCREMNT
#    where LOC is the numeric position of the depth sample within the reach
#              starting at 1 and increasing by 1 per expected station.  Thus
#              in a wadeable reach at A 0, LOC will be 1 and at A 2 will be 3
#              regardless of whether we have a depth at A 1.  The nickpoint
#              added later will be at LOC = 0.
#          DEPTH is the thalweg depth in meters.
#          INCREMNT is the distance in meters from this station to the next
#              expected station.  This is recorded directly in the wadeable
#              protocol, and is calculated as actransp/numberOfStations for
#              boatable reaches.
# b) rows are in order from downstream to upstream (wadeable reaches will
#    be unaffected, boatable reaches will be reversed)
# c) A downstream 'nick point' will be established at LOC 0 for each UID.
#    and have an arbitrary depth of xdepth - sddepth.  This is added to each
#    reach to insure capture of the first pool.
#
# ARGUMENTS:
# thal      dataframe with thalweg parameters
# actransp  dataframe with actual transect space values (ACTRANSP)
# slopes    dataframe with reach slope mean and stdev values (xslope, vslope)
#
# ASSUMPTIONS:
# The maximum station number found in a wadeable reach is the intended number
#   of stations to sample.  In boatable reaches, the intended number is
#   whatever the actual number is.
# Station numbers for a transect start at 0.
# Wadeable incremnt value for reach is constant and stored at the first row
#   of the reach.
# The siteProtocol function for 2008, 2009 data is incorrect for UID 12475,
#   12498, 13049, 13941,  so the SAMPLE_TYPE column will be relied upon for
#   this function
{
  # Work on wadeable reaches: extract DEPTH, create LOC, create INCREMNT.
  wadeable <- subset(thal, SAMPLE_TYPE=='PHAB_THALW')
  if(nrow(wadeable) == 0) {
      rawStreams <- NULL
  } else {
      # Wadeable DEPTH is converted to meters, from cm.
      rawData <- subset(wadeable, PARAMETER=='DEPTH'
                       ,select=c(UID,TRANSECT,STATION,RESULT)
                       )
      rawData <- rename(rawData, 'RESULT', 'DEPTH')
      rawData$DEPTH <- as.numeric(rawData$DEPTH) / 100
  
      # Wadeable LOC is order of transect(A=0, B=1, etc) * number of stations
      # expected in each transect + the station number + 1 since station
      # numbering starts at zero.
      nSta <- nWadeableStationsPerTransect(rawData)
      rawData <- merge(rawData, nSta, by=c('UID','TRANSECT'), all.x=TRUE)
      rawData$LOC <- (match(rawData$TRANSECT, LETTERS) - 1) * rawData$nSta + rawData$STATION + 1
      rawData$LOC <- as.integer(rawData$LOC)
      rawData <- subset(rawData, select=-nSta)

      # Wadeable INCREMNT is the recorded value of incremnt in meters. It is taken
      # from the first row of each reach, and propagated to all rows in the reach.
      wadeable <- wadeable[order(wadeable$UID, wadeable$TRANSECT, wadeable$STATION),]
      incremnt <- first(subset(wadeable, PARAMETER=='INCREMNT'), 'UID', 'firstRow')
      incremnt <- subset(incremnt, firstRow==TRUE, select=c(UID,RESULT))
      incremnt <- rename(incremnt, 'RESULT','INCREMNT')
      rawStreams <- merge(rawData, incremnt, by='UID')
      rawStreams$INCREMNT <- as.numeric(rawStreams$INCREMNT)
      rm(rawData, nSta, wadeable, incremnt)
  }

  # Work on boatable reaches: calculate DEPTH in m, create LOC, create INCREMNT
  boatable <- subset(thal, SAMPLE_TYPE=='PHAB_THAL')
  
  if(nrow(boatable) == 0) {
      rawRivers <- NULL
  } else {
      # Boatable DEPTH is converted to meters if it was recorded in feet.
      rawData <- subset(boatable, PARAMETER %in% c('DEP_SONR','DEP_POLE')
                       ,select=c(UID,TRANSECT,STATION,RESULT,UNITS)
                       )
      rawData$RESULT <- as.numeric(ifelse(rawData$RESULT=='.', NA, rawData$RESULT))
      rawData$DEPTH <- NA
      rawData$DEPTH <- ifelse(rawData$UNITS=='M',  rawData$RESULT
                      ,ifelse(rawData$UNITS=='FT', rawData$RESULT * 0.3048
                             ,NA
                      ))
      rawData <- subset(rawData, select=-c(UNITS,RESULT))

      # Boatable LOC is more complicated than for wadeable reaches since the number
      # of stations per transect will often vary through a reach, and it is
      # ultimately reversed so that it increases in direction, as
      # the wadeable LOC does.  The reversed LOC (revLOC = LOC moving downstream) is
      # calculated first as the cumulative station count up to the beginning of the
      # transect + the order of transect(A=1, B=2, etc) + station.  LOC is then
      # reversed as the max(revLOC) - revLOC + 1.
      nSta <- aggregate(list('lastSta'=rawData$STATION)
                       ,list('UID'=rawData$UID, 'TRANSECT'=rawData$TRANSECT)
                       ,max, na.rm=TRUE
                       )
      nSta <- nSta[order(nSta$UID, nSta$TRANSECT),]
      nSta$cumSta <- ave(nSta$lastSta, nSta$UID, FUN=cumsum)
      rawData <- merge(rawData, nSta, by=c('UID','TRANSECT'))
      rawData$revLOC <- rawData$cumSta - rawData$lastSta +
                        match(rawData$TRANSECT, LETTERS) + rawData$STATION
      maxLOC <- aggregate(list('maxLOC'=rawData$revLOC)
                         ,list('UID'=rawData$UID)
                         ,max, na.rm=TRUE
                         )
      rawData <- merge(rawData, maxLOC, by='UID')
      rawData$LOC <- as.integer(rawData$maxLOC - rawData$revLOC + 1)

      # Boatable INCREMNT is calculated as the actual transect spacing recorded
      # at each transect divided by the number of stations sampled in that transect.
      rawRivers <- merge(rawData, actransp[c('UID','TRANSECT','RESULT')]
                        ,by=c('UID','TRANSECT'), all.x=TRUE
                        )
      rawRivers$INCREMNT <- as.numeric(rawRivers$RESULT) / (rawRivers$lastSta + 1)
      rawRivers <- subset(rawRivers, select=-c(lastSta, cumSta, revLOC, maxLOC, RESULT))
      rm(boatable, nSta, maxLOC)
  }

  if(is.null(rawStreams) & is.null(rawRivers)) {
      # No boatable or wadeable reaches.  Darn!
      return("There are no wadeable nor boatable reaches in the data")
  } else {
      # Rejoin rivers and streams
      thalSeries <- rbind(rawStreams, rawRivers)
  }

  # Include mean slopes, modified with Stack (1989) equation.
  slopes <- subset(slopes, METRIC=='xslope')
  slopes$stackSlope <- 0.12 + 0.25 * as.numeric(slopes$RESULT)
  thalSeries <- merge(thalSeries, slopes[c('UID','stackSlope')], by='UID', all.x=TRUE)


  # Create and include downstream nickpoints
  dMean <- aggregate(list('dMean'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                    ,mean, na.rm=TRUE
                    )
  dStdev <- aggregate(list('dStdev'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                     ,sd, na.rm=TRUE
                     )
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC),]
  nicks <- subset(first(thalSeries, 'UID', 'first'), first=='TRUE')
  nicks <- merge(nicks, dMean, by='UID', all.x=TRUE)
  nicks <- merge(nicks, dStdev, by='UID', all.x=TRUE)
  nicks$LOC <- as.integer(0)
  nicks$DEPTH <- nicks$dMean - nicks$dStdev
  nicks$DEPTH <- ifelse(nicks$DEPTH < 0, 0, nicks$DEPTH)

  thalSeries <- rbind(thalSeries, nicks[names(thalSeries)])

  # Final organization details
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC),]
  rownames(thalSeries) <- NULL
  thalSeries$STATION <- as.integer(thalSeries$STATION)

  return(thalSeries)
}


metsResidualPools.dimensions <- function(thalSeries, thalProtocol
                                        ,minSampPct=85, oldeMethods=FALSE
                                        )
# Detect residual pools by calculating residual depths and numbering pools.
# Return dataframe with residual pools and pool numbers added to input dataframe
# or return a character string describing the error if one occurs.
#
# The dataframe with these initial pool dimensions will have the following
# information: in addition to UID, TRANSECT, STATION, LOC, DEPTH, INCREMNT
# and stackSlope delivered in the input dataframe thalSeries, the following
# calculations are returned as well:
#   resDepth      residual pool depth at this station.  If zero, there is no
#                   pool.
#   resArea       residual sagittal area between this station and the previous
#                   station.
#   resLength     residual pool length between this station and the previous
#                   station.
#   poolID        pool identification number for the current site, starting
#                   at 1.  If zero, there is no residual pool at this station.
#
# ARGUMENTS:
# thalSeries   dataframe with thalweg depth information in the expected format
#              as created by metsResidualPools.dataOrganization().
# thalProtocol dataframe with sampling protocol used at each site.  Used in
#              conjunction with oldeMethods argument.
# minSampPct   numeric value specifying the minimum percentage of depths that
#              must be present to reliably detect residual pools, as stations
#              with missing depths are difficult to associate with a specific
#              pool.
# oldeMethods  logical flag specifying if old (SAS/EMAP) method of calculating
#              residual pools is used.  This affects only wadeable reaches.
#              The unit test for this function is based on SAS calculations, so
#              this flag should be TRUE for testing.
#
{
  intermediateMessage('starting metsResidualPools.dimensions', loc='start')

  # Pool detection and numbering starts at beginning of a reach
  thalSeries <- first(thalSeries[order(thalSeries$UID, thalSeries$LOC),]
                     ,'UID'
                     ,'siteStart'
                     )

  # Create columns for residual dimensions here, and fill them in later
  thalSeries$resDepth <- as.numeric(NA)
  thalSeries$resArea <- as.numeric(NA)
  thalSeries$resLength <- as.numeric(NA)
  thalSeries$poolID <- as.numeric(NA)

  # Calculate residual dimensions at each station (LOC) along the thalweg
  for(i in 1:nrow(thalSeries)) {
      if(thalSeries[i,]$siteStart) {
          intermediateMessage(paste('.', as.character(i), sep=''))

          # initialize counts and such at site nickpoint
          poolBaseDepth <- thalSeries[i,]$DEPTH
          poolID <- 0
          poolLen <- 0
          inPool <- FALSE
          
          # Determine protocol used for this site.  If this is not determinable
          # then assume it to be wadeable.
          pp <- subset(thalProtocol, UID==thalSeries[i,]$UID)
          if(nrow(pp) ==1) {
              isWadeable <- pp$PROTOCOL=='WADEABLE'
          } else {
              isWadeable <- TRUE
          }
          
      } else {
          # Go through site detecting residual depths and related dimensions
          # for each station (LOC) in the site.  The residual depth is the depth
          # of the pool at that point if no water flowed.  Mathematically,
          # residual depth = depth - (poolBaseDepth + poolLen * stackSlope/100)
          #   where poolBaseDepth = depth of downstream lip of pool
          #         poolLen       = distance from the downstream lip of pool to
          #                         current station
          #         stackSlope    = channel slope (in percent), modified using
          #                         Stack (1989) equation.  Dividing by 100
          #                         converts from % slope to a tangent.
          #
          # Identify succeeding pools by counting them in poolID.
          thalSeries[i,]$resLength <- thalSeries[i,]$INCREMNT *
                                     (thalSeries[i,]$LOC - thalSeries[i-1,]$LOC)

          poolLen <- poolLen + thalSeries[i,]$resLength
          if(oldeMethods & isWadeable & thalSeries[i-1,]$siteStart) {
              # In previous rp code, channel length was 0 at start of reach.
              # This results in incorrect calculation in the first position, but
              # is retained here for testing.
              poolLen <- ifelse(is.na(thalSeries[i,]$INCREMNT), NA, 0)
          }
          thalSeries[i,]$resDepth <- thalSeries[i,]$DEPTH -
                                     (poolBaseDepth +
                                      poolLen * thalSeries[i,]$stackSlope/100
                                     )

          if(is.na(thalSeries[i,]$resDepth)) {
              # If residual depth is incalculable, so are the other dimensions.
              thalSeries[i,]$resLength <- NA

          } else if(thalSeries[i,]$resDepth > 0) {
              # This station is part of the current pool.  Increment pool
              # counter if the previous station was not in a pool, and then
              # calculate incremental residual dimensions.  This will also
              # affect the first residual area calculation at each reach.

              if(!inPool) {
                  inPool <- TRUE
                  poolID <- poolID + 1
              }
              thalSeries[i,]$resArea <- thalSeries[i,]$resDepth *
                                       thalSeries[i,]$resLength
              if(oldeMethods & isWadeable & thalSeries[i-1,]$siteStart)
                  thalSeries[i,]$resArea <- 0
                  
              thalSeries[i,]$poolID <- poolID
              
          } else {
              # This station is not in a pool, so reset incremental residual
              # dimensions and get ready to detect next pool
              thalSeries[i,]$resDepth <- 0
              thalSeries[i,]$resArea <- 0
              thalSeries[i,]$resLength <- 0
              thalSeries[i,]$poolID <- 0

              inPool <- FALSE
              poolBaseDepth <- thalSeries[i,]$DEPTH
              poolLen <- 0
          }

      } # end of actions at each station

  } # end of loop through each row
  
  intermediateMessage('.cleaning up')
  
  # Go through calculations and set values to missing in those sites with
  # sparse sampling, as defined by minSampPct.  The sampled percentage for a
  # site is based on the number of expected depth values and the number that
  # are missing; the number of missing values:
  #    sampPct = 100 * (nPresent - nDepthMissing) / nExpected
  #
  tt <- aggregate(list('nExpected'=thalSeries$LOC), list('UID'=thalSeries$UID)
                 ,max, na.rm=TRUE
                 )
  mm <- aggregate(list('nMissing'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                 ,function(x) { sum(is.na(x)) }
                 )
  pp <- aggregate(list('nPresent'=thalSeries$LOC), list('UID'=thalSeries$UID)
                 ,count
                 )
  tt <- merge(tt, merge(mm, pp, by='UID'), by='UID')
  tt$sampPct <- 100 * (tt$nPresent - tt$nMissing) / tt$nExpected
  tt$keep <- (tt$sampPct >= minSampPct)
  
  thalSeries <- merge(thalSeries, subset(tt, select=c(UID,keep)), by='UID')
  thalSeries$resDepth <- ifelse(thalSeries$keep, thalSeries$resDepth, NA)
  thalSeries$resArea <- ifelse(thalSeries$keep, thalSeries$resArea, NA)
  thalSeries$resLength <- ifelse(thalSeries$keep, thalSeries$resLength, NA)
  thalSeries$poolID <- ifelse(thalSeries$keep, thalSeries$poolID, NA)
  thalSeries$keep <- NULL

  intermediateMessage('. Finished.', loc='end')
  return(thalSeries)
}


metsResidualPools.poolCharacteristics <- function(poolDims)
# Summarizes individual residual pools, based on the station by station
# dimensions previously calculated.  Returns dataframe of individual pool
# summaries if successful, or a character string describing the error if one
# occurs.
#
# ARGUMENTS:
# poolDims   dataframe with residual pool dimensions as calculated by
#            metsResidualPools.dimensions().
#
{
  intermediateMessage('starting metsResidualPools.poolCharacteristics', loc='start')
  
  # residual area summaries
  poolar <- aggregate(list('poolar'=poolDims$resArea)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )

  intermediateMessage('.1')

  # residual length summaries
  poolen <- aggregate(list('poolen'=poolDims$resLength)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )
  intermediateMessage('.2')

  # residual depth summaries
  mindep <- aggregate(list('mindep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,min, na.rm=TRUE
                     )

  xdep <- aggregate(list('xdep'=poolDims$resDepth)
                   ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                   ,mean, na.rm=TRUE
                   )
  rpvdep <- aggregate(list('rpvdep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sd, na.rm=TRUE
                     )
  rpmxdep <- aggregate(list('rpmxdep'=poolDims$resDepth)
                      ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                      ,max, na.rm=TRUE
                      )
  meddep <- aggregate(list('meddep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,median, na.rm=TRUE
                     )
  dep25 <- aggregate(list('dep25'=poolDims$resDepth)
                    ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.25, na.rm=TRUE, type=2
                    )
  dep75 <- aggregate(list('dep75'=poolDims$resDepth)
                    ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.75, na.rm=TRUE, type=2
                    )
  intermediateMessage('.3')

  # Combine summaries for output
  poolCharacteristics <- merge(poolar, poolen, by=c('UID','poolID'), all=TRUE)
  poolCharacteristics <- merge(poolCharacteristics, mindep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, xdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpvdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpmxdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, meddep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep25
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep75
                              ,by=c('UID','poolID'), all=TRUE
                              )
                              
  poolCharacteristics <- subset(poolCharacteristics, poolID != 0)

  intermediateMessage('. Finished.', loc='end')
  return(poolCharacteristics)

}


metsResidualPools.siteSummaries <- function(poolInfo, stationInfo, protocols)
# Calculates site summaries from residual pool summaries as calculated by
# metsResidualPools.poolCharacteristics().  Returns dataframe of results if
# successful, or a character string describing the error if one occurs.
#
# ARGUMENTS:
# poolInfo      dataframe of residual pool summaries
# stationInfo   dataframe of residual depths and such at each station along a
#                 reach.
# protocols dataframe with sampling protocol used at each site
#
{
  intermediateMessage(' starting metsResidualPools.siteSummaries ', loc='start')
  # pool length summaries
  rpxlen <- aggregate(list('rpxlen'=poolInfo$poolen)
                     ,list('UID'=poolInfo$UID)
                     ,mean, na.rm=TRUE
                     )
  rpvlen <- aggregate(list('rpvlen'=poolInfo$poolen)
                     ,list('UID'=poolInfo$UID)
                     ,sd, na.rm=TRUE
                     )
  rpmxlen <- aggregate(list('rpmxlen'=poolInfo$poolen)
                      ,list('UID'=poolInfo$UID)
                      ,max, na.rm=TRUE
                      )
  totplen <- aggregate(list('totplen'=poolInfo$poolen)
                      ,list('UID'=poolInfo$UID)
                      ,sum, na.rm=TRUE
                      )
  intermediateMessage('.1')
  
  # pool depth summaries: summarize individual residual depths, then have a
  # look at the maximum depths of each pool, counting the numbers deeper than
  # certain values, and calculating a mean of those maximum depths for some
  # (intended to assist in discering 'real' pools from artifacts of the
  # calculations).
  # Note that the max() of a vector of length 0 (after NAs removed) returns
  # -Inf as the answer and writes warnings to the screen.  A bit of extra care
  # was thus needed for rpmxdep.
  # Convert depth summaries from m to cm here.
  rpxdep <- aggregate(list('rpxdep'=stationInfo$resDepth)
                     ,list('UID'=stationInfo$UID)
                     ,function(x) { mean(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpxdep$rpxdep <- ifelse(rpxdep$UID %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$UID
                         ,rpxdep$rpxdep * 100
                         ,rpxdep$rpxdep
                         )

  rpvdep <- aggregate(list('rpvdep'=stationInfo$resDepth)
                     ,list('UID'=stationInfo$UID)
                     ,function(x) { sd(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpvdep$rpvdep <- ifelse(rpvdep$UID %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$UID
                         ,rpvdep$rpvdep * 100
                         ,rpvdep$rpvdep
                         )

  rpmxdep <- aggregate(list('rpmxdep'=stationInfo$resDepth)
                      ,list('UID'=stationInfo$UID)
                      ,function(x) { max(ifelse(is.na(x),-Inf,x), na.rm=TRUE) }
                      )
  rpmxdep$rpmxdep <- ifelse(rpmxdep$rpmxdep==-Inf, NA, rpmxdep$rpmxdep)
  rpmxdep$rpmxdep <- ifelse(rpmxdep$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpmxdep$rpmxdep * 100
                           ,rpmxdep$rpmxdep
                           )

  rpgt50 <- aggregate(list('rpgt50'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.50) }
                     )
  rpgt75 <- aggregate(list('rpgt75'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.75) }
                     )
  rpgt100 <- aggregate(list('rpgt100'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { sum(x > 1.0) }
                      )

  rpgt05 <- aggregate(list('rpgt05'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.05) }
                     )
  rpgt05x <- aggregate(list('rpgt05x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.05, x, NA), na.rm=TRUE) }
                      )
  rpgt05x$rpgt05x <- ifelse(rpgt05x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt05x$rpgt05x * 100
                           ,rpgt05x$rpgt05x
                           )

  rpgt10 <- aggregate(list('rpgt10'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.10) }
                     )
  rpgt10x <- aggregate(list('rpgt10x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.10, x, NA), na.rm=TRUE) }
                      )
  rpgt10x$rpgt10x <- ifelse(rpgt10x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt10x$rpgt10x * 100
                           ,rpgt10x$rpgt10x
                           )

  rpgt20 <- aggregate(list('rpgt20'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.20) }
                     )
  rpgt20x <- aggregate(list('rpgt20x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.20, x, NA), na.rm=TRUE) }
                      )
  rpgt20x$rpgt20x <- ifelse(rpgt20x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt20x$rpgt20x * 100
                           ,rpgt20x$rpgt20x
                           )


  intermediateMessage('.2')
  
  # pool area summaries
  rpxarea <- aggregate(list('rpxarea'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,mean, na.rm=TRUE
                      )
  rpvarea <- aggregate(list('rpvarea'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,sd, na.rm=TRUE
                      )
  rpmxar <- aggregate(list('rpmxar'=poolInfo$poolar)
                     ,list('UID'=poolInfo$UID)
                     ,max, na.rm=TRUE
                     )
  areasum <- aggregate(list('areasum'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,sum, na.rm=TRUE
                      )
  intermediateMessage('.3')
  
  # rp100 calculations, finally.  Reachlength should not include the distance
  # from the nickpoint to the start of the reach.  It should also not include
  # the distance from the last station to the nonexistant next station.
  # Consequently the incremnt values associated with the nickpoint and last
  # station are removed.
  tt <- stationInfo[order(stationInfo$UID,stationInfo$LOC),]
  tt <- last(tt, 'UID', 'lastStation')
  tt <- subset(tt, LOC>0 & ! lastStation)
  
  
  reachlen <- aggregate(list('reachlen'=tt$INCREMNT)
                       ,list('UID'=tt$UID)
                       ,sum, na.rm=TRUE
                       )
  rp100 <- merge(areasum, reachlen, by='UID')
  rp100$rp100 <- ifelse(is.na(rp100$reachlen), NA
                       ,ifelse(rp100$reachlen==0, NA
                              ,100*rp100$areasum/rp100$reachlen
                              )
                       )
  rp100 <- subset(rp100, select=c(UID,rp100))
  intermediateMessage('.4')

  # combine summaries, setting missing counts to zero
  lengthMets <- merge(rpxlen, rpvlen, by='UID', all=TRUE)
  lengthMets <- merge(lengthMets, rpmxlen, by='UID', all=TRUE)
  lengthMets <- merge(lengthMets, totplen, by='UID', all=TRUE)
  
  depthMets <- merge(rpxdep, rpvdep, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpmxdep, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt50, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt75, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt100, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt05, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt05x, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt10, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt10x, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt20, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt20x, by='UID', all=TRUE)

  areaMets <- merge(rpxarea, rpvarea, by='UID', all=TRUE)
  areaMets <- merge(areaMets, rpmxar, by='UID', all=TRUE)
  areaMets <- merge(areaMets, areasum, by='UID', all=TRUE)
  
  mets <- merge(lengthMets, depthMets, by='UID', all=TRUE)
  mets <- merge(mets, areaMets, by='UID', all=TRUE)
  mets <- merge(mets, rp100, by='UID', all=TRUE)

  intermediateMessage('.  Done.', loc='end')
  return(mets)
}


