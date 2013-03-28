# valRangeThalwegSupplemental.r
#
# 12/16/09 cws Created to pare down the 9043 out of range DEP_SONR values down
#          to 111.  Might add more stuff later.
#  5/03/10 cws Added checks of incremnt against mean wetted width (wadeable
#          reaches).  Organized to separate neighbor based checks and incremnt
#          checks.  Removed code for deprecated supplemental check.
#          NOTE: TEMPORARILY REMOVED NEIGHBOR CHECKS FROM TEST.
#

valRangeThalwegSupplemental <- function(df)
# Performs supplemental range checks for tblTHALWEG2 in NRSA.
# Returns dataframe of validation results with the following columns: UID,
# TRANSECT, STATION, SAMPLE_TYPE, PARAMETER, RESULT, FLAG, UNITS,
# TESTDESCRIPTION.
#
# ARGUMENTS:
# df        dataframe with thalweg data to check.
#
{
  nn <- NULL
  nn <- valRangeThalwegSupplemental.neighborBasedChecks(df)
  ii <- NULL
  ii <- valRangeThalwegSupplemental.incremntCheck(df)

  supCheck <- rbind(nn, ii)
  if(nrow(supCheck) > 0) {
      supCheck <- supCheck[order(supCheck$UID, supCheck$TRANSECT, supCheck$STATION),]
  }
  
  return(supCheck)
}


valRangeThalwegSupplemental.neighborBasedChecks <- function(df)
# Performs neighbor based validation of depths and widths
{
  intermediateMessage('.n1')

  # Look for odd boatable sonar depths in M or FT
  tt <- subset(df, UNITS=='M' & PARAMETER=='DEP_SONR')
  mm <- NULL
  if(nrow(tt) > 0) {
      mm <- neightborBasedValidation(tt, c('UID','TRANSECT','STATION')
                                    ,'PARAMETER','RESULT','DEP_SONR', 5, 0.1, 30
                                    )
  }
  
  intermediateMessage('.n2')

  tt <- subset(df, UNITS=='FT' & PARAMETER=='DEP_SONR')
  ff <- NULL
  if(nrow(tt) > 0) {
      ff <- neightborBasedValidation(tt, c('UID','TRANSECT','STATION')
                                    ,'PARAMETER','RESULT','DEP_SONR', 5, 0.1, 100
                                    )
  }

  intermediateMessage('.n3')
  supCheck <- rbind(mm, ff)
  rm(mm, ff)


  # Look for odd wadeable depths -- all are in cm (at least theoretically)
  tt <- neightborBasedValidation(df, c('UID','TRANSECT','STATION')
                                ,'PARAMETER','RESULT','DEPTH', 10, 0, 150
                                )
  supCheck <- rbind(supCheck, tt)

  intermediateMessage('.n4')

  # Look for odd wadeable wetted widths -- all are in m, or should be
  tt <- neightborBasedValidation(df, c('UID','TRANSECT','STATION')
                                ,'PARAMETER','RESULT','WETWIDTH', 5, 0, 10
                                )
  supCheck <- rbind(supCheck, tt)

  intermediateMessage('.n5')

  # return amalgam of test result(s)
  return(supCheck)
}


valRangeThalwegSupplemental.incremntCheck <- function(df)
# Performs check of incremnt (distance between stations in wadeable reaches)
# against mean wetted width.  This requires additional data from outside
# the NRSA thalweg data.  Incremnt value is expected at A 0 of every reach,
# and is assumed to be uniform throughout the reach.
{
  intermediateMessage('.i1')
  chan <- odbcConnect('NRSA2')

  # Station incremnt values should be 0.4 times the channel width.  Get the
  # required information
  #  dfIncremnt <- fetchNRSATable(chan
  #                              ,'tblTHALWEG2'
  #                              ,where="PARAMETER='INCREMNT' AND STATION=0 AND TRANSECT='A'"
  #                              )
  dfIncremnt <- subset(df, PARAMETER=='INCREMNT' & STATION==0 & TRANSECT=='A')
  if(is.character(dfIncremnt)) {
       print(sprintf("Error fetching incremnt values: %s", dfIncremnt))
  }
  dfIncremnt <- unique(dfIncremnt)
  dfIncremnt$RESULT <- as.numeric(dfIncremnt$RESULT)

  intermediateMessage('.i2')
  dfWidth <- fetchNRSATable(chan, 'tblBANKGEOMETRY2', where="PARAMETER='WETWID'")
  if(is.character(dfWidth)) {
       print(sprintf("Error fetching incremnt values: %s", dfWidth))
  }

  xwidth <- aggregate(list(xwidth = dfWidth$RESULT)
                     ,list(UID = dfWidth$UID)
                     ,mean, na.rm=TRUE
                     )
  intermediateMessage('.i3')

  # Incremnt value should be at least 1 m, and otherwise within a factor of two
  # of 0.4 times the mean wetted width if the incremnt is more than 2 m. Anything
  # between 1 and 2 m is believable in wadeable reaches.
  tt <- subset(merge(dfIncremnt, xwidth, by='UID', all.x=TRUE)
              ,RESULT < 1 |
               (RESULT < 0.2 * xwidth | RESULT > 0.8 * xwidth) & (RESULT > 2)
              )

  if(nrow(tt) == 0) return(NULL)
  intermediateMessage('.i4')

  tt$TESTDESCRIPTION <- sprintf("Value should be about 0.4 times the mean width (%2.1f) = %2.2f"
                               ,tt$xwidth, 0.4*tt$xwidth
                               )

  tt$xwidth <- NULL

  return(tt)
}

# end of file