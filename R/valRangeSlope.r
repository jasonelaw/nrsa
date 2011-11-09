# valRangeSlope.r
#
# NOTE: Check of slopes in cm requires prior validation of incremnt/transpc values.
#
# 05-04-10 cws created


valRangeSlope <- function(since=NULL)
# Performs range check on Slope information.  Assumes INCREMNT has already been
# verified.
#
# ARGUMENTS:
# since     Character string specifying the insertion time prior to which data
#             will not be included.  This string must be in an MS SQL datetime
#             format, e.g. '2010-04-01'. If NULL, all data will be included.
#
{
  chan <- odbcConnect('NRSA2')
  
  # Retrieve data and metadata.  Thalweg data needs INCREMNT for calculations,
  # and must contain at least one row per station at each site so the number
  # of stations at each transect can be determined to calculate slopes from
  # elevation measurements.  Since the thalweg table is so large, selecting
  # only a few parameters is done to save processing time and memory.
  cgWhere <- NULL
  thalWhere <- "PARAMETER in ('INCREMNT','DEPTH','DEP_POLE','DEP_SONR')"
  if(!is.null(since)) {
      cgWhere <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
      thalWhere <- paste(thalWhere, since, sep=' and ')
  }

  cg <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2', where=cgWhere)
  if(!is.data.frame(cg)) return(cg)

  thal <- fetchNRSATable(chan, 'tblTHALWEG2',where=thalWhere)
  if(!is.data.frame(thal)) return(thal)

  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2', filterSideChannels=FALSE)
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create range validation results
  validationResults <- valRangeSlope.1(cg, thal, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d out-of-range slope values", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valRangeSlopes'
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
}

valRangeSlope.1 <- function(cg, thal, meta.df, siteInfo)
# Do the validation work here.
#
# ARGUMENTS:
# cg        dataframe with slope data from channel geometry table.  All columns
#             should be present.
# thal      dataframe with station incremnt information in parameter INCREMNT.
#             Dataframe should also include at least one row per station at each
#             site.
{
  # Temporary fix for duplicate rows:
  cg <- unique(cg)
  thal <- unique(thal)
  
  # Reorganize slope & proportion data and calculate transect spacing as the
  # INCREMNT times the number of stations at a transect (nsta).
  nSta <- aggregate(list(nsta=thal$STATION)
                   ,list(UID=thal$UID, TRANSECT=thal$TRANSECT)
                   ,function(x) { count(unique(x)) }
                   )

  cgSlopes <- subset(cg, grepl('SLOPE.*',PARAMETER)
                    ,select=c(UID,TRANSECT,PARAMETER,RESULT, UNITS)
                    )
  cgSlopes$subsight <- ifelse(cgSlopes$PARAMETER=='SLOPE', 1
                      ,ifelse(cgSlopes$PARAMETER=='SLOPE2', 2
                      ,ifelse(cgSlopes$PARAMETER=='SLOPE3', 3, NA)))
  cgSlopes$subSlope <- as.numeric(cgSlopes$RESULT)
  cgSlopes$PARAMETER <- NULL
  cgSlopes$RESULT <- NULL

  cgProps <- subset(cg, grepl('PROP.*',PARAMETER)
                    ,select=c(UID,TRANSECT,PARAMETER,RESULT)
                    )
  cgProps$subsight <- ifelse(cgProps$PARAMETER=='PROP', 1
                      ,ifelse(cgProps$PARAMETER=='PROP2', 2
                      ,ifelse(cgProps$PARAMETER=='PROP3', 3, NA)))
  cgProps$subProp <- as.numeric(cgProps$RESULT)
  cgProps$PARAMETER <- NULL
  cgProps$RESULT <- NULL

  ss <- merge(merge(cgSlopes, cgProps, by=c('UID','TRANSECT','subsight'))
             ,subset(thal, STATION=='0' & TRANSECT=='A' & PARAMETER=='INCREMNT'
                    ,select=c(UID,RESULT)
                    )
             ,by=c('UID')
             )
  ss <- rename(ss, 'RESULT', 'incremnt')
  ss$incremnt <- as.numeric(ss$incremnt)
  ss <- merge(ss, nSta, by=c('UID','TRANSECT'))

  # Calculate slopes at each subsighting (slopes in percent are rather easy)
  ss$slope <- ifelse(ss$UNITS %in% c('PERCENT','NONE')
                    ,as.numeric(ss$subSlope)
                    ,ifelse(ss$UNITS=='CM'
                           ,100 * (ss$subSlope/100) / ((ss$nsta * ss$incremnt) * (ss$subProp/100) )
                           ,-99999
                           )
                    )
  ss$slope <- ifelse(ss$slope %in% c(Inf, -Inf), NA, ss$slope)
  
  # Look at distribution of slopes with different methods and recorded units.
  #with(ss, table(METHOD, cut(slope, c(0,5,10,15,20,25,30,max(slope,na.rm=TRUE)), include.lowest=TRUE), useNA='ifany'))
  #with(ss, table(UNITS, cut(slope, c(0,5,10,15,20,25,30,max(slope,na.rm=TRUE)), include.lowest=TRUE), useNA='ifany'))

  # select large slopes for validation and recreate slope parameter from subsight
  ss <- subset(ss, slope > 15)
  ss$PARAMETER <- ifelse(ss$subsight==1, 'SLOPE'
                 ,ifelse(ss$subsight==2, 'SLOPE2'
                 ,ifelse(ss$subsight==3, 'SLOPE3',NA)))
  ss$TESTDESCRIPTION <- ifelse(ss$UNITS %in% c('PERCENT','NONE')
                              ,"Percent slope is rather high"
                              ,ifelse(ss$UNITS=='CM'
                                     ,sprintf("Elevation results in slope of %2.2fcm/%2.1fm = %2.2f"
                                             ,as.numeric(ss$subSlope)
                                             ,((ss$nsta * ss$incremnt) * (ss$subProp/100) )
                                             ,ss$slope
                                             )
                                     ,"Unexpected UNITS!!!"
                                     )
                              )

  # select rows in the channel geometry data that match those selected for
  # validation.
  vv <- merge(cg, ss[c('UID','TRANSECT','PARAMETER','TESTDESCRIPTION')]
             ,by=c('UID','TRANSECT','PARAMETER')
             )

  # Construct validation results
  rr <- constructNRSAValidationResults(vv, meta.df, siteInfo)

  return(rr)
}
