# valRangeCheck.r
#
# 10/30/09 cws Created
# 11/03/09 cws checked process with fake metadata.
# 11/12/09 cws Added use of SAMPLE_TYPE in metadata and standardized metadata
#          to upper case names.
# 12/16/09 cws Adding argument for supplemental range test function to allow
#          capability already present in validationRange() (SharedCode)
# 04/19/10 SOMEBODY TAKE CREDIT FOR CHANGING THIS
# 05/03/10 cws Temporarily modified to write only results of supplemental range
#          check on thalweg table: writing output to valRangeSupplementalThalweg.csv,
#          filters out results that don't start with "Value should be about" so
#          only incremnt checks are retained.
# 06/01/10 cws Undoing above temporary mod.
#

valRangeCheck <- function(tableName, supplementalTest=NULL, since=NULL)
# Performs range value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation
#
# Returns NULL on success, or character string describing error.
#
# ARGUMENTS:
# tableName Character string with name of table to do range validation on.
# supplementalTest Function that provides supplemental range checks on the data
# since     Character string specifying the insertion time prior to which data
#             will not be included.  This string must be in an MS SQL datetime
#             format, e.g. '2010-04-01'. If NULL, all data will be included.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valRangeCheck.cleanup(chan))

  if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='') 
  }
   
  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2', filterSideChannels=FALSE)
#  cna <- as.character(NA)
#  meta.df <- data.frame(parameter = c('BARWIDTH'  , 'WETWIDTH' , 'DEP_POLE' , 'DEP_POLE' , 'FOO')
#                       ,units     = c(cna         , cna        , 'm'        , 'ft'       , 'bar')
#                       ,rangeType = c(''          , ''         , ''         , ''         , 'TIME')
#                       ,rangeLow  = c(0           , 0          , 0          , 0          , '1600')
#                       ,rangeHigh = c(30          , 500.0      , 200        , 700        , '1800')
#                       ,formAbbr  = c('Thal'      , 'Thal'     , 'Thal'     , 'Thal'     , 'Other')
#                       ,SAMPLE_TYPE= c('PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'Other')
#                       ,stringsAsFactors=FALSE
#                       )
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create range validation results
  validationResults <- valRangeCheck.1(df, meta.df, siteInfo, supplementalTest)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d out-of-range values", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valRangeSupplemental'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
}

valRangeCheck.1 <- function(df, meta.df, siteInfo, supplementalTest)
# Does all the work
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  range.meta <- subset(meta.df
                      ,(!(RANGELOW=='' | is.na(RANGELOW))  |
                        !(RANGEHIGH=='' | is.na(RANGEHIGH))
                       ) &
                       PARAMETER %in% unique(df$PARAMETER)
                      ,select=c(FORMABBR, PARAMETER, UNITS, SAMPLE_TYPE
                               ,RANGETYPE, RANGELOW, RANGEHIGH
                               )
                      )
  if(nrow(range.meta) == 0) {
      return("No range values occur in the metadata for this table.")
  }

#  names(range.meta) <- toupper(names(range.meta))


  # Comb through data for values out of their expected ranges
  if('UNITS' %in% names(df)) {
      cols <- c('PARAMETER','UNITS')
  } else {
      cols <- 'PARAMETER'
  }

  rr <- validationRange(df, 'RESULT', cols
                       ,range.meta
                       ,otherTests=supplementalTest
                       )
  if(!is.data.frame(rr)) return(rr)

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo)

  return(vv)
}

valRangeCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

# end of file
