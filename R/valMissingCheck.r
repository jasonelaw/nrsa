# valMissingCheck.r
#
# 10/30/09 cws Created
# 11/30/09 cws Filtering out missing values associated with comment flags.
#          Updated unit test.
# 12/02/09 cws Filtering based on FLAG only if that column exists.
# 12/23/09 cws Updating unit test to correct FORMIMAGE hyperlink arguments.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valMissingCheck <- function(tableName,since=NULL)
# Performs missing value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation
#
# Returns NULL on success, or character string describing error.
#
# ARGUMENTS:
# tableName Character string with name of table to do missing data validation on.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valMissingCheck.cleanup(chan))

  if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }

  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2', filterSideChannels=FALSE)
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)
  
  # Create missing check results
  missingResults <- valMissingCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(missingResults)) return(missingResults)

  print(sprintf("Detected %d rows with missing values", nrow(missingResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(missingResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valMissing'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )

  return(NULL)
}

valMissingCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}


valMissingCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing legal values and form imformation
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
{

  # Filter out missing values that have a flag associated with them.  We are
  # assuming that a missing value that has a comment flag attached to it is
  # missing for a reason.
  if('FLAG' %in% names(df)) {
      df <- subset(df, is.na(FLAG) | trimws(FLAG)=='')
  }
  
  # Determine whether UNITS is present and should be included in checks.
  # Use metadata to determine whether UNITS, if it exists, should have a value
  # and exclude rows with missing UNITS that should have missing UNITS unless
  # RESULTS is missing, as well as rows in which missing is noted as a legal
  # value.
  if('UNITS' %in% names(df)) {
      cols <- c('RESULT','UNITS')
      unitsRequired <- unique(subset(meta.df, UNITS!='')$PARAMETER)
      valueOptional <- subset(meta.df, substr(LEGALVALUES,1,1) == '|')$PARAMETER
      checkThese <- subset(df
                          ,(RESULT=='' & !(PARAMETER %in% valueOptional)) |
                           (UNITS=='' & PARAMETER %in% unitsRequired)
                          )
  } else {
      cols <- 'RESULT'
      checkThese <- df
  }

  # Comb through data for missing values and units
  rr <- validationMissing(checkThese, cols, otherTests=NULL)
  if(!is.data.frame(rr)) return(rr)
  if(nrow(rr) == 0) return("No missing values were detected.")

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo)

  return(vv)
}

