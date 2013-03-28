# valLegalCheck.r
#
# 10/30/09 cws Created
# 11/16/09 cws Added use of SAMPLE_TYPE in metadata and standardized metadata
#          to upper case names.  Separated file/table io from the work that's
#          easy to test.
# 12/23/09 cws Added SAMPLE_TYPE to expected results in unit test.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valLegalCheck <- function(tableName,since=NULL)
# Performs legal value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation if any
# issues are discovered.
#
# Returns NULL on success, or character string describing error or if there
# are no illegal values.
#
# ARGUMENTS:
# tableName Character string with name of table to do legal validation on.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valLegalCheck.cleanup(chan))

   if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }


 df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2', filterSideChannels=FALSE)
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create validation results
  validationResults <- valLegalCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)
  
  print(sprintf("Detected %d illegal values", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLegal'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
  return(NULL)
}

valLegalCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing legal values and form imformation
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
#
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  legal.meta <- subset(meta.df
                      ,!(LEGALVALUES=='' | is.na(LEGALVALUES))
                      ,select=c(PARAMETER, LEGALVALUES, SAMPLE_TYPE, FORMABBR)
                      )
  if(nrow(legal.meta) == 0) {
      return("Warning: No legal values occur in the metadata.")
  }

  # Comb through data for values out of their expected ranges
  rr <- validationLegal(df, 'RESULT', 'PARAMETER'
                       ,legal.meta
                       ,otherTests=NULL
                       )
  if(nrow(rr) == 0) {
      return("No illegal values were detected.")
  }

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo)

  return(vv)
}

valLegalCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

