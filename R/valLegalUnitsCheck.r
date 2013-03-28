# valLegalUnitsCheck.r
#
# 11/30/09 cws Created to check for illegal units
# 12/04/09 cws Corrected name of output file and notice of completion to tell
#              user that the check is for legal units, not values.
# 12/07/09 cws Check for illegal values now takes SAMPLE_TYPE into account when
#          determining expectedUnits.
# 12/08/09 SSR Created TESTDESCRIPTION in valLegalUnitsCheck.1 and fill with
#          units expected for given parameter  
# 12/23/09 cws valLegalUnitsCheckTest victim of cut&paste error, apparently
#          overwritten by unit test for valLegalCheck.  Recreated unit test.
#          Changed check to flag missing UNITS when nonMissing UNITS is
#          expected.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valLegalUnitsCheck <- function(tableName,since=NULL)
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
  on.exit(valLegalUnitsCheck.cleanup(chan))

    if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }

  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2',filterSideChannels=FALSE)
  if(!is.data.frame(df)) return(df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2',filterSideChannels=FALSE)
  if(!is.data.frame(df)) return(df)

  # Create validation results
  validationResults <- valLegalUnitsCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)
  
  print(sprintf("Detected %d illegal units", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLegalUnits'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
  return(NULL)
}

valLegalUnitsCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing expected units for parameters that
#            use them.
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
# ASSUMPTIONS:
#  meta.df has columns PARAMETER, UNITS, SAMPLE_TYPE & FORMABBR.
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  units.meta <- subset(meta.df
                      ,!(UNITS=='' | is.na(UNITS))
                      ,select=c(PARAMETER, UNITS, SAMPLE_TYPE, FORMABBR)
                      )
  if(nrow(units.meta) == 0) {
      return("Warning: No units information occur in the metadata.")
  }

  # Comb through data for unexpected parameter-unit combinations
  expectedUnits <- unique(paste(units.meta$PARAMETER
                               ,units.meta$UNITS
                               ,units.meta$SAMPLE_TYPE
                               )
                         )
                         
  rr <- subset(df
              , #!(UNITS=='' | is.na(UNITS)) &
                !(paste(PARAMETER, UNITS, SAMPLE_TYPE) %in% expectedUnits)
              )

  if(nrow(rr) == 0) {
      return("No illegal units were detected.")
  }

  # Create list of expected parameter/unit combinations
  ss<-units.meta[!duplicated(units.meta[c('PARAMETER','UNITS','SAMPLE_TYPE')]),]
  tt<-aggregate(ss['UNITS']
             ,list(PARAMETER  =ss$PARAMETER
                  ,SAMPLE_TYPE=ss$SAMPLE_TYPE
                  )
             ,function(x) { paste(x, collapse=', ') }
             )

  #Rename UNITS to ALL_UNITS for the merge
  colnames(tt) <- c('PARAMETER', 'SAMPLE_TYPE', 'ALL_UNITS')
  xx <- merge(rr,tt, all.x=T, all.y=F)
  
  # add this column, and fill with expected units
  xx$TESTDESCRIPTION <- paste('UNITS value must be ', xx$ALL_UNITS)
  # its work now done, ALL_UNITS can be deleted
  xx$ALL_UNITS <- NULL

  # Construct validation results
  vv <- constructNRSAValidationResults(xx, meta.df, siteInfo)

  return(vv)
}

valLegalUnitsCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

