# valInsertTable.r
#
# 10/30/09 cws created
# 12/03/09 cws Corrected call to cleanup function
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#  3/30/10 cws corrected comment.
# 05/03/10  SSR modified valUpdateTable to valInsertTable by changing call to
#           dbUpdate to dbInsert
# 05/19/10 SSR modified to remove UID from incoming csv file
#
require(RODBC)

valInsertTable <- function(inLoc, tblName)
# Updates specified table with changes in validation file.
# ARGUMENTS:
# inLoc    character string specifying full path of the validation results to
#            be read in.
# tblName  character string specifying the name of the data table to update
#            with the validation results
#
# ASSUMPTIONS:
# The validation results are based on the table being updated.
#
{
  # Retrieve file with user-validated data and format DATE_COL values as
  # expected.
  valResults <- readNRSAValidationResults(inLoc)
  if(!is.data.frame(valResults)) {
      return(valResults)
  }
  # Removing UID
  valResults$UID <- NULL
    
  valResults$DATE_COL <- as.Date(valResults$DATE_COL, "%m/%d/%Y")
  
  # Retrieve data required for constructing an update to the table
  chan <- odbcConnect('NRSA2')
  on.exit(valUpdateTable.cleanup(chan))

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')
  if(!is.data.frame(siteInfo)) {
      return(paste("Error: Could not retrieve tblVISITS2 "
                  ,". "
                  ,siteInfo
                  )
            )
  }
  
  currentData <- fetchNRSATable(chan, tblName)
  if(!is.data.frame(currentData)) {
      return(paste("Error: Could not retrieve current contents of "
                  ,tblName
                  ,". "
                  ,currentData
                  )
            )
  }
  
  # Construct update to the table
  updateData<- constructNRSAValidationUpdate(valResults, siteInfo, currentData)
  if(!is.data.frame(updateData)) {
      return(paste("Error: Could not construct update with validation data for "
                  ,tblName
                  ,". "
                  ,updateData
                  )
            )
  }
  
  # Determine keys used in table.  Retain only columns in validation results
  # which are keys or columns that may have been edited.
  keysUsed <- names(updateData)[names(updateData) %in% c(nrsa.options()$NRSAKeyColumns,'PARAMETER')]

  # Update table in database
  rc <- dbInsert(chan, tblName, updateData, keysUsed)
  
  return(rc)
}

valUpdateTable.cleanup <- function(chan)
# cleans up on exit of valUpdateTable()
{
  odbcClose(chan)
}

# end of file