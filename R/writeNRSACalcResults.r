# writeNRSACalcResults.r
#
# 12/28/09 mrc Created
#  1/04/10 cws Specified file location with NRSACalcLocation
#

writeNRSACalcResults <- function (df, outLoc)
# Writes NRSA metric results to a csv file .
# Returns NULL on success, or character string describing error.
#
# POSSIBLE FUTURE EXTENSIONS:
# + This may later be changed so that an xls, xlsx and/or xml file is created.
#
# ARGUMENTS:
# df            data frame of validation results.
# outLoc        string with full path and filename to which validation results
#                 will be written, using the foreslash (/) as the directory
#                 level delimieter.
#
# ASSUMPTIONS:
#
{
  if(substr(outLoc, nchar(outLoc)-3, nchar(outLoc)) == '.csv') {
      write.csv(df, paste(.nrsa.Options$NRSACalcLocation, outLoc, sep=''), na='', row.names=FALSE)
      rc <- NULL
  } else  {
      rc <- sprintf("Error: writeNRSACalcResults() currently only writes csv files.\n")
  }
  
  return(rc)
}

# end of file
