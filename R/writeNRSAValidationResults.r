# writeNRSAValidationResults.r
#
# 10/28/09 cws Created
#

writeNRSAValidationResults <- function (df, outLoc)
# Writes NRSA QA results to a csv file for review and editing values.
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
      write.csv(df, outLoc, na='', row.names=FALSE)
      rc <- NULL
  } else  {
      rc <- sprintf("Error: writeNRSAValidationResults() currently only writes csv files.\n")
  }
  
  return(rc)
}

# end of file
