# readNRSAValidationResults.r
#
# 12/15/09 cws Recreated after being overwritten. D'oh!
#


readNRSAValidationResults <- function(fileName)
# Reads data validation results from a file in the directory specified by
# NRSAMetricsLocation which contains previously calculated metrics.
#
# Returns the contents of the file as a dataframe if successful, or an
# error message if unsuccessful.
#
# ARGUMENTS:
# filePath   string with full path and filename containing validation results.
#
{

  if(substr(fileName, nchar(fileName)-3, nchar(fileName)) == '.csv') {
      rr <- read.csv(fileName
                    ,na=''
                    ,header=TRUE
                    ,stringsAsFactors=FALSE
                    )
#      rr <- rr[!is.na(rr[1]),]
  } else  {
      cat(sprintf("Error: readNRSAValidationResults() currently only reads csv files.\n"))
      rr <- "Error: readNRSAValidationResults() currently only reads csv files.\n"
  }

  return(rr)
}
