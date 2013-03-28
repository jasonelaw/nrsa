# combineValidationResults.r
#
# 10/27/09 cws Created, taken directly from version for NLA in tableValidation.r
#

combineValidationResults <- function(r1, r2, keys)
# combines the results from two validation tests into a single data frame
# consisting of only the relevant keys and the combined English description
# of the tests in testDescription.  The returned data frame consists of
# the columns listed in the keys argument and testDescription.
#
# ARGUMENTS:
# r1        data frame resulting from a validation test
# r2        data frame resulting from a validation test
# keys      vector of column names (strings) or indexes (integers) used as
#             keys to merge the two data frames.
#
# ASSUMPTIONS:
# Both input dataframes are of the standard format for validation output; this
#   means having the testDescription column, among other things.
# Both input dataframes use the same keys.
#
{
  both <- merge(r1[,c(keys, 'testDescription')]
               ,r2[,c(keys, 'testDescription')]
               ,by=keys, all=TRUE
               )

  # Combine the descriptions, but avoid paste() pasting in NA values
  both$testDescription <- ifelse(is.na(both$testDescription.x)
                                ,both$testDescription.y
                                ,ifelse(is.na(both$testDescription.y)
                                       ,both$testDescription.x
                                       ,paste(both$testDescription.x
                                             ,both$testDescription.y
                                             ,sep='; '
                                             )
                                       )
                                )
  both <- both[, c(keys, 'testDescription')]

  return(both)
}

# end of file