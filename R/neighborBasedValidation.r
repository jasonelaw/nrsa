# neightborBasedValidation.r
#
# 12/18/09 cws Created
#

neightborBasedValidation <- function(df, keys, name, value, parameter, tbf, min, max)
# Performs a context-based range check of a value based on neighboring values,
# to find values that exceed their neighboring values by more than a specified
# factor, or which exceed specified constants if the neighboring values are
# missing.
#
# Returns a subset of the input dataframe containing the flagged values with
# an additional column TESTDESCRIPTION, and optionally including rows with their
# neighboring values as well.
#
# ARGUMENTS:
# df          dataframe to undergo this validation test
# keys        keys used to uniquely identify each row in the dataframe
# name        string with name of column holding names of parameters in dataframe
# value       string with name of column holding value of parameters in dataframe
# parameter   name of parameter to check
# tbf         factor by which adjacent values may be expected to vary; values
#               which change greater than this factor are flagged for validation.
# min, max    numeric values specifying the expected range of the parameter; this
#               static range test is only performed when a value has no nonmissing
#               neighbors.
#
# ASSUMPTIONS:
# Creation of temporary columns ..value, ..first, ..last, ..next, ..prev
#   and ..flag is OK.
#
{
  ds <- df[df[name]==parameter,]  # subset to specific parameter

  # Converting values to a numeric mode is difficult to do in a single step
  # without generating warning messages, so three steps are used.
  ds$..value <- unlist(ds[value])
  ds$..value <- ifelse(is.na(ds$..value), NA, ds$..value)
  ds$..value <- ifelse(trimws(ds$..value) == '' | ds$..value == '.', NA, ds$..value)
  ds$..value <- as.numeric(ds$..value)

  # Adjacent values are obtained with lag() and lead().  The beginning and end
  # of a series has no previous or next neighbors (respectively), so those are
  # made missing; lag()ing and lead()ing thus requires first() and last(), and
  # thus appropriate ordering.
  # if keys==c('UID','TRANSECT','STATION') then this statement will parse to
  # order(ds$UID, ds$TRANSECT, ds$STATION)
  ordering <- eval(parse(text=sprintf('order(%s)'
                                     ,paste('ds$', keys, sep='', collapse=', ')
                                     )
                        )
                  )
  ds <- ds[ordering,]

  ds <- first(ds, keys[1], '..first')
  ds <- last(ds, keys[1], '..last')
  ds <- lag(ds, '..value', '..prev')
  ds <- lead(ds, '..value', '..next')
  ds$..prev <- ifelse(ds$..first, NA, ds$..prev)
  ds$..next <- ifelse(ds$..last, NA, ds$..next)


  # Compare values with their available neighbors.
  ds$TESTDESCRIPTION <- as.character(NA)
  ds$..flag <- ifelse(!(is.na(ds$..prev) | is.na(ds$..next))
                     ,(ds$..value != 0 & ds$..prev != 0 & ds$..next != 0) &
                      (ds$..value > ds$..prev*tbf | ds$..value < ds$..prev/tbf |
                       ds$..value > ds$..next*tbf | ds$..value < ds$..next/tbf
                      )
                     ,ifelse(!is.na(ds$..prev)
                            ,(ds$..value != 0 & ds$..prev != 0) &
                             (ds$..value > ds$..prev*tbf |
                              ds$..value < ds$..prev/tbf
                             )
                            ,ifelse(!is.na(ds$..next)
                                   ,(ds$..value != 0 & ds$..next != 0) &
                                    (ds$..value > ds$..next*tbf |
                                     ds$..value < ds$..next/tbf
                                    )
                                   ,NA
                                   )
                            )
                     )
  ds$TESTDESCRIPTION <- ifelse(ds$..flag,'Value varies considerably from its neighbors', NA)

  # perform static range checks, used to fill in gaps due to missing values
  if(!is.null(max)) {
      if(!is.null(min)) {
          ff <- ds$..value > max | ds$..value < min
      } else {
          ff <- ds$..value > max
      }
  } else {
      if(!is.null(min)) {
          ff <- ds$..value < min
      } else {
          ff <- FALSE
      }
  }

  # fill in missing checks with static range values, if provided
  ds$TESTDESCRIPTION <- ifelse(is.na(ds$..flag) & ff
                              ,sprintf('Value exceeds specified range (%s,%s)'
                                      ,min, max
                                      )
                              ,ds$TESTDESCRIPTION
                              )

  ds <- subset(ds, !is.na(TESTDESCRIPTION)
              ,select=-c(..value,..flag,..first,..last,..prev,..next)
              )

  return(ds)
}


