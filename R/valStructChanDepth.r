# valStructChanDepth.r
#
# 10/19/09 cws Created
#  3/25/10 cws removed source() for nlaSupport.r (now sharedSupport.r) and
#          validation.r.
#
#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/validation.r')
#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/nlaSupport.r')
require(RODBC)
#intermediateMessages <- TRUE

valStructChanDepth <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANDEPTH2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA canopy cover data
# test      String describing which tests to perform.  May be one of the
#             following:
#             'all'       default, performs all tests
#             'vital'     performs only vital tests
#             'nonvital'  performs only nonvital tests
#             'synopsis'  performs all tests, returning counts of detected
#                           errors for all tests
#
# ASSUMPTIONS:
#
{
  intermediateMessage('Structure validation of channel depth data ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'LINE', 'SAMPLE_TYPE'
                                    ,'PARAMETER', 'RESULT', 'UNITS', 'FLAG'
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing UID values
      intermediateMessage('.2')
      pp <- stValMissingValues(df, 'UID')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing UID values"
                               ,"Missing UID values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing TRANSECT values
      intermediateMessage('.3')
      pp <- stValMissingValues(df, 'TRANSECT')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TRANSECT values"
                               ,"Missing TRANSECT values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing LINE values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'LINE')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing LINE values"
                               ,"Missing LINE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing PARAMETER values
      intermediateMessage('.5')
      pp <- stValMissingValues(df, 'PARAMETER')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing PARAMETER values"
                               ,"Missing PARAMETER values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing SAMPLE_TYPE values
      intermediateMessage('.6')
      pp <- stValMissingValues(df, 'SAMPLE_TYPE')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing SAMPLE_TYPE values"
                               ,"Missing SAMPLE_TYPE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected LINE values
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df
                                 ,'LINE'
                                 ,1:5
                                 ,c('UID', 'TRANSECT','SAMPLE_TYPE','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected LINE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-SAMPLE_TYPE-LINE-PARAMETER
      # where RESULT is not NA, so there is only one PARAMETER per line,
      # regardless of the value of PARAMETER.  Data is expected to look like this:
      #
      #       UID TRANSECT LINE     SAMPLE_TYPE PARAMETER RESULT UNITS FLAG
      #   1  9928        A    1 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   2  9928        A    1 PHAB_CHANBFRONT     SONAR    3.3     m <NA>
      #   3  9928        A    2 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   4  9928        A    2 PHAB_CHANBFRONT     SONAR    2.1     m <NA>
      #   5  9928        A    3 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   6  9928        A    3 PHAB_CHANBFRONT     SONAR    1.3     m <NA>
      #   7  9928        A    4 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   8  9928        A    4 PHAB_CHANBFRONT     SONAR    3.5     m <NA>

      intermediateMessage('.9')
      pp <- stValCountRows(subset(df, !is.na(RESULT))
                          ,c('UID','TRANSECT','SAMPLE_TYPE','LINE')
                          ,1
                          )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }

  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected SAMPLE_TYPE values
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANBFRONT')
                                 ,c('UID', 'TRANSECT','LINE','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected SAMPLE_TYPE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected PARAMETER values
      intermediateMessage('.11')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('POLE','SONAR')
                                 ,c('UID', 'TRANSECT', 'SAMPLE_TYPE','LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent TRANSECT values at each UID
      intermediateMessage('.12')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSECT values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values
      intermediateMessage('.13')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                 ,c('POLE','SONAR')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}


