# valStructFishCollection.r
#
# 10/19/2009 mrc Created
# 10/21/2009 added timeThis
# 12/8/09    mrc changed sample_type to reflect W, B
# 12/23/09 cws Did not update unit test to reflect current SAMPLE_TYPE values
# 02/25/10 cws creation of unit test data is now done separately for each
#          SAMPLE_TYPE.  Changed structure checks to take SAMPLE_TYPE into
#          account.
#
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the fishcollection table




valStructFishCollection <- function(df, test='all')

# Performs structure checks on the NRSA table tblFishCollection2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA FishCollection data
# test      String describing which tests to perform.  May be one of the
#             following:
#             'all'       default, performs all tests
#             'vital'     performs only vital tests
#             'nonvital'  performs only nonvital tests
#             'synopsis'  performs all tests, returning counts of detected
#                           errors for all tests
#
# ASSUMPTIONS: have a open channel to the database
#

{
  intermediateMessage('Structure validation of fish collection data ', loc='end')
  timeThis <- FALSE
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }



  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1correctColumnNames', loc='end')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT','PAGE','LINE'
                                    ,'PARAMETER', 'RESULT'
                                    ,'SAMPLE_TYPE', 'FLAG'
                                    ) , timing=timeThis
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

#-----------------------------MISSING VALUES-----------------------------------#

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing UID values
      intermediateMessage('.2missingUIDValues', loc='end')
      pp <- stValMissingValues(df, 'UID', timing=timeThis)
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
      intermediateMessage('.3MissingTransectValues', loc='end')
      pp <- stValMissingValues(df, 'TRANSECT', timing=timeThis)
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

  # Check for missing PAGE values
      intermediateMessage('.4MissingPageValues', loc='end')
      pp <- stValMissingValues(df, 'PAGE', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing PAGE values"
                               ,"Missing PAGE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
       # Check for missing LINE values
      intermediateMessage('.5MissingLineValues', loc='end')
      pp <- stValMissingValues(df, 'LINE', timing=timeThis)
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
      intermediateMessage('.6missingParameterValues', loc='end')
      pp <- stValMissingValues(df, 'PARAMETER', timing=timeThis)
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
      intermediateMessage('.7missingSampleType', loc='end')
      pp <- stValMissingValues(df, 'SAMPLE_TYPE', timing=timeThis)
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

#------------------------------------------------------------------------------#
      # Separate structure checks by SAMPLE_TYPE
      fishb <- subset(df, SAMPLE_TYPE=='FISHB')
      fishw <- subset(df, SAMPLE_TYPE=='FISHW')
      lmsamp <- subset(df, SAMPLE_TYPE=='LMSAMP')

#-------------------------UNEXPECTED VALUES------------------------------------#
  
      # Check for unexpected TRANSECT values
      intermediateMessage('.8unexpectedTransect', loc='end')
      if(nrow(fishb)>0) {
          pp <- stValUnexpectedValues (fishb,'TRANSECT'
                                      ,c(LETTERS[1:10], 'NOT MARKED')
                                      ,c('UID'), timing=timeThis
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
      }
      if(nrow(fishw)>0) {
          pp <- stValUnexpectedValues (fishw,'TRANSECT'
                                      ,c('NONE')
                                      ,c('UID'), timing=timeThis
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
      }
      if(nrow(lmsamp)>0) {
          pp <- stValUnexpectedValues (lmsamp,'TRANSECT'
                                      ,c(LETTERS[1:10])
                                      ,c('UID'), timing=timeThis
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
      }

      # Check for unexpected PARAMETER values
      intermediateMessage('.9unexpectedParameter', loc='end')
      if(nrow(fishb)>0) {
          pp <- stValAbsentValues(fishb, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                   ,'BANK_OTHER', 'COUNT', 'COVER'
                                   ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'NO_FISH', 'PHOTO', 'SHOCK_TIME'
                                   ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                   ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=FISHB): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw)>0) {
          pp <- stValAbsentValues(fishw, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                   ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                   ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                   ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                   )
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=FISHW): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp)>0) {
          pp <- stValAbsentValues(lmsamp, 'PARAMETER'
                                 ,c('AC_COUNT', 'AC_SIDE')
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=LMSAMP): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for unexpected values of SAMPLE_TYPE.
         
      intermediateMessage('.10unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('FISHW','FISHB', 'LMSAMP')
                                 ,c('UID', 'TRANSECT'), timing=timeThis
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
#---------------------------UNIQUE VALUES--------------------------------------#
 #requires too much memory to run at this time   
      # Check for uniqueness of each UID-TRANSECT-PAGE-LINE-PARAMETER
 #     intermediateMessage('.11uniqueUID*Transect*Page*Line*Parameter', loc='end')
 #     pp <- stValCountRows(df, c('UID','TRANSECT', 'PAGE','LINE', 'PARAMETER'), 1)
 #     if(test=='synopsis') {
 #         probs <- rbind(probs
 #                       ,sprintf("Duplicate UID-TRANSECT-PAGE_LINE-PARAMETER values: %d (vital)"
 #                               ,ifelse(is.null(pp), 0, nrow(pp))
 #                               )
 #                       )
 #     } else {
 #         probs <- rbind(probs, pp)
 #     }

  }

#----------------------------ABSENT VALUES-------------------------------------#
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.12absentTransectValues', loc = 'end')
      if(nrow(fishb) > 0) {
          pp <- stValAbsentValues(fishb, 'TRANSECT'
                                 ,c(LETTERS[1:10], 'NOT MARKED')
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=FISHB) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw) > 0) {
          pp <- stValAbsentValues(fishw, 'TRANSECT'
                                 ,c('NONE')
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=FISHW) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp) > 0) {
          pp <- stValAbsentValues(lmsamp, 'TRANSECT'
                                 ,c(LETTERS[1:10])
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=LMSAMP) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for absent PARAMETER values (parameters NO_FISH and PHOTO may
      # be absent at a transect.
      intermediateMessage('.13absentParameter', loc='end')
      if(nrow(fishb) > 0) {
          pp <- stValAbsentValues(fishb, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                   ,'BANK_OTHER', 'COUNT', 'COVER'
                                   ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'SHOCK_TIME'
                                   ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                   ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=FISHB): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw) > 0) {
          pp <- stValAbsentValues(fishw, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                   ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                   ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                   ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=FISHW): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp) > 0) {
          pp <- stValAbsentValues(lmsamp, 'PARAMETER'
                                 ,c('AC_COUNT', 'AC_SIDE')
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=LMSAMP): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
  }


  intermediateMessage('.  Done', loc='end')
  return(probs)

}



