# valStructThalweg.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblTHALWEG2
# 10/22/09 cws Added unit test.  Modified structure check to include uniqueness
#          check, only allow main channel transects, separate absent/unexpected
#          parameter checks by SAMPLE_TYPE.  Commented out absent parameter
#          check for thalweg data since there is no way to discern wadeable
#          and boatable reaches using SAMPLE_TYPE.  NOTE: Will need to add lines
#          of expected problems to unit test if/when this changes.
# 11/6/09 mrc - fixed parameter SIZE_CLS to reflect change in parameter
# 12/02/09 cws Removed code checking for SAMPLE_TYPE 'PHAB_CHANF' and parameters
#          'ACTRANSP' and 'INTDTRAN', which are in tblCHANNELGEOMETRY2.  Updated
#          unit test accordingly.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#  4/19/10 ssr added 'NOT MARKED' as possible transect
# 05/28/10 ssr added check for non-numeric values

# Contains functions valStructBankGeometry, fixBankGeometry 

require(RODBC)
#intermediateMessages <- TRUE

valStructThalweg <- function(df, test='all')
# Performs structure checks on the NRSA table tblTHALWEG2.  Returns NULL if
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
  intermediateMessage('Structure validation of thalweg data ', loc='start')
  
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'STATION'
                                    ,'PARAMETER', 'RESULT', 'FLAG'
                                    ,'SAMPLE_TYPE', 'UNITS'
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


      # Check for missing STATION values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'STATION')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing STATION values"
                               ,"Missing STATION values exist (vital)"
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


      # Check for unexpected STATION values in Streams
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 , 'STATION'
                                 ,c('0', '1', '2', '3', '4', '5', '6', '7', '8',
                                 '9', '10', '11', '12', '13', '14')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected STATION values in Rivers
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                                 ,'STATION'
                                 ,c('NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values in Streams
      intermediateMessage('.9')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','NOT MARKED')
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


      # Check for unexpected TRANSECT values in Rivers
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J')
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


      # Check for uniqueness of each UID-TRANSECT-STATION-PARAMETER
      intermediateMessage('.11')
      pp <- stValCountRows(df, c('UID','TRANSECT','STATION','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      #  Check that numeric fields contain numeric data
      intermediateMessage('.11a')
      pp <- stValNonNumericValues(subset(df, PARAMETER %in% c('DEPTH','INCREMNT'
                                         , 'WETWIDTH', 'BARWIDTH','DEP_SONR'
                                         , 'DEP_POLE'))
                                  ,'RESULT'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Non-numeric values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      



  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent STATION values at each UID-TRANSECT in Streams
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                             ,'STATION'
                             ,c(as.character(0:9))
                             ,c('UID', 'TRANSECT')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent STATION values with SAMPLE_TYPE=='PHAB_THAL': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent STATION values at each UID-TRANSECT in boatable
      # channel/riparian data.
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                             ,'STATION'
                             ,c('NONE')
                             ,c('UID', 'TRANSECT')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent STATION with values SAMPLE_TYPE='PHAB_CHANF': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }



      # Check for absent TRANSECT values at each UID
      intermediateMessage('.14')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J')
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

      # Check for unexpected PARAMETER values
      intermediateMessage('.15')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 ,'PARAMETER'
                                 ,c('BACKWATER', 'BAR_PRES', 'CHANUNCD', 'DEPTH'
                                   ,'INCREMNT', 'POOLFMCD', 'SEDIMENT', 'SIDCHN'
                                   ,'WETWIDTH', 'BARWIDTH', 'DEP_SONR', 'OFF_CHAN'
                                   ,'SIZE_CLS', 'SNAG', 'DEP_POLE'
                                   )
                                 ,c('UID', 'TRANSECT', 'STATION')
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

#      intermediateMessage('.16')
#      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
#                                 ,'PARAMETER'
#                                 ,c('ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Unexpected PARAMETER values: %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#      } else {
#          probs <- rbind(probs, pp)
#      }

      # Check for absent PARAMETER values
#      intermediateMessage('.17')
#      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_THAL')
#                                 ,'PARAMETER'
#                                 ,c('BACKWATER', 'BAR_PRES', 'CHANUNCD', 'DEPTH',
#                                    'INCREMNT', 'POOLFMCD', 'SEDIMENT', 'SIDCHN',
#                                    'WETWIDTH', 'BARWIDTH', 'DEP_SONR', 'OFF_CHAN',
#                                    'SIZ_CLS', 'SNAG', 'DEP_POLE', 'ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT', 'STATION')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Absent PARAMETER values with SAMPLE_TYPE='PHAB_THAL': %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#     } else {
#          probs <- rbind(probs, pp)
#      }

      # Check for absent PARAMETER values
#      intermediateMessage('.18')
#      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANF')
#                                 ,'PARAMETER'
#                                 ,c('ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Absent PARAMETER values with SAMPLE_TYPE='PHAB_CHANF': %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#     } else {
#          probs <- rbind(probs, pp)
#      }


      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.19')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
#                                 ,c('PHAB_THAL','PHAB_CHANF', 'PHAB_CHANBFRONT')
                                 ,c('PHAB_THAL','PHAB_THALW')
                                 ,c('UID', 'TRANSECT', 'STATION')
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


  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}

