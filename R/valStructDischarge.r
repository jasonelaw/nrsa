# valStructDischarge.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblDISCHARGE2
# 10/21/09 cws Added unit test.  Modified structure check to include tests for
#          nonunique UID-REP-LINE-PARAMETER-SAMPLE_TYPE combinations and a
#          check for unexpected SAMPLE_TYPE
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
#

# Contains functions valStructDischarge 

require(RODBC)
#intermediateMessages <- TRUE

valStructDischarge <- function(df, test='all')
# Performs structure checks on the NRSA table tblDISCHARGE2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA discharge data
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
  intermediateMessage('Structure validation of stream discharge data ', loc='start')
  
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'REP', 'LINE', 'METHOD'
                                    ,'PARAMETER', 'RESULT', 'UNITS', 'FLAG'
                                    ,'SAMPLE_TYPE'
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

      
      # Check for missing REP values
      intermediateMessage('.3')
      pp <- stValMissingValues(df, 'REP')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing REP values"
                               ,"Missing REP values exist (vital)"
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


      # Check for missing METHOD values
      intermediateMessage('.6')
      pp <- stValMissingValues(df, 'METHOD')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing METHOD values"
                               ,"Missing METHOD values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected REP values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df, 'REP'
                                 ,c('1', '2', '3', '4', '5', '99')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected REP values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected METHOD values
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df, 'METHOD'
                                 ,c('NBO','QVAL','TIMED FILLING','VELOCITY AREA')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected METHOD values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-REP-LINE-PARAMETER
      intermediateMessage('.9')
      pp <- stValCountRows(df, c('UID','REP','LINE','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-REP-LINE-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent LINE values at each UID-REP : Velocity area
      intermediateMessage('.10')
      pp <- stValAbsentValues(subset(df, METHOD == 'VELOCITY AREA'), 'LINE'
                             ,c('1')
                             ,c('UID', 'REP')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent LINE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent LINE values at each UID-REP : Timed fill and NBO
      intermediateMessage('.11')
      pp <- stValAbsentValues(subset(df, METHOD != 'VELOCITY AREA'), 'LINE'
                             ,c('999')
                             ,c('UID', 'REP')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent LINE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      

      # Check for absent REP values at each UID-LINE: Velocity area
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, METHOD == 'VELOCITY AREA'), 'REP'
                             ,c('99')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: Timed fill
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, METHOD == 'TIMED FILLING'), 'REP'
                             ,c('1', '2', '3', '4', '5')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: NBO
      intermediateMessage('.14')
      pp <- stValAbsentValues(subset(df, METHOD == 'NBO'), 'REP'
                             ,c('1', '2', '3')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: QVal
      intermediateMessage('.15')
      pp <- stValAbsentValues(subset(df, METHOD == 'QVAL'), 'REP'
                             ,c('99')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected PARAMETER values
      intermediateMessage('.16')
      pp <- stValUnexpectedValues(subset(df, METHOD=='NBO')
                                 ,'PARAMETER'
                                 ,c('AVGWIDTH', 'DEPTH_1','DEPTH_2', 'DEPTH_3'
                                    ,'DEPTH_4', 'DEPTH_5','FLOAT', 'TOTTIME'
                                   )
                                 ,c('UID', 'REP')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for NBO method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.17')
      pp <- stValUnexpectedValues(subset(df, METHOD=='QVAL')
                                 ,'PARAMETER'
                                 ,'QVAL'
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for QVAL method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.18')
      pp <- stValUnexpectedValues(subset(df, METHOD=='TIMED FILLING')
                                 ,'PARAMETER'
                                 ,c('TIME', 'VOLUME')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for TIMED FILLING method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.19')
      pp <- stValUnexpectedValues(subset(df, METHOD=='VELOCITY AREA')
                                 ,'PARAMETER'
                                 ,c('DEPTH', 'DISTBANK', 'VELOCITY')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for VELOCITY AREA method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values: Velocity area
      intermediateMessage('.20')
      pp <- stValAbsentValues(subset(df, METHOD == "VELOCITY AREA")
                                 ,'PARAMETER'
                                 ,c('DEPTH')#, 'DISTBANK', 'VELOCITY')
                                 ,c('UID', 'REP', 'LINE')
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

      # Check for absent PARAMETER values: Timed filling
      intermediateMessage('.21')
      pp <- stValAbsentValues(subset(df, METHOD == "TIMED FILLING")
                                 ,'PARAMETER'
                                 ,c('VOLUME', 'TIME')
                                 ,c('UID', 'REP', 'LINE')
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

      # Check for absent PARAMETER values: NBO
      intermediateMessage('.22')
      pp <- stValAbsentValues(subset(df, METHOD == "NBO")
                                 ,'PARAMETER'
                                 ,c('AVGWIDTH', 'DEPTH_1',
                                    'DEPTH_2', 'DEPTH_3', 'DEPTH_4', 'DEPTH_5',
                                    'FLOAT', 'TOTTIME')
                                 ,c('UID', 'REP', 'LINE')
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

      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.23')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('FLOW')
                                 ,c('UID', 'REP', 'LINE')
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

