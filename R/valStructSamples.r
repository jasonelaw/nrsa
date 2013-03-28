# valStructSamples.r
#
# 11/02/2009 ssr created
# 12/07/2009 mrc update to reflect change in sample_type
# 02/18/2010 cws Changed test dataframe in unit test to use SAMPLE_TYPE 'KICKB'
#            instead of 'KICK' and converted SUBREACH from factor to character;
#            also changed the expected structural problems for the 'real' data
#            to use ENTEW, BELGW and PAPAB instead of ENTE, BELG and PAPA.
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
#  04/20/2010 SSR added B and BD to SAMPLE_CAT

require(RODBC)
#intermediateMessages <- TRUE

valStructSamples <- function(df, test='all')
# Performs structure checks on the NRSA table tblSAMPLES2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA sample data
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
  intermediateMessage('Structure validation of samples data ', loc='start')
  
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'PAGE', 'LINE'
                                    ,'SUBREACH', 'SAMPLE_TYPE', 'SAMPLE_CAT'
                                    ,'PARAMETER','RESULT','FLAG'
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

      
      # Check for missing SAMPLE_TYPE values
      intermediateMessage('.3')
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


      # Check for missing SAMPLE_CAT values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'SAMPLE_CAT')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing SAMPLE_CAT values"
                               ,"Missing SAMPLE_CAT values exist (vital)"
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


#      # Check for missing SAMPLE_ID values
#      intermediateMessage('.6')
#      pp <- stValMissingValues(df, subset(df, PARAMETER=='SAMPLE_ID'))
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,ifelse(is.null(pp)
#                               ,"No Missing SAMPLE_ID values"
#                               ,"Missing SAMPLE_ID values exist (vital)"
#                               )
#                        )
#      } else {
#          probs <- rbind(probs, pp)
#      }


      # Check for unexpected SAMPLE_TYPE values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df, 'SAMPLE_TYPE'
                                 ,c('BELGB', 'BELGW','BERWW','CHEMB','CHEMW','ENTEB','ENTEW','FISHTV','KICKB'
                                 ,'PAPAW', 'PAPAB','PBIOW','PBIOB','PCHLW','PCHLB','PERIW','PERIB','PHYTW','PHYTB'
                                 ,'PPCPW','PPCPB','SEDEW','SEDEB','SNAGB','WCHLW','WCHLB','WPFCB')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected SAMPLE_TYPE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected SAMPLE_CAT values in Rivers
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_CAT'
                                 ,c('P','D','F','NONE','B','BD')
                                 ,c('UID')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected SAMPLE_CAT values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }



      # Check for uniqueness of each UID-SAMPLE_TYPE-SAMPLE_CAT-PARAMETER
#      intermediateMessage('.9a')
#      pp <- stValCountRows(subset(df,!(SAMPLE_CAT=='NONE')),
#                            c('UID','SAMPLE_TYPE','PAGE','LINE','SAMPLE_CAT','PARAMETER'), 1)
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Duplicate UID-SAMPLE_TYPE-SAMPLE_CAT-PARAMETER values: %d (vital)"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#      } else {
#          probs <- rbind(probs, pp)
#      }


      # Check for uniqueness of each UID-SAMPLE_TYPE-SAMPLE_CAT-PARAMETER
      intermediateMessage('.9b')
      pp <- stValCountRows(subset(df,SAMPLE_CAT=='NONE'),
                            c('UID','SAMPLE_TYPE','PAGE','LINE','SUBREACH','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-SAMPLE_TYPE-SUBREACH-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent SAMPLE_CAT values at each UID
      intermediateMessage('.10')
      pp <- stValAbsentValues(df, 'SAMPLE_CAT'
                             ,c('P')
                             ,'UID'
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent SAMPLE_CAT value (P): %d"
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
                                 ,c('JAR_NO','PRESERVED','SAMPLE_ID','CHILLED',
                                 'DEPTH_COLLECTED','FILTER_1','FILTER_2',
                                 'FILTER_3','FILTER_4','FILTER_END_TIME',
                                 'FILTER_START_TIME','SAMPLE_VOL',
                                 'TIME_COLLECTED','TIME_FROZEN','ACTUAL_DATE',
                                 'COMMON_NAME','FROZEN','TTL_LENGTH','TYPE',
                                 'COMP_VOL','TRAN_NO','VOLUME','LOCATIONS',
                                 'VOLUME_FILTERED')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
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

      # Check for absent PARAMETER values (BELG AND BERG): 
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='BELGW' |SAMPLE_TYPE=='BELGB'| SAMPLE_TYPE=='BERWW')
                                 ,'PARAMETER'
                                 ,c('JAR_NO','PRESERVED','SAMPLE_ID')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (BELGW, BERLGB AND BERWW): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (CHEM): 
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='CHEMB'| SAMPLE_TYPE=='CHEMW')
                                 ,'PARAMETER'
                                 ,c('CHILLED','SAMPLE_ID')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (CHEMB, CHEMW): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (ENTEB,W): 
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='ENTEW' | SAMPLE_TYPE=='ENTEB')
                                 ,'PARAMETER'
                                 ,c('DEPTH_COLLECTED','FILTER_1','FILTER_2',
                                 'FILTER_3','FILTER_4','FILTER_END_TIME',
                                 'FILTER_START_TIME','SAMPLE_ID','SAMPLE_VOL',
                                 'TIME_COLLECTED','TIME_FROZEN')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (ENTEB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (FISHTV): 
      intermediateMessage('.14')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='FISHTV')
                                 ,'PARAMETER'
                                 ,c('ACTUAL_DATE','PRESERVED','SAMPLE_ID',
                                 'COMMON_NAME','FROZEN','TTL_LENGTH')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (FISHTV): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (KICKB): 
      intermediateMessage('.15')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='KICKB')
                                 ,'PARAMETER'
                                 ,c('JAR_NO','SAMPLE_ID','TYPE')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (KICKB): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (PAPA & PBIO): 
      intermediateMessage('.16')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PAPAW' |SAMPLE_TYPE=='PAPAB' |SAMPLE_TYPE=='PBIOW' |SAMPLE_TYPE=='PBIOB')
                                 ,'PARAMETER'
                                 ,c('COMP_VOL','FROZEN','SAMPLE_ID',
                                 'TRAN_NO','VOLUME')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (PAPAB, W & PBIOB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (PERIB,W): 
      intermediateMessage('.17')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PERIW' | SAMPLE_TYPE=='PERIB')
                                 ,'PARAMETER'
                                 ,c('COMP_VOL','PRESERVED','SAMPLE_ID',
                                 'TRAN_NO','VOLUME')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (PERIB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (PHYTB): 
      intermediateMessage('.18')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHYTB')
                                 ,'PARAMETER'
                                 ,c('COMP_VOL','LOCATIONS','SAMPLE_ID')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (PHYTB): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (PPCPB,W): 
      intermediateMessage('.19')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PPCPB'|SAMPLE_TYPE=='PPCBW')
                                 ,'PARAMETER'
                                 ,c('CHILLED','SAMPLE_ID')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (PPCPB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (SEDEB,W): 
      intermediateMessage('.20')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='SEDEB' |SAMPLE_TYPE=='SEDEW')
                                 ,'PARAMETER'
                                 ,c('COMP_VOL','PRESERVED','SAMPLE_ID',
                                 'JAR_NO', 'TRAN_NO','CHILLED')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (SEDEB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values (SNAGB): 
      intermediateMessage('.21')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='SNAGB')
                                 ,'PARAMETER'
                                 ,c('JAR_NO','SAMPLE_ID','TYPE','TRAN_NO')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (SNAGB): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent PARAMETER values (WCHLB,W): 
      intermediateMessage('.22')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='WCHLB' | SAMPLE_TYPE=='WCHLW')
                                 ,'PARAMETER'
                                 ,c('FROZEN','SAMPLE_ID','VOLUME_FILTERED',
                                 'CHILLED','COMP_VOL')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (WCHLB,W): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      # Check for absent PARAMETER values (WPFCB): 
      intermediateMessage('.22')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='WPFCB')
                                 ,'PARAMETER'
                                 ,c('TIME_COLLECTED','SAMPLE_ID')
                                 ,c('UID', 'SAMPLE_TYPE', 'SAMPLE_CAT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (WPFCB): %d"
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



