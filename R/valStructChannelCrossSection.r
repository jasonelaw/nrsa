# valStructChannelCrossSection.r
#
# 10/06/2009 mrc Created
# 10/21/2009 mrc added timeThis
# 10/28/2009 mrc changed expected TRANSDIR from "X" to "NONE"
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the channelcrossection table





valStructChannelCrossSection <- function(df, test='all')

# Performs structure checks on the NRSA table tblCHANNELCROSSSECTION2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA benthic data
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
  intermediateMessage('Structure validation of channel cross section data ', loc='start')
  timeThis <- FALSE
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1correctColumnNames')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'TRANSDIR'
                                    ,'PARAMETER', 'RESULT' 
                                    ,'SAMPLE_TYPE', 'FLAG' 
                                    ), timing=timeThis
                                    
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }
  
  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
#-----------------------------MISSING VALUES-----------------------------------#
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
 
  # Check for missing TRANSDIR values
      intermediateMessage('.4MIssingTransdirValues', loc='end')
      pp <- stValMissingValues(df, 'TRANSDIR', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TRANSDIR values"
                               ,"Missing TRANSDIR values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing PARAMETER values
      intermediateMessage('.5missingParameterValues', loc='end')
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
      intermediateMessage('.6missingSampleType', loc='end')
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
 #-------------------------UNEXPECTED VALUES------------------------------------#
   # Check for unexpected TRANSDIR values in Streams
      intermediateMessage('.7UnexpectedTransdirW', loc='end')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW', timing=timeThis)
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values with SAMPLE_TYPE='PHAB_CHANW': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
        # Check for unexpected TRANSDIR values in Thal
      intermediateMessage('.8UexpectedTransdirT', loc='end')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC', 'NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values with SAMPLE_TYPE='PHAB_THALW': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      # Check for unexpected TRANSECT values
      intermediateMessage('.9unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K', 'NONE',
                                 'XA', 'XB', 'XC', 'XD', 'XE', 'XF', 'XG', 'XH', 'XI', 'XJ', 'XK'
                                 )
                                 ,c('UID') , timing=timeThis
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

          # Check for unexpected PARAMETER values- Fronts
      intermediateMessage('.10unexpectedParameterFront', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
      pp <- stValUnexpectedValues(tt
                                 ,'PARAMETER'
                                 ,c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
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
    }
  # Check for unexpected PARAMETER values- Backs
      intermediateMessage('.11unexpectedParameterBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
      pp <- stValUnexpectedValues(tt
                                 ,'PARAMETER'
                                 ,c('XSIZE_CLS', 'SUB_5_7')
                                 ,c('UID', 'TRANSECT')
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
    }
           # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.12unexpectedSampleType')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANW','PHAB_THALW')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
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
      # Check for uniqueness of each UID-TRANSECT-TRANSDIR-PARAMETER
      intermediateMessage('.13uniqueUID*Transect*Transdir*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'TRANSDIR', 'PARAMETER'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
  
}
  # Perform nonvital tests if requested
     if(test %in% c('all','nonvital','synopsis')) {
 #----------------------------ABSENT VALUES-------------------------------------#
  
     # Check for absent TRANSDIR values at each UID-TRANSECT in Fronts
      intermediateMessage('.14Transdir@UID-TRANSECTFront', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW', timing=timeThis)
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC')
                                 ,c('UID', 'TRANSECT')
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_CHANW': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for absent TRANSDIR values at each UID-TRANSECT in Back
      intermediateMessage('.15absentTRANSDIR@UID-TRANSECTBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC', 'NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_THALW': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

  
  
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.16absentTransectValues', loc='end')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,c('UID')
                             , timing=timeThis
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
      intermediateMessage('.17absentParameter-Fronts', loc='end')
    tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
      pp <- stValAbsentValues(tt
                                 ,'PARAMETER'
                                 ,c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER-Fronts values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
    }

    # Check for absent PARAMETER values- Backs
      intermediateMessage('.18absentParameterBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
      pp <- stValAbsentValues(tt
                                 ,'PARAMETER'
                                 ,c('XSIZE_CLS', 'SUB_5_7')
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

  }


  intermediateMessage('.  Done', loc='end')
  return(probs)

}



