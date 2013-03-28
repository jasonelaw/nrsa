# valStructVisRip.r
#
# 10/16/2009 mrc Created
# 10/22/2009 add timeThis
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the channelcrossection table



valStructVisRip <- function(df, test='all')

# Performs structure checks on the NRSA table tblVisRip2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA VisRip data
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
  intermediateMessage('Structure validation of visual riparian data ', loc='start')
  
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'TRANSDIR'
                                    ,'PARAMETER', 'RESULT' 
                                    ,'SAMPLE_TYPE', 'FLAG'   
                                    )
                                  ,timing = timeThis 
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }
  
  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
#---------------------------------MISSING VALUES---------------------------------#
      # Check for missing UID values
      intermediateMessage('.2missingUIDValues', loc='end')
      pp <- stValMissingValues(df, 'UID',timing = timeThis)
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
      pp <- stValMissingValues(df, 'TRANSECT',timing = timeThis)
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
      pp <- stValMissingValues(df, 'TRANSDIR',timing = timeThis)
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
      pp <- stValMissingValues(df, 'PARAMETER',timing = timeThis)
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
      pp <- stValMissingValues(df, 'SAMPLE_TYPE',timing = timeThis)
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

   # Check for unexpected TRANSDIR values in Streams
      intermediateMessage('.7UnexpectedTransdir', loc='end')
      pp <- stValUnexpectedValues(df, 'TRANSDIR'
                                 ,c('LF','RT')
                                 ,c('UID', 'TRANSDIR') ,timing = timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values : %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
#-------------------------UNEXPECTED VALUES-------------------------------#       
      # Check for unexpected TRANSECT values
      intermediateMessage('.8unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K', 'X',
                                 'XA', 'XB', 'XC', 'XD', 'XE', 'XF', 'XG', 'XH', 'XI', 'XJ', 'XK'
                                 )
                                 ,c('UID') ,timing = timeThis
                                )
     if(test=='synopsis') {
         probs <- rbind(probs
                                ,ifelse(is.null(pp), 0, nrow(pp))
                             )
                       
     } else {
         probs <- rbind(probs, pp)
     }
  #   # Check for unexpected PARAMETER values
     intermediateMessage('.8aunexpectedParameter', loc='end')
     pp <- stValUnexpectedValues (df,'PARAMETER'
                               ,c('BARE', 'BUILD', 'CANBTRE', 'CANSTRE'
                                ,'CANVEG', 'GCNWDY', 'GCWDY', 'LANDFL'
                                ,'LOG', 'MINE','PARK','PAST','PAVE','PIPES'
                                 ,'ROAD','ROW','UNDERVEG','UNDNWDY','UNDWDY'
                                 ,'WALL'
                                )
                                ,c('UID') ,timing = timeThis
                                )
     if(test=='synopsis') {
         probs <- rbind(probs
                                ,ifelse(is.null(pp), 0, nrow(pp))
                               )
                       
     } else {
         probs <- rbind(probs, pp)
     }  
 #-------------------------UNIQUE VALUES-------------------------------# 
 
      # Check for uniqueness of each UID-TRANSECT-TRANSDIR-PARAMETER
      intermediateMessage('.9uniqueUID*Transect*Transdir*Parameter*sample_type', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'TRANSDIR', 'PARAMETER', 'SAMPLE_TYPE'), 1,timing = timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR-PARAMETER-SAMPLE_TYPE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
  
}
  # Perform nonvital tests if requested
     if(test %in% c('all','nonvital','synopsis')) {

#---------------------------------Absent tests---------------------------------------#  
     # Check for absent TRANSDIR values at each UID-TRANSECT
      intermediateMessage('.10Transdir@UID-TRANSECT', loc='end')
          tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT')
                                 ,c('UID', 'TRANSECT') ,timing = timeThis
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

      # Check for absent TRANSDIR values at each UID-TRANSECT
      intermediateMessage('.11Transdir@UID-TRANSECT', loc='end')
          tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANB')
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT')
                                 ,c('UID', 'TRANSECT')  ,timing = timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_CHANB': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }


      # Check for absent TRANSECT values at each UID
      intermediateMessage('.12absentTransectValues', loc = 'end')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,c('UID') ,timing = timeThis
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
      intermediateMessage('.13absentParameter', loc='end')
            tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'PARAMETER'
                                 ,c('BARE', 'BUILD', 'CANBTRE', 'CANSTRE'
                                 ,'CANVEG', 'GCNWDY', 'GCWDY', 'LANDFL'
                                 ,'LOG', 'MINE','PARK','PAST','PAVE','PIPES'
                                 ,'ROAD','ROW','UNDERVEG','UNDNWDY','UNDWDY'
                                 ,'WALL')
                                 ,c('UID', 'TRANSECT'),timing = timeThis
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



