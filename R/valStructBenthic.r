# valStructBenthic.r
#
# 10/01/2009 mrc Created
# 10/21/2009 added timeThis
# 12/07/2009 mrc: changed sample_types to reflect new reality
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)
#intermediateMessages <- TRUE



valStructBenthic <- function(df, test='all')
# Performs structure checks on the NRSA table tblBENTHIC2.  Returns NULL if
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
# ASSUMPTIONS: 
#

{
  intermediateMessage('Structure validation of benthic data ', loc='end')
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT',
                                    'PARAMETER', 'RESULT', 
                                    'SAMPLE_TYPE' 
                                    ) , timing=timeThis
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



      # Check for missing PARAMETER values
      intermediateMessage('.4missingParameterValues', 'loc=end')
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
      intermediateMessage('.5missingSampleType', loc='end')
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
      # Check for unexpected TRANSECT values
      intermediateMessage('.6unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K')
                                 ,'UID', timing=timeThis
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
         # Check for unexpected PARAMETER values
      intermediateMessage('.7unexpectedParameter', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('SECOND_HABITAT', 'LOCATION', 'CHANNEL', 'SUBSTRATE', 'EDGE', 'HABITAT')
                                 ,c('UID', 'TRANSECT'), timing=timeThis
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
           # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.8unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('BERWW','BELGW', 'BELGB')
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
      # Check for uniqueness of each UID-TRANSECT-PARAMETER
      intermediateMessage('.8uniqueUID*Transect*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'PARAMETER', 'SAMPLE_TYPE'), timing=timeThis, 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-PARAMETER-SAMPLE_TYPE values: %d (vital)"
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
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.9absentTransectValues', loc='end')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'  , timing=timeThis
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
      intermediateMessage('.10absentParameter', loc='end')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                ,c('SECOND_HABITAT', 'LOCATION', 'CHANNEL', 'SUBSTRATE', 'EDGE', 'HABITAT')
                                 ,c('UID', 'TRANSECT')  , timing=timeThis
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



