# valStructFishPhoto.r
#
# 10/08/2009 mrc Created
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# 04/20/2010 ssr added NOT MARKED as transect type
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the FishPhoto table
#the FishPhoto field is a really puny table that can have almost any values for most vars.
#it requires very little testing





valStructFishPhoto <- function(df, test='all')

# Performs structure checks on the NRSA table tblFishPhoto2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA Fish Photo data
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
  intermediateMessage('Structure validation of fish photo section data ', loc='start')
  
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT'
                                    ,'PAGE', 'TAG' 
                                    ,'SAMPLE_TYPE', 'PHOTO_FILE'
                                    )
                                    , timing=timeThis
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
      intermediateMessage('.3missingTransectValues', loc='end')
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


      # Check for missing SAMPLE_TYPE values
      intermediateMessage('.4missingSampleType', loc='end')
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
          # Check for missing TAG values
      intermediateMessage('.5missingTag', loc='end')
      pp <- stValMissingValues(df, 'TAG', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TAG values"
                               ,"Missing TAG values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
 #-------------------------UNEXPECTED VALUES-------------------------------#     
 
      
     # Check for unexpected Sample_type values
        intermediateMessage('.6unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues (df,'SAMPLE_TYPE'
                                 ,c('FISHW','FISHB'
                                 )
                                 ,c('UID'), timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected Sample_Type values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

   # Check for unexpected TRANSECT values
      intermediateMessage('.7unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K', 'NONE','NOT MARKED')
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



#----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-TRANSECT-TAG-PAGE
      intermediateMessage('.8uniqueUID*TRANSECT*TAG*PAGE', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT','TAG', 'PAGE'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT- TAG-PAGE values: %d (vital)"
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



#valStructFishPhotoTest <- function()
# Tests valStructFishPhoto()
# This file is too dumb to test, no test function is written.

#end of file
