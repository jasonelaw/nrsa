# valStructTorrent.r
#
# 10/09/2009 mrc Created
# 12/22/2009 add timeThis
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the Torrent table

  
valStructTorrent <- function(df, test='all')

# Performs structure checks on the NRSA table tblTorrent2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA Torrent data
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
  intermediateMessage('Structure validation of torrent data ', loc='start')
  
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
  probs <- stValColumnPresence(df, c('UID', 'PARAMETER'
                                    ,'RESULT', 'FLAG' 
                                    ,'SAMPLE_TYPE'
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
      pp <- stValMissingValues(df, 'UID' , timing=timeThis)
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

      # Check for missing PARAMETER values
      intermediateMessage('.3missingParameterValues', loc='end')
      pp <- stValMissingValues(df, 'PARAMETER' , timing=timeThis)
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
      intermediateMessage('.4missingSampleType', loc='end')
      pp <- stValMissingValues(df, 'SAMPLE_TYPE' , timing=timeThis)
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
          
      
 #-------------------------UNEXPECTED VALUES-------------------------------#     
 
      
     # Check for unexpected Sample_type values
        intermediateMessage('.5unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues (df,'SAMPLE_TYPE'
                                 ,c('TORR'
                                 )
                                 ,c('UID')  , timing=timeThis
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

   # Check for unexpected PARAMETER values
      intermediateMessage('.6unexpectedPARAMETER', loc='end')
      pp <- stValUnexpectedValues (df,'PARAMETER'
                                 ,c('TSD01', 'TSD02', 'TSD03', 'TSD04', 'TSD05'
                                 ,'TSD06', 'TSD07', 'TSD08', 'TSD09', 'TSD10', 'TSD11')
                                 ,c('UID')  , timing=timeThis
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

     #  Check for uniqueness of each UID-PARAMETER
      intermediateMessage('.7uniqueUID*PARAMETER', loc='end')
      pp <- stValCountRows(df, c('UID','PARAMETER'), 1 , timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-PARAMETER values: %d (vital)"
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

#--------------------END OF valStructTorrent------------------------------------#

