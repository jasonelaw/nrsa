# valStructVisAssess.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblVISUALASSESSMENT2
# 10/20/09 cws Added unit test.  Modified structure check to include tests for
#          nonunique UID-SAMPLE_TYPE-PARAMETER combinations and a check for
#          absent parameters.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#

# Contains functions valStructVisAssess 

require(RODBC)
#intermediateMessages <- TRUE

valStructVisAssess <- function(df, test='all')
# Performs structure checks on the NRSA table tblVISUALASSESSMENT2.  Returns NULL if
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
  intermediateMessage('Structure validation of visual assessment data ', loc='start')
  
  timeThis <- FALSE
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'PARAMETER', 'RESULT', 'FLAG'
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

      # Check for missing UID values
      intermediateMessage('.2')
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

      
      # Check for missing PARAMETER values
      intermediateMessage('.3')
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
      intermediateMessage('.4')
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

      # Check for absent PARAMETER values
      intermediateMessage('.5')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                 ,c('AGR_PAS', 'AGR_STCK', 'BEAVER', 'BVFLMOD',
                                    'DLANDUSE', 'GEN_COM', 'APPEALING', 'FORAGECL',
                                    'IND_FIRE', 'MAN_DAMS', 'PRISTINE', 'REC_PARK',
                                    'RES_ROAD', 'REC_TRAS', 'REC_TRLS', 'WEATHCOM',
                                    'RES_BRID', 'AGR_CROP', 'MAN_CHAN', 'REC_FILM',
                                    'RES_LAWN', 'RES_PIPE', 'RES_RES', 'IND_COML',
                                    'RES_SEWG', 'IND_IND', 'MAN_ANGL', 'RES_CON',
                                    'AGR_IRRG', 'AGR_WITH', 'MAN_WLF', 'IND_OIL',
                                    'IND_MINE', 'REC_PRIM', 'RES_DUMP', 'MAN_FISH',
                                    'IND_LOG', 'IND_ODOR', 'IND_POWR', 'AGR_ORCH',
                                    'MAN_DWT', 'MAN_DRED', 'AGR_POUL', 'MAN_LIME')
                                 ,c('UID'), timing=timeThis
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

      # Check for uniqueness of each UID
      intermediateMessage('.6')
      pp <- stValCountRows(df, c('UID','SAMPLE_TYPE','PARAMETER'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected PARAMETER values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('AGR_PAS', 'AGR_STCK', 'BEAVER', 'BVFLMOD',
                                    'DLANDUSE', 'GEN_COM', 'APPEALING', 'FORAGECL',
                                    'IND_FIRE', 'MAN_DAMS', 'PRISTINE', 'REC_PARK',
                                    'RES_ROAD', 'REC_TRAS', 'REC_TRLS', 'WEATHCOM',
                                    'RES_BRID', 'AGR_CROP', 'MAN_CHAN', 'REC_FILM',
                                    'RES_LAWN', 'RES_PIPE', 'RES_RES', 'IND_COML',
                                    'RES_SEWG', 'IND_IND', 'MAN_ANGL', 'RES_CON',
                                    'AGR_IRRG', 'AGR_WITH', 'MAN_WLF', 'IND_OIL',
                                    'IND_MINE', 'REC_PRIM', 'RES_DUMP', 'MAN_FISH',
                                    'IND_LOG', 'IND_ODOR', 'IND_POWR', 'AGR_ORCH',
                                    'MAN_DWT', 'MAN_DRED', 'AGR_POUL', 'MAN_LIME')
                                 ,c('UID'), timing=timeThis
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
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('ASSE')
                                 ,c('UID')
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


  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}


