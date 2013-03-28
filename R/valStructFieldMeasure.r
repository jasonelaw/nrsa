# valStructFieldMeasure.r
#
# 10/01/09 cws Created but incomplete.  Appears that SAMPLE_ID and BANK columns
#          may be truncated.  Also there may be multiple parameter spellings:
#          TEMP & TEMPERATURE, COND & CONDUCTIVITY.
# 10/30/2009 mrc.  Changed parameter values to only include TEMPERATURE and CONDUCTIVITY
#          BANK column was widened to include full value
#          SAMPLE_TYPE is as intended FIELDMEAS
#          Added valStructFieldMeasureTest
#  2/25/2010 cws moved source() calls to NRSAvalidation.r

#
require(RODBC)


valStructFieldMeasure <- function(df, test='all')
# Performs structure checks on the NRSA table tblFIELDMEASURE2.  Returns
# NULL if no errors have been found, or an Nx1 character matrix describing the
# detected errors.
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
  intermediateMessage('Structure validation of Field Measurements data ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1', loc='end')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'BANK', 'SAMPLE_TYPE'
                                    ,'PARAMETER', 'RESULT', 'FLAG'
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
  
  #-----------------MISSING VALUES---------------------------------------------#
      # Check for missing UID values
      intermediateMessage('.2', loc='end')
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

      # Check for missing PARAMETER values
      intermediateMessage('.3', loc='end')
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
      intermediateMessage('.4', loc='end')
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
     #----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-PARAMETER
      intermediateMessage('.5uniqueUID*Transect*Bank*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','PARAMETER', 'TRANSECT', 'BANK'), 1)
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
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected PARAMETER values
      intermediateMessage('.6', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('CONDUCTIVITY','CORRECTED','DO','pH'
                                   ,'TEMPERATURE','TIME'
                                   ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                                   )
                                 ,'UID'
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

      # Check for absent PARAMETER values
      intermediateMessage('.7', loc='end')
      pp <- stValAbsentValues(df
                             ,'PARAMETER'
                             ,c('CONDUCTIVITY','CORRECTED','DO','pH'
                               ,'TEMPERATURE','TIME'
                               ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                               )
                             ,'UID'
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

