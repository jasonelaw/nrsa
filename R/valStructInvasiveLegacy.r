# valStructInvasiveLegacy.r
#
# 10/02/09 cws Created
# 01/13/10 SSR added UNITS to expected columns, and DISTANCE to expected PARAMETER
# 02/22/10 cws Readded UNITS to expected columns.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#

require(RODBC)


valStructInvasiveLegacy <- function(df, test='all')
# Performs structure checks on the NRSA table tblINVASIVELEGACY2.  Returns
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
  intermediateMessage('Structure validation of Invasive species and Legacy tree data ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'SAMPLE_TYPE'
                                    ,'PARAMETER', 'RESULT', 'UNITS', 'FLAG'
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

      # Check for missing PARAMETER values
      intermediateMessage('.4')
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

      # Check for missing SAMPLE_TYPE values -- IS THIS NEEDED?
      intermediateMessage('.5')
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

  }

  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected PARAMETER values
      intermediateMessage('.6')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c("DBH", "DISTANCE", "E_WTRMILF", "FLWR_RUSH", "G_REED"
                                   ,"HEIGHT", "HYDRILLA", "MF_ROSE"
                                   ,"NO_INVASIVES", "NOT_VIS", "P_LSTRIFE"
                                   ,"SALT_CED", "SPECIES", "SPURGE", "TREE_TYP"
                                   ,"W_HYACINTH", "YLW_FLHEAR"
                                   )
                                 ,c('UID','TRANSECT')
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

      # Check for absent PARAMETER values.  This is somewhat difficult to
      # check.  If NOT_VIS present, then DBH, HEIGHT, NOT_VIS, SPECIES and
      # TREE_TYP will not be, and visa vers.  If NO_INVASIVES is checked, then
      # E_WTRMILF, FLWR_RUSH, G_REED, HYDRILLA", P_LSTRIFE, SALT_CED, SPURGE,
      # W_HYACINTH and YLW_FLHEAR will not be present; otherwise one or more
      # of the invasive species parameters will be present.
      intermediateMessage('.7')
      if(test=='synopsis') {
          probs <- rbind(probs,"(Absent PARAMETER values not checked)")
      }

  }

  intermediateMessage('.  Done', loc='end')
  return(probs)
}


