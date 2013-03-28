# valStructParameterDescriptions.r
#
# Performs structure check on tblPARAMETERDESCRIPTIONS2, which contains
# the metadata used in NRSA validation.
#
# 11/12/09 cws created
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#
require(RODBC)

valStructParameterDescriptions <- function(df, test='all')
# Performs structure checks on the NRSA table tblPARAMETERDESCRIPTIONS2.  
# Returns NULL if no errors have been found, or an Nx1 character matrix 
# describing the detected errors.
#
# ARGUMENTS:
# df        dataframe of NRSA parameter metadata
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
  intermediateMessage('Structure validation of parameter metadata ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c("FORMABBR", "PARAMETER", "LABEL", "UNITS"
                                    ,"SAMPLE_TYPE", "RANGETYPE", "RANGELOW"
                                    ,"RANGEHIGH", "LEGALVALUES"
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing parameter values
      intermediateMessage('.2')
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

      # Check for missing units values when a numeric range is specified.
      # (Not viable, as degrees and GPS coords have no specified units)
#      intermediateMessage('.3')
#      tt <- subset(df, !(is.na(rangeHigh) & is.na(rangeLow)))
#      pp <- stValMissingValues(tt, 'units')
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,ifelse(is.null(pp)
#                               ,"No Missing units values"
#                               ,"Missing units values exist (vital)"
#                               )
#                        )
#      } else {
#          probs <- rbind(probs, pp)
#      }

      # Check for unexpected rangeType values
      intermediateMessage('.4')
      pp <- stValUnexpectedValues(df, 'RANGETYPE', c('','TIME')
                                 ,c('PARAMETER','UNITS')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No unexpected RANGETYPE values"
                               ,"Unexpected RANGETYPE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for nonunique keys for parameters (non-parameters have missing
      # sampleType values).
      intermediateMessage('.5')
      tt <- subset(df, !(SAMPLE_TYPE=='' | is.na(SAMPLE_TYPE)))
      pp <- stValCountRows(tt, c('SAMPLE_TYPE', 'PARAMETER', 'UNITS'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No unexpected rangeType values"
                               ,"Unexpected rangeType values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

  }

  if(test %in% c('all','nonvital','synopsis')) {

      # Check for missing formAbbr values when validation info exists
      # Stuff listed here is likely either WRS form or not parameter
      # information.
      intermediateMessage('.6')
      tt <- subset(df, RANGEHIGH != '' & !is.na(RANGEHIGH) |
                       RANGELOW !='' & !is.na(RANGELOW)    |
                       LEGALVALUES != '' & !is.na(LEGALVALUES)
                  )
      pp <- stValMissingValues(tt, 'FORMABBR')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing FORMABBR values for validation data"
                               ,"Missing FORMABBR values for validation data exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for missing Label values
      intermediateMessage('.7')
      pp <- stValMissingValues(df, 'LABEL')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing LABEL values"
                               ,"Missing LABEL values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
  }

  return(probs)
}
# end of file
