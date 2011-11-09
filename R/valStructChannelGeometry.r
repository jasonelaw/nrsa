# valStructChannelGeometry.r
#
# 10/01/09 cws Created
# 10/28/2009  mrc - added LINE as a variable, used in slope
# 10/28/2009 mrc - ARRIVE and LEAVE only have TRANLINE= NONE as they are TRANLINE independent
#  2/25/2010 cws moved source() calls to NRSAvalidation.r

require(RODBC)


valStructChannelGeometry <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANNELGEOMETRY2.  Returns
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
  intermediateMessage('Structure validation of channel geometry data ', loc='start')

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
                                    ,'TRANLINE', 'LINE','PARAMETER', 'RESULT', 'UNITS'
                                    ,'FLAG', 'BANK','METHOD'
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

      # Check for missing TRANLINE values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'TRANLINE')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TRANLINE values"
                               ,"Missing TRANLINE values exist (vital)"
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

      # Check for missing SAMPLE_TYPE values
      intermediateMessage('.6')
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

      # Check for missing BANK values
      intermediateMessage('.7')
      pp <- stValMissingValues(df, 'BANK')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing BANK values"
                               ,"Missing BANK values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for missing METHOD values (as data, not in structure checks)

      # Check for missing BANK values
      intermediateMessage('.9')
      pp <- stValMissingValues(df, 'BANK')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing BANK values"
                               ,"Missing BANK values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent SAMPLE_TYPE values
      intermediateMessage('.10')
      pp <- stValAbsentValues(df
                             ,'SAMPLE_TYPE'
                             ,c('PHAB_CHANBFRONT', 'PHAB_SLOPE')
                             ,NULL
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent SAMPLE_TYPE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected TRANSECT values in boatable phab
      # Missing values are previously noted and won't be included here.
      intermediateMessage('.11')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANBFRONT')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K'
                                   ,'XA','XB','XC','XD','XE','XF','XG','XH'
                                   ,'XI','XJ','XK','NONE'
                                   )
                                 ,c('UID','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
      # Check for unexpected TRANSECT values in wadeable slope & bearing data
      # Missing values are previously noted and won't be included here.
      intermediateMessage('.12')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','NONE')
                                 ,c('UID','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values for SAMPLE_TYPE=='PHAB_SLOPE': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected METHOD values (later, not here)

      # Check for unexpected TRANLINE values in boatable phab.  TRANLINE is a
      # key for GPS data collected at the top of the boatable form, so will
      # only be non-"NONE" for the lat/long parameters
      intermediateMessage('.15')
      pp <- stValUnexpectedValues(subset(df
                                        ,SAMPLE_TYPE=='PHAB_CHANBFRONT' &
                                         PARAMETER %in% c('LATDD_TOP','LATMM_TOP'
                                                         ,'LATSS_TOP','LONGDD_TOP'
                                                         ,'LONGMM_TOP','LONGSS_TOP'
                                                         )
                                        )
                                 ,'TRANLINE'
                                 ,c('BANK','MID')
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANLINE values for boatable GPS lat/lon values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      pp <- stValUnexpectedValues(subset(df
                                        ,SAMPLE_TYPE=='PHAB_CHANBFRONT' &
                                         !(PARAMETER %in% c('LATDD_TOP'
                                                           ,'LATMM_TOP'
                                                           ,'LATSS_TOP'
                                                           ,'LONGDD_TOP'
                                                           ,'LONGMM_TOP'
                                                           ,'LONGSS_TOP'
                                                           ,'ARRIVE'
                                                           ,'LEAVE'
                                                           )
                                          )
                                        )
                                 ,'TRANLINE'
                                 ,c('NONE')
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANLINE values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected TRANLINE values in wadeable slope & bearing data
      intermediateMessage('.16')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                                 ,'TRANLINE'
                                 ,'NONE'
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANLINE values for SAMPLE_TYPE='PHAB_SLOPE': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected BANK values in boatable phab
      intermediateMessage('.17')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANBFRONT')
                                 ,'BANK'
                                 ,c('Left','Right','NONE')
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected BANK values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected BANK values in wadeable slope & bearing data
      intermediateMessage('.18')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                                 ,'BANK'
                                 ,'NONE'
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected BANK values for SAMPLE_TYPE='PHAB_SLOPE': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-PARAMETER ?

  }

  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent TRANSECT values at each UID of boatable phab
      intermediateMessage('.19')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANBFRONT')
                             ,'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,c('UID')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSECT values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent TRANSECT values at each UID of wadeable slope
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                             ,'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J')
                             ,c('UID')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSECT values for SAMPLE_TYPE='PHAB_SLOPE': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected PARAMETER values in boatable phab
      intermediateMessage('.20')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANBFRONT')
                                 ,'PARAMETER'
                                 ,c(# Top of form
                                    'ARRIVE','LEAVE', 'LATDD_TOP'
                                   ,'LATMM_TOP','LATSS_TOP','LONGDD_TOP'
                                   ,'LONGMM_TOP','LONGSS_TOP'
                                    # Bottom of form
                                   ,'ACTRANSP','INTDTRAN','SLOPE_ND'
                                   ,'SLOPE','SLOPE2','SLOPE3','BEAR','BEAR2'
                                   ,'BEAR3','DISTANCE','DISTANCE2','DISTANCE3'
                                   ,'WAYPT','LATDD','LATMM','LATSS','LONGDD'
                                   ,'LONGMM','LONGSS'
                                   ,'WAYPT2','LATDD2','LATMM2','LATSS2','LONGDD2'
                                   ,'LONGMM2','LONGSS2'
                                   ,'WAYPT3','LATDD3','LATMM3','LATSS3','LONGDD3'
                                   ,'LONGMM3','LONGSS3'
                                   )
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected PARAMETER values in wadeable slope & bearing data
      intermediateMessage('.21')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                                 ,'PARAMETER'
                                 ,c('SLOPE','SLOPE2','SLOPE3','BEARING','BEARING2'
                                   ,'BEARING3','PROP','PROP2','PROP3'
                                   )
                                 ,c('UID','TRANSECT','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for SAMPLE_TYPE='PHAB_SLOPE': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values in boatable phab
      # Note WAYPT may occur, but is not required to occur, so it is not listed
      # in this check.
      intermediateMessage('.22')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANBFRONT')
                             ,'PARAMETER'
                             ,c(# Top of form
                                'ARRIVE','LEAVE', 'LATDD_TOP'
                               ,'LATMM_TOP','LATSS_TOP','LONGDD_TOP'
                               ,'LONGMM_TOP','LONGSS_TOP'
                                # Bottom of form
                               ,'ACTRANSP','INTDTRAN','SLOPE_ND'
                               ,'SLOPE', 'BEAR','DISTANCE'
                               ,'LATDD','LATMM','LATSS','LONGDD'
                               ,'LONGMM','LONGSS'
                               )
                             ,c('UID','TRANSECT')
                             )

      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values for SAMPLE_TYPE='PHAB_CHANBFRONT': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values in wadeable slope & bearing data
      intermediateMessage('.23')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_SLOPE')
                             ,'PARAMETER'
                             ,c('SLOPE','BEARING','PROP')
                             ,c('UID','TRANSECT')
                             )

      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values for SAMPLE_TYPE='PHAB_SLOPE': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected SAMPLE_TYPE values
      intermediateMessage('.24')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANBFRONT', 'PHAB_SLOPE')
                                 ,c('UID','TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected SAMPLE_TYPE values: %d "
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

fixit <- function(df)
{
#  df <- sqlFetch(nrsa, 'tblCHANNELGEOMETRY2', stringsAsFactors=FALSE)
  df$UID <- as.character(df$BATCHNO)
  df$TRANSECT <- trimws(df$TRANSECT)
  df$PARAMETER <- trimws(df$PARAMETER)
  df$TRANLINE <- trimws(df$TRANLINE)
  df$BANK <- trimws(df$BANK)
  df$METHOD <- trimws(df$METHOD)
  df$UNITS <- trimws(df$UNITS)
  df$RESULT <- trimws(df$RESULT)
  df$FLAG <- trimws(df$FLAG)

  df <- df[c('UID','TRANSECT','TRANLINE','PARAMETER','RESULT','UNITS'
            ,'SAMPLE_TYPE','METHOD','BANK','FLAG'
            )
          ]
}


