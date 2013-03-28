# valStructVisits.r
#
# 10/02/09 cws Created
# 10/27/2009 mrc removed errant comma in list of args.
#  2/25/2010 cws moved source() calls to NRSAvalidation.r

require(RODBC)


valStructVisits <- function(df, test='all')
# Performs structure checks on the NRSA table tblIVISITS2.  Returns
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
  intermediateMessage('Structure validation of Visit/Verification form data '
                     ,loc='start'
                     )

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID','SITE_ID','DATE_COL','VISIT_NO'
                                    ,'SAMPLE_TYPE','STATE','TEAM_ID','XLAT_DEG'
                                    ,'XLAT_MIN','XLAT_SEC','XLON_DEG','XLON_MIN'
                                    ,'XLON_SEC','XLAT_DD','XLON_DD','VALXSITE'
                                    ,'XSTATUS','SITESAMP','GENCOM'
                                    ,'NOT_SAMPLED_TEMP','SAMPLED','NOT_SAMPLED'
                                    ,'NO_ACCESS','SAMP_STATUS','CHEM','WCHL'
                                    ,'PPCP','PERI','PCHL','PBIO','PAPA','ENTE'
                                    ,'SEDE','FTIS','BERW','VERT','BELG','PHYT'
                                    ,'SNAG','KICK','WPFC','STATUS_COM'
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
  }

  intermediateMessage('.  Done', loc='end')
  return(probs)
}

# end of file