".nrsa.Options" <-
    list(NRSACalcLocation = 'S:/jlaw/pawmap/metric_calculation/out/',
         NRSAvalidationLocation = 'S:/jlaw/pawmap/out/validation/',
         NRSAKeyColumns = c('UID', 'BATCHNO','TRANSECT','TRANSDIR','STATION','REP'
                   ,'LINE', 'PAGE', 'TRANLINE','BANK','ANALYTE', 'SAMPLE_TYPE'
                   ,'SAMPLE_CAT'
                   ),
         NRSADSN = c('NRSA_REP_64'),
         ProgressReports = FALSE)

.onAttach <- function(library, pkg)
{
    ## we can't do this in .onLoad
    unlockBinding(".nrsa.Options", asNamespace("nrsa"))
    packageStartupMessage("Package `nrsa', version 0.1\n",
        "Jason Law\n",
        "Type help(nrsa) for summary information\n")
    invisible()
}
