".nrsa.Options" <-
    list(NRSACalcLocation = 'S:/jlaw/pawmap/out/results/',
         NRSAvalidationLocation = 'S:/jlaw/pawmap/out/validation/',
         NRSAKeyColumns = c('UID', 'BATCHNO','TRANSECT','TRANSDIR','STATION','REP'
                   ,'LINE', 'PAGE', 'TRANLINE','BANK','ANALYTE', 'SAMPLE_TYPE'
                   ,'SAMPLE_CAT'
                   ))

.onAttach <- function(library, pkg)
{
    ## we can't do this in .onLoad
    unlockBinding(".nrsa.Options", asNamespace("nrsa"))
    packageStartupMessage("Package `nrsa', version 0.1\n",
        "Jason Law\n",
        "Type help(nrsa) for summary information\n")
    invisible()
}
