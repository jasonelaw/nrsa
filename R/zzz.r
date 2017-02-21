".nrsa.Options" <-
    list(ProgressReports = TRUE)

.onAttach <- function(library, pkg)
{
    ## we can't do this in .onLoad
    unlockBinding(".nrsa.Options", asNamespace("nrsa"))
    pdescr <- packageDescription("nrsa")
    packageStartupMessage("Package ", pdescr$Package, "\nversion ", pdescr$Version, "\n",
        pdescr$Maintainer, "\n",
        "Type help(nrsa) for summary information\n")
    invisible()
}

suppressCat <- function(x){
  sink(tmp <- tempfile())
  ret <- force(x)
  sink()
  file.remove(tmp)
  ret 
}