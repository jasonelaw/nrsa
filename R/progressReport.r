#'Print a progress report
#'This function prints a progress report.  The time is appended so that the
#'elapsed time between reports can be used to track calculation progress.
#'@param msg the message to be printed
progressReport <- function(msg){
  if(nrsa.options()$ProgressReports){
    message(Sys.time(), ": ", msg)
  }
  invisible()
}
