#'Print a progress report
#'
#'This function prints a progress report.  The time is appended so that the
#'elapsed time between reports can be used to track calculation progress.
#'The printing of progress reports is controlled by the \link{nrsa.options} function.
#'Additionally \code{plyrProgress} will be used to print the progress bars for ddply
#'functions when \code{nrsa.options(ProgressReports = TRUE)}.
#'@param msg the message to be printed
progressReport <- function(msg, metrics = NULL){
  met.message <- if(is.null(metrics)) NULL else " Metrics calculated: " 
  if(nrsa.options()$ProgressReports){
    message(Sys.time(), ": ", msg, met.message, toString(unique(metrics)))
  }
  invisible()
}

#' @rdname progressReport
#' @param progress the name of the plyr progress reporter to use.
plyrProgress <- function(progress = 'text'){
  if (!nrsa.options()$ProgressReports){
    progress <- 'none'
  }
  return(progress)
}
