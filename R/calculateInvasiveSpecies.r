#' Calculates the invasive species metrics.
#' 
#' Calculates the proportion of transects at which the species was observed.  The
#' species id is used in the metric name, except species on the original NRSA EPA list
#' are mapped to the NRSA metric names.
#' @param uid a vector of site identifiers
#' @param species a vector of species ids
#' @param is.present a logical vector (or 0 and 1) which identifies whether that
#' species was present at that transect
#' @return a 'metric' data.frame
#' @export
#' @importFrom plyr ddply mapvalues
calculatePropTranInvPresent <- function(uid, species, is.present){
  x <- data.frame(uid, species, is.present)
  f <- function(x){
    mean(x$is.present, na.rm = T)
  }
  ans <- ddply(x, .(uid, species), summarize, result = mean(is.present, na.rm = T))
  ans$metric <- paste0('f_', ans$species)
  ans$metric <- mapvalues(ans$metric, EpaList(), names(EpaList()))
  ans$species <- NULL
  return(ans)
}

#' Calculate total invasive score
#' 
#' Calculates the ip_score metric from the NRSA metrics.  This is just the sum of
#' the other metric scores for the site.  The metric is somewhat concerning if 
#' a survey doesn't use the stock EPA invasives list, because the metrics would
#' be incomparable.  The mean might be a better metric, although not much better
#' 
#' For the ip_score to be useful, each site must have the same number and identity
#' of invasive species.  For this implementation, the metrics are subsetted
#' so that only the species on the original EPA NRSA list (f_myrspi, f_hydver, 
#' f_eiccra, f_nympel, f_lytsal, f_arudon, f_butumb, f_tamspp, f_rosmul, 
#' f_eupesu) are included in the calculation.
#' @param uid a vector of site identifiers
#' @param result a vector of invasive metrics for each site.
#' @return a 'metric' data.frame
#' @export
calculateInvasiveScore <- function(uid, metric, result, FUN = sum){
  i <- as.character(metric) %in% names(EpaList())
  ip_score <- tapply(result[i], uid[i], FUN)
  convertNamedVectorToMetricDF(ip_score)
}

EpaList <- function(){
  c(f_myrspi = 'E_WTRMILF', f_hydver = 'HYDRILLA', f_eiccra = 'W_HYACINTH', 
    f_nympel = 'YLW_FLTHEAR', f_lytsal = 'P_LSTRIFE', f_arudon = 'G_REED',
    f_butumb = 'FLWR_RUSH', f_tamspp = 'SALT_CED', f_rosmul = 'MF_ROSE', 
    f_eupesu = 'SPURGE')
}

PortlandList <- function(){
  c("TSN24852", "TSN36089", "TSN41335", "TSN30414", "TSN30705", 
    "TSN20889", "TSN29393", "TSN43194", "TSN25864", "TSN184481", 
    "TSN503154")
}

getInvasiveData <- function(parameters){
  on.exit(odbcClose(chan))
  chan <- odbcConnect('NRSA2')
  data <- fetchNRSATable(chan, 'tblINVASIVELEGACY2')

  df <- subset(data,
               PARAMETER %in% parameters, 
               select=c('UID','TRANSECT','PARAMETER','RESULT'))
  return(df)
}

