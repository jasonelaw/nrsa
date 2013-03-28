mergeMetrics <- function(...){
  metrics <- list(...)
  metrics <- rbind.fill(metrics)
  metrics <- melt(metrics, id.var = 'uid', na.rm = T)
  metrics <- dcast(metrics, uid ~ variable)
  return(metrics)
}
