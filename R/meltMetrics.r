#'Melts metric data
#'
#'Melts metric data. If data.frame, then there must be a UID column.  Dimnames must be present for a matrix.
#'@param ... metric data either in data.frame or matrix form; assumes in uid x metric form.
#'@export
meltMetrics <- function(...){
  # If arg is a mat assumes that it has dimnames uid x metric
  args <- list(...)
  ans <- ldply(args, function(x){
    if (is.data.frame(x)){
      return(reshape2::melt(x, id.var = 'uid', variable.name = 'metric', 
                  value.name = 'result'))
    } else if (is.matrix(x)){
      return(reshape2::melt(x, varnames = c('uid', 'metric'), 
                  value.name = 'result'))
    } else {
      stop("Arguments must either matrix or data.frame")
    }
  })
  ans <- data.frame(allFacToChar(ans))
  is.na(ans$result) <- !is.finite(ans$result)
  arrange(ans, uid, metric)
}

#'@export
castMetrics <- function(...){
  x <- rbindMetrics(...)
  x <- reshape2::dcast(x, uid ~ metric, value.var = 'result')
  x[-1] <- lapply(x[-1], function(x) {if (is.character(x)) type.convert(x, as.is = T) else x})
  x
}

#'@export
rbindMetrics <- function(...){
  x <- list(...)
  checkNames <- function(x){
    nms.diff <- setdiff(c('uid', 'metric', 'result'), names(x))
    identical(nms.diff, character(0))
  } 
  stopifnot(all(sapply(x, checkNames)))
  x <- lapply(x, allFacToChar)
  ans <- rbind.fill(x)
  if(is.numeric(ans$result)){
    is.na(ans$result) <- !is.finite(ans$result)
  }
  arrange(ans[, c('uid', 'metric', 'result')], uid, metric)
}

#'@export
mergeMetrics <- function(...){
  metrics <- list(...)
  metrics <- reshape2::rbind.fill(metrics)
  metrics <- reshape2::melt(metrics, id.var = 'uid', na.rm = T)
  metrics <- reshape2::dcast(metrics, uid ~ variable)
  return(metrics)
}

#'@rdname meltMetrics
#'@examples
#'d1 <- data.frame(uid = 1:9, metric = 'xcl', result = 1:9)
#'fillMissingMetrics(d1, uids = 1:10, metric = 'xcl')
#'@export
fillMissingMetrics <- function(x, uids, metrics){
  nas <- expand.grid(uid = uids, metric = metrics, result = NA)
  plyr:::join(x, nas, by = c('uid', 'metric'), type = 'full')
}
