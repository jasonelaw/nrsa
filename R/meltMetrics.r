meltMetrics <- function(...){
  # If arg is a mat assumes that it has dimnames uid x metric
  args <- list(...)
  ans <- ldply(args, function(x){
    if (is.data.frame(x)){
      return(melt(x, id.var = 'uid', variable.name = 'metric', 
                  value.name = 'result'))
    } else if (is.matrix(x)){
      return(melt(x, varnames = c('uid', 'metric'), 
                  value.name = 'result'))
    } else {
      stop("Arguments must either matrix or data.frame")
    }
  })
  ans <- data.frame(allFacToChar(ans))
  arrange(ans, uid, metric)
}

castMetrics <- function(...){
  x <- rbindMetrics(...)
  dcast(x, uid ~ metric, value.var = 'result')
}

rbindMetrics <- function(...){
  x <- list(...)
  checkNames <- function(x){
    nms.diff <- setdiff(c('uid', 'metric', 'result'), names(x))
    identical(nms.diff, character(0))
  } 
  stopifnot(all(sapply(x, checkNames)))
  x <- lapply(x, allFacToChar)
  ans <- rbind.fill(x)
  arrange(ans[, c('uid', 'metric', 'result')], uid, metric)
}
