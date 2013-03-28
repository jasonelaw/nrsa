eids <- function(...){
  pars <- list(...)
  if(is.list(pars[[1]])){
    pars <- pars[[1]]
  } 
  for (i in 1:length(pars)){
    pars[[i]] <- paste( names(pars[i]), pars[[i]], sep = ':')
  }
  pars$sep <- '/'
  ans <- do.call('paste', pars)
  ans
}

split.eids.data.frame <- function(x){
  ans <- split.eids(x$entity)
  x <- cbind(x, ans)
  return(x)
}

split.eids.character <- function(x){
  eidskey <- unique(x)
  eids <- strsplit(eidskey, '/|:')
  leids <- sapply(eids, length)
  eids <- c(eids, recursive = T)
  eids.df <- as.data.frame(matrix(eids, ncol = 2, byrow = T))
  eids.df$entity <- rep(eidskey, times = leids/2)
  eids.df.melt <- melt(eids.df, measure.var = 'V2')
  ans <- dcast(eids.df.melt, entity ~ V1)
  ans <- join(data.frame(entity = x), ans, by = 'entity', type = 'left')
  ans$entity <- NULL
  ans
}

split.eids.factor <- function(x){
  split.eids.character(as.character(x))
}

split.eids <- function(x){
  UseMethod('split.eids')
}
