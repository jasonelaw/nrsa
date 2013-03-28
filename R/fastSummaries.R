#'Helper function for fast summaries
#'
#'\code{fastSummaries} is a helper function that accepts a vector or matrix, a list of grouping
#'factors, and function that calculates summaries by group.
#'@param X a vector or matrix
#'@param INDEX a list of factors
#'@param FUN a function that accepts two arguments a numeric matrix or vector and 
#'a grouping vector and returns a vector of summary statistics for each member of the group
#'@return a data.frame with the grouping variables and the output of FUN applied to each unique group
#'@export
#'@examples
#'d <- expand.grid(uid = 1:2000, transects = LETTERS[1:11], zone = 1:2, class = 1:12)
#'d$result <- rpois(nrow(d), .5)
#'f <- function(x, i, ...){
#'  cbind(sm = igroupSums(x, i),
#'        me = igroupMeans(x, i),
#'        mn = igroupMins(x, i),
#'        mx = igroupMaxs(x, i))
#'}
#'system.time(fastSummaries(d$result, d[c('uid', 'class', 'zone')], FUN = f))
fastSummaries <- function(X, INDEX, FUN, drop = T, ...){
  splitv <- id(INDEX, drop = drop)
  split_labels <- split_labels(INDEX, drop = drop, id = splitv)
  ans <- FUN(X, as.integer(splitv), ...)
  cbind(split_labels, result = ans)
}
