#' Identify the first and last elements for each run in a vector
#' 
#' For an input vector which contains a sequence of runs, these functions identify
#' the first and last points in each run.  They return a logical vector.
#' 
#' These functions use \link{rle} to compute a run length encoding for the vector
#' and extract the first and last elements in each run from the lengths of each run.
#' @param x a vector with runs
#' @param a logical vector; \code{TRUE} if it is the first or last of a run
#' @export
#' @examples
#' first(c(1,1,2,2))
first <- function(x, na.as.group = F){
  n <- length(x)
  nas <- is.na(x)
  if(na.as.group && any(nas)){
    y <- ifelse(nas[-1L] | nas[-n], nas[-1L] != nas[-n], x[-1L] != x[-n])
    y <- c(T, y)
  } else {
    y <- x[-1L] != x[-n]
    y <- c(T, y) | nas
  }  
  return(y)
}

#' @export
#' @rdname first
#' @examples
#' last(c(1,1,2,2))
last <- function(x, na.as.group = F){
  n <- length(x)
  nas <- is.na(x)
  if(na.as.group){
    y <- ifelse(nas[-1L] | nas[-n], nas[-1L] != nas[-n], x[-1L] != x[-n])
    y <- c(y, T)
  } else {
    y <- x[-1L] != x[-n]
    y <- c(y, T) | nas
  }  
  return(y)
}

#lagmatrix <- function(x,max.lag){embed(c(rep(NA,max.lag),x),max.lag+1)} 
#' Lag a vector
#' 
#' Returns the lag of a vector.
#' @param x a vector
#' @param k the lag
#' @return a lagged vector
#' @examples
#' lag(1:5)
lag <- function(x, k){
  nr <- NROW(x)
  nas <- rep(NA, abs(k))
  if (k != round(k)) {
    k <- round(k)
    warning("k is not an integer")
  }
  if (k == 0) 
    return(x)
  if (abs(k) > nr) 
    k <- nr
  if (k > 0) {
    xx <- c(x[-seq(1, length = k)], nas)
  }
  else {
    xx <- c(nas, x[-seq(to = nr, length = -k)])
  }
  return(xx)
}
