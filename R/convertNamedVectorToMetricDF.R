#' Convert a named vector to a metric data.frame
#' 
#' Converts a named vector to a metric data.frame.  The names should be the site
#' identifiers, and the deparsed name of the the argument \code{x}, becomes the
#' metric name.
#' @param x a named vector
#' @return a 'metric' data.frame
#' @examples
#' my_metric <- c('uid1' = 0.1, 'uid2' = 0.2, 'uid3' = 0.3)
#' convertNamedVectorToMetricDF(my_metric)
#' @export
convertNamedVectorToMetricDF <- function(x){
  data.frame(uid    = names(x), 
             metric = deparse(substitute(x)), 
             result = x, 
             stringsAsFactors = F)
}