convertNamedVectorToMetricDF <- function(x){
  data.frame(uid    = names(x), 
             metric = deparse(substitute(x)), 
             result = x, 
             stringsAsFactors = F)
}