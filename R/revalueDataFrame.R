revalueDataFrame <- function(x, maps){
  map.names <- names(maps)
  from      <- lapply(maps, names)
  to        <- lapply(maps, as.character)
  for (m in map.names){
    x[[m]] <- mapvalues(x[[m]], from[[m]], to[[m]])
  }
  x
}
