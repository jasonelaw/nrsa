#'Removes side channels from a data frame
#'
#'\code{removeSideChannels} removes side channels from a dataframe using the 
#'transect codes
#'@param x a data frame
#'@param transect a vector of transect codes of the same length as rows in \code{x}
#'@export
removeSideChannels <- function(x, transect){
  side.channels <- paste0('X', LETTERS[1:11])
  subset(x, !(transect %in% side.channels))
}
