#'Calculates Substrate Embeddedness metrics
#'\code{calculateSubstrateEmbed} calculates substrate embeddedness metrics:
#'N33, N55, VCEMBED, VEMBED, XCEMBED, XEMBED.
#'@param uid a vector of site-visit indicators
#'@param embed numeric vector of embeddedness values (0 - 100).
#'@param is.center a logical vector describing whether the embeddedness
#'is from a center point
#'@export
#'@import plyr
calculateSubstrateEmbed <- function(uid, embed, is.center){
  if (is.character(embed) || is.factor(embed)){
    embed <- as.numeric(as.character(embed))
  }
  x <- data.frame(uid = uid, is.center = is.center, embed = embed)
  met1 <- ddply(x, .(uid), summarize, 
                n55    = nrsa:::count.notna(embed), 
                xembed = mean(embed, na.rm = T), 
                vembed = sd(embed, na.rm = T))
  cx <- subset(x, x$is.center)
  met2 <- ddply(cx, .(uid), summarize, 
                n33     = nrsa:::count.notna(embed), 
                xcembed = mean(embed, na.rm = T), 
                vcembed = sd(embed, na.rm = T))
  met <- merge(met1, met2, by = 'uid', all = T)
  progressReport('Finished calculating substrate embeddedness metrics.')
  return(met)
}
