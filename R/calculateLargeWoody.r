#'Number of observed transects
#'
#'\code{getNumTransects} counts the number of observed transects per site
#'@param uid a vector of site-visit indicators
#'@param transect a vector of transect ids
#'@importFrom plyr ddply
#'@export
getNumTransects <- function (uid, transect) {
  ans <- ddply(data.frame(transect = transect, uid = uid), .(uid),
               function(x) c(numtran = length(unique(transect))))
  progressReport("Finished calculating number of transects.")
  ans
}

#'Calculate the mean of the bankfull width
#'
#'\code{calculateBankfullWidthGeomean} returns the mean of the bankfull width at each site
#'@param x a vector of bankfull width measurements
#'@param uid a vector of site-visit indicators
#'@importFrom Rigroup igroupMeans
calculateBankfullWidthGeomean <- function(x, uid){
  if (!is.factor(uid)){
    uid <- as.factor(uid)
  }
  result <- igroupMeans(x, as.numeric(uid), na.rm = T)
  progressReport("Finished calculating mean bankfull width at each site.")
  return(data.frame(uid = as.numeric(levels(uid)), xbkf_w = result))
}

#'LWD class metadata
#'
#'A data frame of LWD size class metadata that is in the LWD metric calculations.
#'@return A data frame with the metadata 
WoodClassesData <- function(){
  # Formula for volume wrong in Kaufmann.  Should be:
  # Volume = pi * [0.5*(minDiam + (maxDiam - minDiam)/3)]/2 * [minLength + (maxLength - minLength)/3]
  lwdv <- function(mind, maxd, minl, maxl){
    # Not used here, but used to calculate volumes in dataframe below
    dadj <- (maxd - mind)/3
    ladj <- (maxl - minl)/3
    r <- (mind + dadj) / 2
    l <- minl + ladj
    pi * r^2 * l
  }
  classes <- structure(list(
     diameter = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 3L, 6L, 7L, 5L, 3L, 6L, 7L, 5L, 3L, 6L, 7L), 
                          .Label = c("[0.1,0.3]", "(0.3,0.6]", "(0.6,0.8]", "(0.8,Inf)", "[0.3,0.6]", "(0.8,1]", "(1,Inf)"), class = "factor"), 
     length = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 5L, 5L, 5L, 5L), 
                        .Label = c("[1.5,5]", "(5,15]","(15,Inf)", "[5,15]", "(30,Inf)", "(15,30]"), class = "factor"), 
     code = structure(c(9L, 6L, 3L, 12L, 8L, 5L, 2L, 11L, 7L, 4L, 1L, 10L, 9L, 6L, 3L, 12L, 8L, 5L, 2L, 11L, 7L, 4L, 1L, 10L), 
                      .Label = c("ldll", "ldml", "ldsl", "mdll", "mdml", "mdsl", "sdll", "sdml", "sdsl", "xdll", "xdml", "xdsl"), class = "factor"), 
     d.lower = c(0.1, 0.3, 0.6, 0.8, 0.1, 0.3, 0.6, 0.8, 0.1, 0.3, 0.6, 0.8, 0.3, 0.6, 0.8, 1, 0.3, 0.6, 0.8, 1, 0.3, 0.6, 0.8, 1), 
     d.upper = c(0.3, 0.6, 0.8, 2, 0.3, 0.6, 0.8, 2, 0.3, 0.6, 0.8, 2, 0.6, 0.8, 1, 2, 0.6, 0.8, 1, 2, 0.6, 0.8, 1, 2), 
     l.lower = c(1.5, 1.5, 1.5, 1.5, 5, 5, 5, 5, 15, 15, 15, 15, 5, 5, 5, 5, 15, 15, 15, 15, 30, 30, 30, 30), 
     l.upper = c(5, 5, 5, 5, 15, 15, 15, 15, 30, 30, 30, 30, 15, 15, 15, 15, 30, 30, 30, 30, 75, 75, 75, 75), 
     volume = c(0.0581776417331443, 0.335103216382911, 0.930842267730309, 3.0159289474462, 0.181805130416076, 1.0471975511966, 2.90888208665722, 9.42477796076938, 0.436332312998582, 2.51327412287183, 6.98131700797732, 22.6194671058465, 1.0471975511966, 2.90888208665722, 4.9160107264507, 11.6355283466289, 2.51327412287183, 6.98131700797732, 11.7984257434817, 27.9252680319093, 5.65486677646163, 15.707963267949, 26.5464579228338, 62.8318530717959), 
     size = structure(c(1L, 2L, 2L, 3L, 2L, 3L, 4L, 4L, 3L, 4L, 4L, 5L, 1L, 2L, 2L, 3L, 2L, 3L, 4L, 4L, 3L, 4L, 4L, 5L), 
                      .Label = c("T", "S", "M", "L", "X"), class = "factor"), 
     diameter.name = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), .Label = c("s", "m", "l", "x"), class = "factor"), 
    length.name = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), 
                            .Label = c("s", "m", "l"), class = "factor"), 
     protocol = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          .Label = c("BOATABLE", "WADEABLE"), class = "factor")), 
    .Names = c("diameter", "length", "code", "d.lower", "d.upper", "l.lower", "l.upper", "volume", "size", "diameter.name", "length.name", "protocol"), 
    row.names = c(NA, 24L), class = "data.frame")
  return(classes)
}

#'Calculate number of LWD observed transects
#'
#'\code{observedLWDTransects} is used to calculate the number of transects that
#'were observed for the LWD data.
#'
#'@param count vector of counts of LWD pieces
#'@param in.bankfull logical vector; true if the lwd was within bankfull
#'@param uid vector of site-visit indicators
#'@param transect vector of transect names
#'@importFrom plyr ddply rename revalue
#'@importFrom reshape2 dcast melt
#'@export
observedLWDTransects <- function(uid, transect, count, in.bankfull){
  if(nrsa.options()$ProgressReports){
    on.exit(message(Sys.time(), ': Finished calculating number of transects observed.'))   
  }
  x <- data.frame(uid = uid, transect = transect, count = count, in.bankfull = in.bankfull)
  numObserved <- function(x){
    is.observed <- vapply(split(x$count, x$transect), function(x) identical(length(x), 12L) & all(!is.na(x)), logical(1))
    return(c(result = sum(is.observed, na.rm = T))) 
  }
  observed.transect        <- ddply(x, .(uid, in.bankfull), numObserved)
  observed.transect        <- rename(observed.transect, c("in.bankfull" = "metric"))
  observed.transect$metric <- revalue(as.character(observed.transect$metric), c('FALSE' = 'ns', 'TRUE' = 'nc'))
  observed.transect <- dcast(melt(observed.transect, measure.var = 'result'), uid ~ metric)
  return(observed.transect)
}

#'Calculate LWD counts within each size class
#'
#'This function calculates LWD counts within each site, size class, and whether
#'it is within the bankfull area
#'@param uid vector of site-visit indicators
#'@param in.bankfull logical vector; true if the lwd was within bankfull
#'@param length the length category of the lwd piece
#'@param diameter the diamter cateogry of the lwd piece 
#'@param count vector of counts of LWD pieces
#'@importFrom reshape2 melt
#'@export
calculateLWDCounts <- function(uid, in.bankfull, length, diameter, count){
  x <- data.frame(count = count, uid = uid, in.bankfull = in.bankfull, 
                  length = length, diameter = diameter)
  x <- merge(x, WoodClassesData(), by = c('diameter', 'length'), all.x = T)
  # Another option is:
  # group <- interaction(df1b$UID, df1b$zone, df1b$length, df1b, diameter)
  # rowsum(df1b$RESULT, df1b$group, na.rm = T)
  # Another option is: 
  # counts <- xtabs(RESULT ~ UID + zone + length + diameter, df1b, na.action = na.omit)
  # mar.counts <- as.data.frame(addmargins(counts, 2:4))
  # df1bm <- melt(df1b, measure.var = c('RESULT'))
  # mar.counts <- acast(df1bm, UID ~ zone ~ length.name ~ diameter.name ~ variable,
  #                     fun.aggregate = sum, na.rm = T,
  #                     margins = c('zone', 'length.name', 'diameter.name'))
  mar.counts <- tapply(x$count,
                       x[c('uid', 'in.bankfull', 'length.name', 'diameter.name')], 
                       sum, na.rm = T)
  mar.counts <- addmargins(mar.counts, 2:4)
  
  mar.counts <- melt(mar.counts, c('uid', 'in.bankfull', 'length.name', 'diameter.name'), 
                     value.name = 'result')
  fix <- list(t = 'Sum')
  levs <-
    list(smdrydia = "rchdsdtl", smwetdia = "rchwsdtl", 
         smdiatot = "rchtsdtl", mddrydia = "rchdmdtl", mdwetdia = "rchwmdtl", 
         mddiatot = "rchtmdtl", lgdrydia = "rchdldtl", lgwetdia = "rchwldtl", 
         lgdiatot = "rchtldtl", xldrydia = "rchdxdtl", xlwetdia = "rchwxdtl", 
         xldiatot = "rchtxdtl", shdrylen = "rchdtdsl", shwetlen = "rchwtdsl", 
         shlentot = "rchttdsl", mddrylen = "rchdtdml", mdwetlen = "rchwtdml", 
         mdlentot = "rchttdml", lgdrylen = "rchdtdll", lgwetlen = "rchwtdll", 
         lglentot = "rchttdll", rchdryt = "rchdtdtl", rchwett = "rchwtdtl", 
         rchwdt = "rchttdtl") 
  mar.counts <- within(mar.counts, {
    length.name      <- replace.levels(length.name, fix)
    diameter.name    <- replace.levels(diameter.name, fix)
    in.bankfull <- replace.levels(in.bankfull, list(t = 'Sum', w = 'TRUE', d = 'FALSE'))
    metric <- tolower(paste('rch', in.bankfull, diameter.name, 'd', length.name, 'l', sep = ''))
    metric <- replace.levels(as.factor(metric), levs)
  })
  progressReport("Finished calculating counts of lwd in each size class.")
  mar.counts
}

#'Calculate length of area sampled for LWD
#'
#'\code{calculateLWDSiteLength} is used to calculate the area of the sampled
#'lwd plots so that counts/area can be calculated.
#'
#'@param lwdlength a vector of site lengths as returned by calculateLWDSiteLength
#'@param xbkf_w a vector of average bankfull width
#'@param protocol a logical vector; \code{TRUE} if the site was wadeable
#'@export
calculateLWDSiteArea <- function(lwdlength, xbkf_w, is.wadeable){
  # Calculates the area in which LWD data is collected
  ifelse(is.wadeable,
         xbkf_w * lwdlength,
         lwdlength * 10)
}

#'Calculate length of area sampled for LWD
#'
#'\code{calculateLWDSiteLength} is used to calculate the length of the sampled
#'area so that counts/length and counts/area can be calculated.
#'
#'@param reachlen a vector of reachlengths for the sites
#'@param numtran a vector with the number of transects sampled
#'@param protocol a logical vector; \code{TRUE} if the site was wadeable
#'@export
calculateLWDSiteLength <- function(reachlen, numtran, is.wadeable){
  # Calculates the length of stream reach over which LWD data is collected
  ifelse(is.wadeable,
         reachlen,
         numtran * 20)
}

#'Calculate LWD volume metrics
#'
#'\code{calculateLWDVolumeMetrics} is used to calculate the volume metrics from the
#'lwd counts at a site.
#'@param uid a vector of site-visit indicators
#'@param zone a vector of 'd', 'w', and 't' values that describe whether the count was
#'observed within the bankfull ('w'), outside it ('d') or it's a total count ('t').
#'@param lwdlength the total length of the sampled lwd plots per site
#'@param lwdarea the total area of the sampled lwd plots per site
#'@param count counts of lwd pieces
#'@param size the size class of the LWD pieces
#'@param volume the calculated volume of the LWD size class
#'@importFrom plyr ddply
#'@importFrom reshape2 melt
#'@export
calculateLWDVolumeMetrics <- function(uid, zone, lwdlength, lwdarea, count, size, volume){
  x <- data.frame(uid = uid, zone = zone, count = count, size = size, volume = volume,
                  lwdlength = lwdlength, lwdarea = lwdarea)
  calcVols <- function(x){
    # Calculate total counts and volumes for each code T, S, M, L, X
    lwdlength <- unique(x$lwdlength)
    lwdarea <- unique(x$lwdarea)
    size.sum <- tapply(x$count, x$size, sum)
    size.vol <- tapply(x$count * x$volume, x$size, sum)
    # Calculate cumulative sums for each code starting with X to T.
    ans <- c(cumsum(rev(size.sum)),
             cumsum(rev(size.vol)))
    names(ans) <- c(paste('c', 5:1, '%s', sep = ''), paste('v', 5:1, '%s', sep = ''))
    m100 <- ans / lwdlength * 100
    msq  <- ans / lwdarea
    names(m100) <- paste(names(m100), 'm100', sep = '')
    names(msq) <- paste(names(msq), 'msq', sep = '_')
    return(c(ans, m100, msq))
  }
  class.metrics <- ddply(x, .(uid, zone), calcVols)
  class.metrics <- melt(class.metrics, id.var = c('uid', 'zone'), 
                        variable.name = 'metric', value.name = 'result')
  class.metrics$metric <- sprintf(as.character(class.metrics$metric), 
                                  as.character(class.metrics$zone))
  class.metrics$zone <- NULL
  class.metrics <- subset(class.metrics, !grepl('d_msq|t_msq', as.character(metric)))
  progressReport("Finished calculating volumes and counts per site, 100 m, and m^2")
  class.metrics
}

#'@rdname calculateLWDVolumeMetrics
#'@importFrom plyr ldply arrange
#'@importFrom reshape2 melt acast
#'@export
calculateLWDVolumeMetrics2 <- function(uid, zone, lwdlength, lwdarea, count, size, volume){
  x <- data.frame(uid = uid, zone = zone, count = count, size = size, volume = volume,
                  lwdlength = lwdlength, lwdarea = lwdarea)
  x$volume <- x$volume * x$count
  xm <- melt(x, id.var = c('uid', 'zone', 'size'), measure.var = c('count', 'volume'))
  xc <- acast(xm, size ~ uid ~ zone ~ variable, fun.aggregate = sum)
  xc1 <- apply(xc, 2:4, function(x) cumsum(rev(x)))
  xc2 <- sweep(xc1, 
               2, tapply(x$lwdlength, x$uid, unique), '/') * 100
  xc3 <- sweep(xc1[,,3,, drop = F], 
               2, tapply(x$lwdarea, x$uid, unique), '/')
  ans <- ldply(list(xc1, 'm100' = xc2,'_msq' = xc3), 
               function(x) melt(x, varnames = c('size', 'uid', 'zone', 'variable'), value.name = 'result'))
  # Get the metric names right
  levels(ans$size) <- list('5' = 'X', '4' = 'L', '3' = 'M', '2' = 'S', '1' = 'T')
  ans$metric <- paste0(substr(ans$variable, 1, 1), ans$size, ans$zone, ans$.id)
  progressReport("Finished calculating volumes and counts per site, 100 m, and m^2")
  arrange(ans[, c('uid', 'metric', 'result')], uid, metric)
}
