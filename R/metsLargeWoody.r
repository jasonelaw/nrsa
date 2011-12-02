# metsLargeWoody.r
#
# Calculates Large Woody Debris portion of the Physical Habitat metrics from
# validated NRSA data.
#
# 12/31/2009 mrc started
# 01/07/2009 mrc finished writing the metric for wadeable/boatable... start tests
# 01/21/2010 mrc copied needed test files to \code\testfiles
# 03/22/2010 rch replaced upData with rename.  also modified some aggregate
#            functions to rename the first value.
# 04/06/2010 ssr updated test for wadeable and boatable  
# 06/04/2010 cws Using reachlen calculated in metsGeneral instead of value
#            reported by field crew.  Of the 2308 sites, there were 635 sites
#            with differences in the value of c1dm100, counts of (old-new)/old
#            broke out this way:
#            (-10,-5]     (-5,-2]     (-2,-1]   (-1,-0.5] (-0.5,-0.2]
#                   5           0           2           5           2
#            (-0.2,-0.1]  (-0.1,0.1]   (0.1,0.2]   (0.2,0.5]     (0.5,1]
#                      5         481           7          48          80
#            (1,2]
#                0

#wrapper function
metsLargeWoody <- function ()
# calculate large woody debris metrics and saves results to a file.
# Wadeable and Boatable protocol look the same:
#  Wadeable metrics: c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t, c2tm100
#, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq, c3wm100, c4d, c4dm100, c4t
#, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100, c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot
#, lgdrydia, lgdrylen, lglentot, lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33
#, lwdwvcal, mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns, rchdldll, rchdldml
#, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt, rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml
#, rchdxdsl, rchtldll, rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml
#, rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml, rchwldsl, rchwmdll
#, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl, rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot
#, shwetlen, smdiatot, smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d
#, v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100, v3w, v3w_msq, v3wm100
#, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100, v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq
#, v5wm100, xldiatot, xldrydia, xlwetdia
#
#  Boatable metrics:c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t, c2tm100
#, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq, c3wm100, c4d, c4dm100, c4t
#, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100, c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot
#, lgdrydia, lgdrylen, lglentot, lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33
#, lwdwvcal, mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns, rchdldll, rchdldml
#, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt, rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml
#, rchdxdsl, rchtldll, rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml
#, rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml, rchwldsl, rchwmdll
#, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl, rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot
#, shwetlen, smdiatot, smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d
#, v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100, v3w, v3w_msq, v3wm100
#, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100, v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq
#, v5wm100, xldiatot, xldrydia, xlwetdia

# Formula for volume wrong in Kaufmann.  Should be:
# Volume = pi * [0.5*(minDiam + (maxDiam - minDiam)/3)]2 * [minLength + (maxLength - minLength)/3]

# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Large Woody debris calculations', loc='start')

  #read in the data from tblWOOD2
  chan <- odbcConnect ('NRSA2')
  df1 <- fetchNRSATable (chan, 'tblWOOD2')
  intermediateMessage ('fetch_data.1', loc='start')
  
  #read in data from other required tables. Reachlen (thalweg), xbk_wid (bankgeometry2)
  df2<- fetchReachlen()
  df3 <- fetchNRSATable (chan, 'tblBANKGEOMETRY2', where = "PARAMETER = 'BANKWID'")
  intermediateMessage ('fetch_data.2', loc='start') 
   
  #determine protocol used for each site
  protocols <- siteProtocol (unique(df1$UID))
  intermediateMessage ('set_protocols.3', loc='start')

  #calculate the metrics
  mets <- metsLgWoody.1(df1, df2, df3, protocols)
  if(is.character(mets)) return (mets)
  #write the results
  rc <- writeNRSACalcResults(mets, 'metsLargeWoody.csv')
  intermediateMessage( ' Done.', loc='end')
  
  return (rc)
}

fetchReachlen <- function(data = NULL, chan, from.database = F){
  # If data is passed it must either be a table like tblThalweg2 or metsGeneral.csv;
  # If from.database is T it should fetch the raw data from the database (or use
  # the database table passed in the data parameter), rather than the calculation results.
  if (is.null(data)){
    if(from.database){
      #stop('Calculate option not implemented')
      data <- fetchNRSATable (chan, 'tblTHALWEG2', where = "PARAMETER = 'REACHLENGTH'")
    } else {
      data <- readNRSACalculationResults('metsGeneral.csv')
    }
  }
  if (from.database){
    reachlen <- subset(data, select = c('UID', 'PARAMETER', 'RESULT'))
    names(reachlen) <- c('UID', 'METRIC', 'RESULT')
    reachlen$METRIC <- 'reachlen'
    reachlen <- unique(reachlen)
    return(reachlen)
  } else {
    reachlen <- subset(data, METRIC == 'reachlen')
    reachlen$RESULT <- as.numeric(reachlen$RESULT)
    return(reachlen)
  }
}

GetNumTransects <- function (transect, uid) {
  ans <- ddply(data.frame(transect = transect, UID = uid), .(UID),
               function(x) c(RESULT = length(unique(transect))))
  ans$METRIC <- 'numtran'
  ans
}

CalcBankGeomean <- function(bankgeo){
  stopifnot(identical(unique(bankgeo$PARAMETER), 'BANKWID'))
  bankgeo$RESULT <- as.numeric(bankgeo$RESULT)
  bankgeomean <- ddply(bankgeo, .(UID), function(x) c(RESULT = mean(x$RESULT, na.rm = T)))
  bankgeomean$METRIC <- 'xbkf_w'
  bankgeomean
}

WoodClassesData <- function(){
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
                          .Label = c("[0.1,0.3]", "(0.3,0.6]", "(0.6,0.8]", "(0.8,2]", "[0.3,0.6]", "(0.8,1]", "(1,2]"), class = "factor"), 
     length = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 5L, 5L, 5L, 5L), 
                        .Label = c("[1.5,5]", "(5,15]","(15,30]", "[5,15]", "(30,75]"), class = "factor"), 
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
     PROTOCOL = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          .Label = c("BOATABLE", "WADEABLE"), class = "factor")), 
    .Names = c("diameter", "length", "code", "d.lower", "d.upper", "l.lower", "l.upper", "volume", "size", "diameter.name", "length.name", "PROTOCOL"), 
    row.names = c(NA, 24L), class = "data.frame")
  return(classes)
}
 
#sample_type PHAB_CHANFRONTB ......boatable?
#sample_type THAL...........wadeable?

metsLgWoody.1 <- function (df1, df2, df3, protocols)
#Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.
#
# ARGUMENTS:
# df1  dataframe of the lgwoody data.
# df2 dataframe with reachlen values.
# df3 data
# protocols   dataframe relating UID to the sampling protocol used at that site
 
 {
 #most of the mets are the same for each protocol, but there are a few differences.
 
 #do the calculations
#
# ARGUMENTS:
#   None
# 
# Calculates the LargeWoody metrics
# ASSUMPTIONS:
# 
# for some of the calulations, need variables from other files
# number of transects = numtran
# reachlen = reachlen  (reachlen is in THALWEG2)
# samplen = 20m * numtran
# xbkf_w = mean(BANKWID) calculated in channel morphology metrics, or from tblBANKGEOMETRY2
# nc, ns are the number of transects with non-missing wet or dry responses.  For each transect,
# 12 results are expected for the wet/dry parameters.  If 1 or more is missing, the entire transect
# is scored 0.  The result for each site is 0-numtran for nc/ns.  If there are 12 results for
# the wet/dry at each transect for the entire site nc/ns is 0, if they were all missing, nc/ns=numtran

  intermediateMessage('LgWoody mets', loc='start')

  #reachlen
  reachlen <- df2

  # numtran - several aggregate steps will get us to the number of transects for each UID
  lgWoodTranNo <- GetNumTransects(df1$TRANSECT, df1$UID)
  intermediateMessage('end of lgWoodTranNo', loc='end')
   
  # xbkf_w
  bankgeomean <-  CalcBankGeomean(df3) 
  intermediateMessage('end of bankgeomean', loc='end')

  # append these files with all the necessary columns
  # put all these pieces together, ensuring that all parameters are present in mm
  tt <- rbind(lgWoodTranNo, reachlen, bankgeomean)
  mm <- dcast(melt(tt, measure.var = 'RESULT'), 
              list(.(UID), .(factor(METRIC, levels = c('numtran', 'reachlen', 'xbkf_w')))),
              drop = F)
  
  classes <- WoodClassesData()

  # nc/ns metrics
  df1$RESULT <- as.numeric(df1$RESULT)
  df1$code <- tolower(substr(df1$PARAMETER, 2, 5))
  df1$zone <- as.factor(tolower(substr(df1$PARAMETER, 1, 1)))
  ncns <- ddply(df1, .(UID, zone), function(x){
    all.pars <- c("ldll", "ldml", "ldsl", "mdll", "mdml", "mdsl", "sdll", "sdml", 
                  "sdsl", "xdll", "xdml", "xdsl")
    transect.observed <- tapply(x$code, x$TRANSECT, 
                                function(x){ all(all.pars %in% x) })
    return(c(RESULT = sum(transect.observed)))
  })
  ncns$METRIC <- ifelse(ncns$zone == 'd', 'ns', 'nc')
  ncns$zone <- NULL
  intermediateMessage('End of ncns', loc='end')
  
  #calculate sums of counts, that serve as the basis for nearly all other metrics
  # these sums need to be across TRANSECTS

  ##  04MAY10 SSR adding code to make RESULT numeric
  class.names <- unique(subset(classes, select = c('code', 'diameter.name', 'length.name')))
  df1 <- merge(df1, class.names, by = 'code')
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
  mar.counts <- tapply(df1$RESULT, 
                       df1[c('UID', 'zone', 'length.name', 'diameter.name')], 
                       sum, na.rm = T)
  mar.counts <- addmargins(mar.counts, 2:4)

  mar.counts <- melt(mar.counts, c('UID', 'zone', 'length.name', 'diameter.name'), 
                     value.name = 'RESULT')
  fix <- list(t = 'Sum')
  levs <-
    list(SMDRYDIA = "RCHDSDTL", SMWETDIA = "RCHWSDTL", SMDIATOT = "RCHTSDTL", 
         MDDRYDIA = "RCHDMDTL", MDWETDIA = "RCHWMDTL", MDDIATOT = "RCHTMDTL", 
         LGDRYDIA = "RCHDLDTL", LGWETDIA = "RCHWLDTL", LGDIATOT = "RCHTLDTL", 
         XLDRYDIA = "RCHDXDTL", XLWETDIA = "RCHWXDTL", XLDIATOT = "RCHTXDTL", 
         SHDRYLEN = "RCHDTDSL", SHWETLEN = "RCHWTDSL", SHLENTOT = "RCHTTDSL", 
         MDDRYLEN = "RCHDTDML", MDWETLEN = "RCHWTDML", MDLENTOT = "RCHTTDML", 
         LGDRYLEN = "RCHDTDLL", LGWETLEN = "RCHWTDLL", LGLENTOT = "RCHTTDLL", 
         RCHDRYT = "RCHDTDTL", RCHWETT = "RCHWTDTL", RCHWDT = "RCHTTDTL")
  mar.counts <- within(mar.counts, {
    length.name   <- replace.levels(length.name, fix)
    diameter.name <- replace.levels(diameter.name, fix)
    zone          <- replace.levels(zone, fix)
    METRIC <- toupper(paste('RCH', zone,  diameter.name, 'd', length.name, 'l', 
                            sep = ''))
    METRIC <- replace.levels(as.factor(METRIC), levs)
    })

  intermediateMessage('end of lgwoodsums', loc='end')
  
  category.totals <- subset(mar.counts, !(length.name == 't' | diameter.name == 't'))
  # add in the protocols, so we can subset and do the calculations for each
  mm <- merge(mm, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  category.totals <- merge(category.totals, mm, by = 'UID')
  category.totals <- merge(category.totals, classes, 
                           by = c('diameter.name', 'length.name', 'PROTOCOL'))

  calc <- function(x){
    # Calculate total counts and volumes for each code T, S, M, L, X
    site.data <- allFacToChar(unique(x[, c('xbkf_w', 'reachlen', 'numtran', 'PROTOCOL', 'zone')]))
    size.sum <- tapply(x$RESULT, x$size, sum)
    size.vol <- tapply(x$RESULT * x$volume, x$size, sum)
    # Calculate cumulative sums for each code starting with X to T.
    ans <- c(cumsum(rev(size.sum)),
             cumsum(rev(size.vol)))
    names(ans) <- c(paste('c', 5:1, '%s', sep = ''), paste('v', 5:1, '%s', sep = ''))
    m100 <- switch(site.data$PROTOCOL, 
                   WADEABLE = ans / site.data$reachlen * 100,
                   BOATABLE = ans / (site.data$numtran * 20) * 100)
    msq <- switch(site.data$PROTOCOL,
                  WADEABLE = ans / (site.data$xbkf_w * site.data$reachlen),
                  BOATABLE = ans / (site.data$numtran * 20 * 10))
    names(m100) <- paste(names(m100), 'm100', sep = '')
    names(msq) <- paste(names(msq), 'msq', sep = '_')
    return(c(ans, m100, msq))
  }
  #debug(calc)
  class.metrics <- ddply(category.totals, .(UID, zone), calc)
  class.metrics <- melt(class.metrics, id.var = c('UID', 'zone'), 
                        variable.name = 'METRIC', value.name = 'RESULT')
  class.metrics$METRIC <- sprintf(as.character(class.metrics$METRIC), 
                                  as.character(class.metrics$zone))
  class.metrics$zone <- NULL
  class.metrics <- subset(class.metrics, !grepl('d_msq|t_msq', as.character(METRIC)))
  
  mar.counts <- subset(mar.counts, select = c('UID', 'METRIC', 'RESULT'))
  mets <- rbind(mar.counts, class.metrics, ncns)
  mets <- allFacToChar(mets)
  mets$METRIC <- tolower(mets$METRIC)
  return(mets)
}

#END OF metsLargeWoody.1


