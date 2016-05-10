#'Add substrate size variables to wadeable data
#'
#'The substrate size metrics are calculated on variables representing four
#'different sets of the size class data. This function convert the size classes
#'to a diameter and logDiameter for each of those size classes (geometric mean
#'of the extreme sizes) for each of these sets. There are three data sets we
#'need to work with (1) ALL the size_classes (mm) (2) subclasses excluding HP,
#'RD, RR, RS, RC, OT, WD (tt) (3) subclasses including all classes and lumps the
#'boulder class (XB+SB= BL)  (bl) (4) subclasses excluding HP, RD, RR, RS, RC,
#'OT, WD, and lumps the boulder class (XB+SB= BL)  (ttbl)
#'
#'@param uid a vector of site-visit indicators
#'@param size.class a vector size class codes
#'@import plyr
#'@export
#'@examples
#'d <- expand.grid(uid = 1:2, 
#'size.class = c("OM", "OT", "WD", "HP", "FN", "SA", "GF", "GC","CB", "SB", "XB", "RS", "RR", "RC"))
#'addWadeSubstrateSizes(d$uid, d$size.class)
#'d <- expand.grid(uid = 1:2,
#'size.class =c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT'))
#'addBoatSubstrateSizes(d$uid, d$size.class)
addWadeSubstrateSizes <- function(uid, size.class){
  kWadeSubstrateSizes <- c("OM", "OT", "WD", "HP", "FN", "SA", "GF", "GC","CB", "SB", 
                           "XB", "BL", "RS", "RR", "RC")
  kResultSizes        <- kWadeSubstrateSizes[kWadeSubstrateSizes != 'BL']
  kCollapseBoulder    <- c(XB = 'BL', SB = 'BL')
  kTTDropClasses      <- setNames(rep(NA, 7), c("OM", "OT", "WD", "HP",  "RS", "RR", "RC"))
  
  sizes <-
    data.frame('class' = factor(kWadeSubstrateSizes, kWadeSubstrateSizes),
               'min'   = c(NA, NA, NA, NA, 0.001, 0.06, 2, 16, 64, 250, 1000,
                           250, 4000, 4000, 4000),
               'max'   = c(NA, NA, NA, NA, 0.06, 2, 16, 64, 250, 1000, 4000,
                           4000, 8000, 8000, 8000))
  sizes$diam <- apply(sizes[,2:3], 1, gmean)
  sizes$lDiam <- log10(sizes$diam)
  
  x <- data.frame(uid = uid, 
                  result = factor(as.character(size.class), kResultSizes))
  x$result.bl <- revalue(x$result, kCollapseBoulder)
  x$result.tt <- revalue(x$result, kTTDropClasses)
  x$result.ttbl <- revalue(x$result.tt, kCollapseBoulder)
  i <- lapply(x[c('result', 'result.bl', 'result.tt', 'result.ttbl')], match, table = sizes$class)
  x$min        <- sizes$min[i$result]
  x$max        <- sizes$max[i$result]
  x$lDiam      <- sizes$lDiam[i$result]
  x$lDiam.tt   <- sizes$lDiam[i$result.tt]
  x$diam.tt    <- sizes$diam[i$result.tt]
  x$lDiam.bl   <- sizes$lDiam[i$result.bl]
  x$lDiam.ttbl <- sizes$lDiam[i$result.ttbl]
  x$diam.ttbl  <- sizes$diam[i$result.ttbl]
  x$result.bl <- x$result.ttbl <- NULL
  progressReport('Created numeric size classes for wadeable')
  x
}

#'@rdname addWadeSubstrateSizes
#'@import plyr
#'@export
addBoatSubstrateSizes <- function(uid, size.class){
  kBoatSubstrateClasses <- c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT')
  kMetrics              <- paste0('pct_', tolower(kBoatSubstrateClasses))
  x <- data.frame(uid = uid, result = size.class)
  # SIZE_CLS from the rivers have slightly different gmeans.
  sizes <- data.frame(class = factor(kBoatSubstrateClasses, kBoatSubstrateClasses),
                      min = c(4000, 250, 64, 2, 0.06, 0.001, NA),
                      max = c(8000, 4000, 250, 64, 2, 0.06, NA))
  sizes$diam <- apply(sizes[,2:3], 1, gmean)
  sizes$lDiam <- log10(sizes$diam)
  ans <- merge(x, sizes, by.x = 'result', by.y = 'class', all.x = TRUE)
  ans$result.no.ot <- mapvalues(ans$result, 'OT', NA)
  ans$result       <- mapvalues(ans$result, kBoatSubstrateClasses, kMetrics)
  progressReport('Created numeric size classes for non-wadeable')
  return(ans)
}

#'Calculate wadeable substrate metrics
#'
#'\code{calculateWadeSubstrateMetrics} is used to calculate wadeable substrate metrics: 
#'d16,d50,d84,dgm,lsub2d16,lsub2d16inor,lsub2d25,lsub2d50,lsub2d50inor,lsub2d75,
#'lsub2d84,lsub2d84inor,lsub2dmm,lsub2dmm_nor,lsub2iqr,lsubd2sd,lsubd2sd_nor, 
#'lsubd_sd,lsubd_sd_nor,lsub_d16,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm, 
#'lsub_dmm_nor,lsub_iqr,n,n_nor,pct_bdrk,pct_bigr,pct_bl,pct_cb,pct_fn,pct_gc, 
#'pct_gf,pct_hp,pct_org,pct_om,pct_ot,pct_rc,pct_rr,pct_rs,pct_sa,pct_safn,pct_sb,
#'pct_sfgf,pct_wd,pct_xb,sub2dmm_nor,subd2sd_nor,subd_sd_nor,sub_dmm_nor
#'
#'@param uid a vector of site-visit indicators
#'@param size.class a vector size classes
#'
#'@import plyr
#'@export
calculateWadeSubstrateMetrics <- function(uid, size.class){
  x <- addWadeSubstrateSizes(uid, size.class)
  #for each of the mm, tt, bl, ttbl groups, we want summaries (lDiam) for all classes with numeric values
  calcAggrMets <- function(x, breaks){ 
    # This calculates metrics requiring some sort of aggregation of raw data
    lDiam.mm   <- na.omit(x$lDiam)
    lDiam.tt   <- na.omit(x$lDiam.tt)
    diam.tt    <- na.omit(x$diam.tt)
    lDiam.bl   <- na.omit(x$lDiam.bl)
    lDiam.ttbl <- na.omit(x$lDiam.ttbl)
    diam.ttbl  <- na.omit(x$diam.ttbl)
    result.tt <- na.omit(x$result.tt)
    ans <- 
      c(# mm calcs
        summary.nrsa(lDiam.mm),
        # summaries for the tt dataset (NOR) (ldiam AND diam)
        mean(lDiam.tt), 10^mean(lDiam.tt), sd(lDiam.tt), mean(diam.tt), 
        sd(diam.tt),
        # bl calcs: all the same metrics for the subset with the lumped boulder classes
        summary.nrsa(lDiam.bl),
        # ttbl calcs: special few extra summaries that use the lumped boulder class for the NOR
        mean(lDiam.ttbl), sd(lDiam.ttbl), mean(diam.ttbl), sd(diam.ttbl))
    names(ans) <- 
      c('lsub2d16', 'lsub2d25', 'lsub2d50', 'lsub2d75', 'lsub2d84', 'lsub2dmm',
        'lsubd2sd', 'lsub2iqr', 'lsub2dmm_nor', 'dgm', 'lsubd2sd_nor', 
        'sub2dmm_nor', 'subd2sd_nor', 'lsub_d16', 'lsub_d25', 'lsub_d50', 
        'lsub_d75', 'lsub_d84', 'lsub_dmm', 'lsubd_sd', 'lsub_iqr', 
        'lsub_dmm_nor', 'lsubd_sd_nor', 'sub_dmm_nor', 'subd_sd_nor')
    # substrate category proportions
    tbl <- table(x$result)
    n     <- sum(tbl[c('RS', 'RR', 'RC', 'HP', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA', 'FN')])
    n_nor <- sum(tbl[c('XB', 'SB', 'CB', 'GC', 'GF','SA', 'FN')])
    proportions <- prop.table(tbl) * 100
    names(proportions) <- paste('pct_', tolower(names(proportions)), sep = '')
    # substrate size percentiles
    percentiles <- interpolatePercentile2(result.tt, breaks, c(0.16, 0.50, 0.84))
    names(percentiles) <-  c('lsub2d16inor','lsub2d50inor', 'lsub2d84inor')
    return(c(ans, n = n, n_nor = n_nor, proportions, percentiles, recursive = T))
  }

  calcCompositeMets <- function(x){ 
    # These are metrics that are straightforward functions of other metrics.
    x$d16 <- 10^x$lsub2d16inor
    x$d50 <- 10^x$lsub2d50inor
    x$d84 <- 10^x$lsub2d84inor
    x$pct_bl   <- x$pct_xb + x$pct_sb
    x$pct_bigr <- x$pct_rr + x$pct_rs + x$pct_rc + x$pct_bl + x$pct_cb + x$pct_gc
    x$pct_bdrk <- x$pct_rr + x$pct_rs
    x$pct_safn <- x$pct_sa + x$pct_fn
    x$pct_sfgf <- x$pct_sa + x$pct_fn + x$pct_gf
    x$pct_org  <- x$pct_om + x$pct_wd
    x
  }
  breaks.df <- na.omit(unique(x[,c('result.tt', 'min', 'max')]))
  breaks <- log10(sort(unique(c(breaks.df$min, breaks.df$max))))
  metrics <- ddply(x, .(uid), calcAggrMets, .drop = FALSE, breaks = breaks,
                   .progress = plyrProgress())
  metrics <- calcCompositeMets(metrics)
  metrics <- meltMetrics(metrics)
  progressReport('Finished wadeable substrate metrics.')
  return(metrics)
}

#'Calculate non-wadeable substrate metrics
#'
#'\code{calculateBoatSubstrateMetrics} is used to calculate non-wadeable substrate metrics
#'d16,d50,d84,dgm,LDCBF_G08,lsub2d16inor,lsub2d50inor,lsub2d84inor,lsubd_sd,lsub_d16,
#' ,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,lsub_iqr,n,pct_bh,pct_bl,pct_cb,
#' ,pct_dbbl,pct_dbcb,pct_dbfn,pct_dbgc,pct_dbgf,pct_dbhp,pct_dbom,pct_dbot,pct_dbrc,
#' ,pct_dbrr,pct_dbrs,pct_dbsa,pct_dbsb,pct_dbwd,pct_dbxb,pct_dsbl,pct_dscb,pct_dsfn,
#' ,pct_dsgc,pct_dsgf,pct_dshp,pct_dsom,pct_dsot,pct_dsrc,pct_dsrr,pct_dsrs,pct_dssa,
#' ,pct_dssb,pct_dswd,pct_dsxb,pct_fn,pct_gr,pct_ot,pct_sa,pct_safn,pct_sbbl,pct_sbcb,
#' ,pct_sbfn,pct_sbgc,pct_sbgf,pct_sbhp,pct_sbom,pct_sbot,pct_sbrc,pct_sbrr,pct_sbrs,
#' ,pct_sbsa,pct_sbsb,pct_sbwd,pct_sbxb,pct_ssbl,pct_sscb,pct_ssfn,pct_ssgc,pct_ssgf,
#' ,pct_sshp,pct_ssom,pct_ssot,pct_ssrc,pct_ssrr,pct_ssrs,pct_sssa,pct_sssb,pct_sswd,
#' ,pct_ssxb,
#'@param uid a vector of site-visit indicators
#'@param size.class a vector size classes
#'
#'@import plyr
#'@export
calculateBoatThalwegSubstrateMetrics <- function(uid, size.class){
  x <- addBoatSubstrateSizes(uid, size.class)
  f <- function(x){
    lDiam <- na.omit(x$lDiam)
    sizes <- c(count.notna(x$result.no.ot), summary.nrsa(lDiam))
    names(sizes) <- c('n', 'lsub_d16', 'lsub_d25', 'lsub_d50', 'lsub_d75', 
                      'lsub_d84', 'lsub_dmm', 'lsubd_sd', 'lsub_iqr')
    proportions <- prop.table(table(x$result)) * 100
    return(c(sizes, proportions))
  }
  metrics <- ddply(x, .(uid), f, .progress = plyrProgress())
  metrics$pct_safn <- metrics$pct_sa + metrics$pct_fn
  metrics <- meltMetrics(metrics)
  progressReport('Finished non-wadeable thalweg substrate metrics.')
  return(metrics)
}

#'@rdname calculateBoatThalwegSubstrateMetrics
#'@param db dominant, bottom substrate classes
#'@param ds dominant, shoreline substrate classes
#'@param sb secondary, bottom substrate classes
#'@param ss secondary, shoreline substrate classes
#'@export
calculateBoatLittoralSubstrateMetrics <- function(uid, db, ds, sb, ss){
  kAllowedClasses <- c('RS', 'RR', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA',
                       'FN', 'HP', 'WD', 'OT', 'BL', 'OM', 'RC')
  x <- data.frame(uid, db, ds, sb, ss)
  x <- reshape2::melt(x, id.var = 'uid', variable.name = 'parameter', value.name = 'metric')
  x$metric <- factor(x$metric, levels = kAllowedClasses)
  metrics <- prop.table(table(x), 1:2)
  metrics <- as.data.frame(metrics, responseName = 'result')
  metrics$metric <- paste('pct_', metrics$parameter, tolower(metrics$metric), sep = '')
  metrics$parameter <- NULL
  progressReport('Finished non-wadeable littoral substrate metrics.')
  return(metrics)
}
