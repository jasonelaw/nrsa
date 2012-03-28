#SubstrateCharacterization

#calculate Substrate Characterization Metrics for streams and rivers

# 2/11/2010 mrc started
# 3/4/2010 errs are currently at 10-3
# 3/19/10 cws setting all=TRUE in unit test merge of expected and actual
#         results.  Removing unwanted calculations from metrics, adding missing
#         calcs to metrics, correcting code and test data.
# 3/31/10 cws Added call to on.exit()
#

#Stream mets

#,d16,d50,d84,dgm,lsub2d16,lsub2d16inor,lsub2d25,lsub2d50,lsub2d50inor,lsub2d75,
 #,lsub2d84,lsub2d84inor,lsub2dmm,lsub2dmm_nor,lsub2iqr,lsubd2sd,lsubd2sd_nor,
 #,lsubd_sd,lsubd_sd_nor,lsub_d16,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,
 #,lsub_dmm_nor,lsub_iqr,n,n_nor,pct_bdrk,pct_bigr,pct_bl,pct_cb,pct_fn,pct_gc,
 #,pct_gf,pct_hp,pct_org,pct_om,pct_ot,pct_rc,pct_rr,pct_rs,pct_sa,pct_safn,pct_sb,
 #,pct_sfgf,pct_wd,pct_xb,sub2dmm_nor,subd2sd_nor,subd_sd_nor,sub_dmm_nor,

#River mets  

#,d16,d50,d84,dgm,LDCBF_G08,lsub2d16inor,lsub2d50inor,lsub2d84inor,lsubd_sd,lsub_d16,
# ,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,lsub_iqr,n,pct_bh,pct_bl,pct_cb,
# ,pct_dbbl,pct_dbcb,pct_dbfn,pct_dbgc,pct_dbgf,pct_dbhp,pct_dbom,pct_dbot,pct_dbrc,
# ,pct_dbrr,pct_dbrs,pct_dbsa,pct_dbsb,pct_dbwd,pct_dbxb,pct_dsbl,pct_dscb,pct_dsfn,
# ,pct_dsgc,pct_dsgf,pct_dshp,pct_dsom,pct_dsot,pct_dsrc,pct_dsrr,pct_dsrs,pct_dssa,
# ,pct_dssb,pct_dswd,pct_dsxb,pct_fn,pct_gr,pct_ot,pct_sa,pct_safn,pct_sbbl,pct_sbcb,
# ,pct_sbfn,pct_sbgc,pct_sbgf,pct_sbhp,pct_sbom,pct_sbot,pct_sbrc,pct_sbrr,pct_sbrs,
# ,pct_sbsa,pct_sbsb,pct_sbwd,pct_sbxb,pct_ssbl,pct_sscb,pct_ssfn,pct_ssgc,pct_ssgf,
# ,pct_sshp,pct_ssom,pct_ssot,pct_ssrc,pct_ssrr,pct_ssrs,pct_sssa,pct_sssb,pct_sswd,
# ,pct_ssxb,


#input for streams tblCHANNELCROSSSECTION2



#wrapper function

metsSubstrateCharacterization <- function ()

# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Substrate Characterization', loc='start')

  #read in the data from tblChannelCrossSection2, tblThalweg2, tblLittoral2

  chan <- odbcConnect ('NRSA2')
  on.exit(close(chan))
  df1 <- fetchNRSATable (chan, 'tblChannelCrossSection2',
                         where = "PARAMETER IN ('SIZE_CLS', 'XSIZE_CLS')")
  df2 <- fetchNRSATable (chan, 'tblThalweg2',
                         where = "PARAMETER IN ('SIZE_CLS')")
  df3 <- fetchNRSATable (chan, 'tblLITTORAL2',
                         where = "PARAMETER IN ('SHOREDOM', 'BOTTOMSEC', 'SHORESEC', 'BOTTOMDOM')")
  intermediateMessage ('fetch_data.1')
  
  #calculate the metrics
  mets <- metsSubstrateCharacterization.1 (df1, df2, df3)
  if(is.character(mets)) return (mets)

  #write the results
  rc <- writeNRSACalcResults (mets, 'metsSubstrateCharacterization.csv')

  intermediateMessage ( ' Done.', loc='end')

  return (rc)
}



metsSubstrateCharacterization.1 <- function (df1, df2, df3)

 #Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.

 #ARGUMENTS:
 #df1   dataframe of the channel cross section data.
 #df2   dataframe of the thalweg data.
 #df3   dataframe of the littoral data.
 
{
  # An 8 number summary (5 quantiles, mean, sd, IQR) is used in several places
  # for metrics.  summary.nrsa in NARSShared does it.
  
  #do the calculations
 
  #convert the size classes to the charDia (geometric mean of the extreme sizes)

  #make a dataframe with a numeric representation of the size classes .. start with all the
  #size classes, and then make a smaller set with some exclusions
  #there are three data subsets we need to work with
  #(1) ALL the size_classes  (mm)
  #(2) subclasses excluding HP, RD, RR, RS, RC, OT, WD   (tt)
  #(3) subclasse INCLUDES all classes and lumps the boulder class (XB+SB= BL)  (bl)
  #(4) subclasses excluding HP, RD, RR, RS, RC, OT, WD, and lumps the boulder class (XB+SB= BL)  (ttbl)
  
  intermediateMessage('Create numeric size classes for streams.2', loc='end') 
  sizes <- data.frame('CLASS' = c('OT', 'WD', 'HP', 'FN', 'SA', 'GF', 'GC', 
                                  'CB', 'SB', 'XB', 'BL', 'RS', 'RR', 'RC'),
                      'min'   = c(NA, NA, NA, 0.001, 0.06, 2, 16, 64, 250, 1000,
                                  250, 4000, 4000, 4000),
                      'max'   = c(NA, NA, NA, 0.06, 2, 16, 64, 250, 1000, 4000,
                                  4000, 8000, 8000, 8000), stringsAsFactors = F)
  sizes$CLASS <- factor(x      = sizes$CLASS, 
                        levels = c('OT', 'WD', 'HP', 'FN', 'SA', 'GF', 'GC', 
                                   'CB', 'SB', 'XB', 'BL', 'RS', 'RR', 'RC'))
  sizes$diam <- apply(sizes[,2:3], 1, gmean)
  sizes$lDiam <- log10(sizes$diam)
  
  # Modify RESULT field to create the four subsets mentioned above
  bl <- list(OT = "OT", WD = "WD", HP = "HP", FN = "FN", SA = "SA", GF = "GF", 
             GC = "GC", CB = "CB", BL = c("XB", "SB"), RS = "RS", RR = "RR", 
             RC = "RC")
  tt <- list(FN = "FN", SA = "SA", GF = "GF", GC = "GC", CB = "CB", SB = "SB", 
             XB = "XB")
  ttbl <- list(FN = "FN", SA = "SA", GF = "GF", GC = "GC", CB = "CB", 
             BL = c("XB", "SB"))
  df1$RESULT <- as.factor(df1$RESULT)
  df1$RESULT.ttbl <- df1$RESULT.tt <- df1$RESULT.bl <- df1$RESULT
  levels(df1$RESULT.bl) <- bl
  levels(df1$RESULT.tt) <- tt
  levels(df1$RESULT.ttbl) <- ttbl
  ldBug <- df1
  
  # Merge with sizes table, once for each RESULT subset
  for (i in c('RESULT', 'RESULT.tt', 'RESULT.bl', 'RESULT.ttbl')){
    suffix <- sprintf('.%s', strsplit(i, '\\.')[[1]][2])
    ldBug <- merge(ldBug, sizes[,c('CLASS', 'diam', 'lDiam')], 
                   by.x = i, by.y = 'CLASS', all.x = TRUE,
                   suffixes = c('', suffix))
  }
  
  #for each of these subsets of data above, we want summaries (lDiam) for all classes with numeric values
  f <- function(x){
    lDiam.mm <- na.omit(x$lDiam)
    lDiam.tt <- na.omit(x$lDiam.tt)
    diam.tt <- na.omit(x$diam.tt)
    lDiam.bl <- na.omit(x$lDiam.bl)
    lDiam.ttbl <- na.omit(x$lDiam.ttbl)
    diam.ttbl <- na.omit(x$diam.ttbl)
    ans <- c(# mm calcs
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
    return(ans)
  }
  ldBug <- ldBug[, c('UID', 'lDiam', 'lDiam.tt', 'diam.tt', 'lDiam.bl', 'lDiam.ttbl', 'diam.ttbl')]
  ldBug.res <- ddply(ldBug, .(UID), f, .drop = FALSE)
  streamld <- melt(ldBug.res, id.var = 'UID', variable.name = 'METRIC', 
                   value.name = 'RESULT')

  intermediateMessage('Complete size classes summaries for streams (ldiam).3', loc='end')
  intermediateMessage('Complete size classes summaries for streams (ldiam- NOR).4', loc='end')  
  intermediateMessage('Complete size classes summaries for streams (lumped boulder class).5', loc='end') 
  intermediateMessage('Complete size classes summaries for streams (lumped boulder class- NOR).6', loc='end')

  #interpolated metrics 
  sizes.tt <- subset(sizes, 
                     subset = !(CLASS %in% c('HP', 'RD', 'RR', 'RS', 'RC', 'OT',
                                             'WD', 'BL')), 
                     select = c('CLASS', 'min', 'max'))
  sizes.tt <- transform(sizes.tt, min = log10(min), max = log10(max))
  interpdata <- subset(df1, df1$RESULT %in% c('XB','SB','CB','GC','GF','SA','FN'))
  calcs <- interpolatePercentile(interpdata, 'RESULT', c(16, 50, 84), 
                                 c('lsub2d16inor','lsub2d50inor', 'lsub2d84inor'),
                                 sizes.tt)
  calcs$d16 <- 10^calcs$lsub2d16inor
  calcs$d50 <- 10^calcs$lsub2d50inor
  calcs$d84 <- 10^calcs$lsub2d84inor
  calcs <- melt(calcs, id.var = 'UID', variable.name = 'METRIC', 
                value.name = 'RESULT')
 
#moving on to counts and percentages for these, want to count SIZE_CLS/XSIZE_CLS THE SAME,
#there are three 'n' values associated with these counts
# realallsize.... every size class
# allsize ....... every size class with a numeric value
# norsize........ all the mobile size classes with characteristic diameters
   realallsize <- df1
   realallsize$PARAMETER <- NULL
 
#get counts for each size class: BL, CB, FN, GC, GF, HP, OT, RC, RR, RS SA, SB, WD, XB
  realallsize$RESULT <- as.factor(realallsize$RESULT)
   levels(realallsize$RESULT) <- 
     list(OM = "OM", OT = "OT", WD = "WD", HP = "HP", FN = "FN", 
          SA = "SA", GF = "GF", GC = "GC", CB = "CB", 
          XB = "XB", SB = "SB", RS = "RS", RR = "RR", RC = "RC")

  # Function to calculate percentages for size classes
  f <- function(x){
    n <- sum(x$RESULT %in% c('RS', 'RR', 'RC', 'HP', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA', 'FN'))
    n_nor <- sum(x$RESULT %in% c('XB', 'SB', 'CB', 'GC', 'GF','SA',  'FN'))
    tbl <- table(x$RESULT)
    proportions <- prop.table(tbl) * 100
    names(proportions) <- paste('pct_', tolower(names(proportions)), sep = '')
    proportions <- within(as.list(proportions), 
                          {
                            pct_bl <- pct_xb + pct_sb
                            pct_bigr <- pct_rr + pct_rs + pct_rc + pct_bl + 
                                        pct_cb + pct_gc
                            pct_bdrk <- pct_rr + pct_rs
                            pct_safn <- pct_sa + pct_fn
                            pct_sfgf <- pct_sa + pct_fn + pct_gf
                            pct_org <- pct_om + pct_wd
                          })
    
    return(c(n =n, n_nor = n_nor, proportions, recursive = T))
  }

  # Calcualte percentages for each size class
  pct <- ddply(realallsize[,c('UID', 'RESULT')], .(UID), 
                 f, .drop = F)
  pct <- melt(pct, id.var = 'UID', variable.name = 'METRIC', value.name = 'RESULT')

  intermediateMessage('Complete size classes percentages for streams.7', loc='end')
  intermediateMessage('Streams - Done.8', loc='end')

  # on to the rivers
  # input from the boatable site is in the thalweg2 and littoral2
  # df2 contains all the SIZE_CLS responses from boatable

  # SIZE_CLS from the rivers have slightly different gmeans.
  rr <- data.frame(class = c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT'),
                   min = c(4000, 250, 64, 2, 0.06, 0.001, NA),
                   max = c(8000, 4000, 250, 64, 2, 0.06, NA),
                   stringsAsFactors=FALSE)
  rr$class <- factor(rr$class, 
                     levels = c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT'),
                     labels = c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT'))
  rr$diam <- apply(rr[,2:3], 1, gmean)
  rr$lDiam <- log10(rr$diam)
  
  #16, 25, 50, 75, 84, mean, std, iqr....... for rivers data
  ldRivmm <- merge(df2, rr,
                   ,by.x='RESULT', by.y='class'
                   ,all.x=TRUE)
  ldRivmm <- subset(ldRivmm,
                    subset = RESULT %in% c('BH', 'BL','CB', 'GR' ,'SA', 'FN'),
                    select = c('UID', 'RESULT', 'lDiam'))
  f <- function(x){
    lDiam <- na.omit(x$lDiam)
    ans <- c(count(x$RESULT), summary.nrsa(lDiam))
    names(ans) <- c('n', 'lsub_d16', 'lsub_d25', 'lsub_d50', 'lsub_d75', 
                    'lsub_d84', 'lsub_dmm', 'lsubd_sd', 'lsub_iqr')
    return(ans)
  }  
  riv1 <- ddply(ldRivmm, .(UID), f)
  riv1 <- melt(riv1, id.var  = 'UID', 
               variable.name = 'METRIC', 
               value.name    = 'RESULT')
  intermediateMessage ('Initial summaries for rivers.9', loc='end')      

  #get counts for each size class: BH, BL, CB, FN, GC, GF, HP, OT, RC, RR, RS SA, SB, WD, XB   
  #from the back of the thalweg form
  all.levels <- c('BH', 'BL', 'CB', 'GR', 'SA', 'FN','OT')
  all.labels <- paste('pct_', tolower(all.levels), sep = '')
  df2$RESULT <- factor(df2$RESULT, levels = all.levels, labels = all.labels)
  riv2 <- ddply(df2, .(UID), function(x) prop.table(table(x$RESULT)) * 100)

  # Calculate composite metric(s)
  riv2 <- mutate(riv2, pct_safn = pct_sa + pct_fn)
  riv2 <- melt(riv2, id.var  = 'UID', 
               variable.name = 'METRIC', 
               value.name    = 'RESULT')
  intermediateMessage ('Complete counts (pct) from the thalweg data.10', loc='end')
  
  #on to the littoral data
  df3$RESULT <- factor(df3$RESULT,
                       levels = c('RS', 'RR', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA',
                                  'FN', 'HP', 'WD', 'OT', 'BL', 'OM', 'RC'))
  riv3 <- ddply(df3, .(UID, PARAMETER), function(x) prop.table(table(x$RESULT)) * 100)                     
  riv3 <- melt(riv3, id.var  = c('UID', 'PARAMETER'), 
               variable.name = 'METRIC', 
               value.name    = 'RESULT')
  riv3$PARAMETER <- factor(riv3$PARAMETER, 
                           levels = c('BOTTOMDOM', 'BOTTOMSEC',
                                      'SHOREDOM', 'SHORESEC'),
                           labels = c('db', 'sb', 'ds', 'ss'))
  riv3$METRIC <- paste('pct_', riv3$PARAMETER, tolower(riv3$METRIC), sep = '')
  riv3$PARAMETER <- NULL


  intermediateMessage ('Complete pct from the littoral data.11', loc='end')
  intermediateMessage ('Rivers Done.12', loc='end')
 
  #things to put together
  mets <- rbind(streamld, calcs, pct, riv1, riv2, riv3)

  intermediateMessage('  Done.', loc='end')
  return(mets)

  }       
  
  #test of metsSubstrateCharacterization function

