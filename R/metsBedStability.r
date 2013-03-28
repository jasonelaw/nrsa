# metsBedStability.r
#
# 12/24/09 cws created
#  3/11/10 cws Reading metrics calcs with readNRSACalculationResults() instead
#          of readNRSAValidationResults().  Call to writeNRSACalcResults()
#          corrected.
#  3/12/10 cws Changed PROTOCOL to protocol.  Adding s_rp100 calculation here
#          since it is used here, and modifying unit test accordingly.
#
#  03/18/10 ssr Changed 'protocol' from siteProtocol to 'PROTOCOL'
#  03/22/10 ssr moved creation of unit test dataframes to separate functions.
#   3/25/10 cws Changed diff() calls to dfCompare().
#
#  04/02/10 mrc Modified unit test and metrics code to handle data with just
#           one protocol.  
#  04/13/10 cws Increasing precision of s_rp100 values in unit test by
#           separating checks of s_* and non-s_* metrics, setting precision
#           to 10E-4 and 10E-7 respectively.  The difference in attainable
#           accuracy is due to the exponential calculation of s_rp100.
#
# TO DO: Expand check of wadeable reaches.
#        Handle case when protocol can not be determined.
#
require(RODBC)
require(RUnit)

metsBedStability <- function()
# Calculates NRSA bed stability metrics:
#   Wadeable protocol:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
#   s_ldmb_bw5 s_lrbs_bw5 s_lrbs_bw6
#
#   Boatable protocol:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Bed stability metrics are calculated entirely with previously calculated
  # metrics.  Retrieve these metrics from their individual temporary csv
  # files and combine in a single dataframe.
  cm <- readNRSACalculationResults('metsChannelMorphology.csv')  # xdepth, sddepth, xbkf_h, xbkf_w, xwidth
  if(is.character(cm)) return(cm)

  sb <- readNRSACalculationResults('metsSlopeBearing.csv')       # xslope
  if(is.character(sb)) return(sb)

  sc <- readNRSACalculationResults('metsSubstrateCharacterization.csv') # lsub_dmm, lsub2dmm
  if(is.character(sc)) return(sc)

  rp <- readNRSACalculationResults('metsResidualPools.csv')      # rp100, s_rp100
  if(is.character(rp)) return(rp)

  lwd <- readNRSACalculationResults('metsLargeWoody.csv')   # v1w_msq
  if(is.character(lwd)) return(lwd)

  fc <- readNRSACalculationResults('metsFishCover.csv')          # xfc_lwd
  if(is.character(fc)) return(fc)

  mets <- rbind(subset(cm, METRIC %in% c('xdepth', 'sddepth', 'xbkf_h', 'xbkf_w', 'xwidth'))
               ,subset(sb, METRIC %in% c('xslope'))
               ,subset(sc, METRIC %in% c('lsub_dmm', 'lsub2dmm'))
               ,subset(rp, METRIC %in% c('rp100', 's_rp100'))
               ,subset(lwd, METRIC %in% c('v1w_msq'))
               ,subset(fc, METRIC %in% c('xfc_lwd'))
               )
             

  protocols <- siteProtocol(unique(mets$UID))

  bs <- metsBedStability.1(mets, protocols)
  if(is.character(bs)) return(bs)

  rc <- writeNRSACalcResults(bs, 'metsBedStability.csv')
}

metsBedStability.1 <- function(mets, protocols)
# Does the work for for metsBedStability
#
# ARGUMENTS:
# mets      dataframe of relevant metrics: xdepth, sddepth, xbkf_h, xbkf_w,
#             xwidth, xslope, lsub_dmm, rp100, v1w_msq, xfc_lwd
# protocols dataframe specifying the protocol each UID was sampled with.
{
  intermediateMessage('Beginning bed stability calculations', loc='start')

  # Convert long to wide so metrics are all on same row.  Drop prefix for
  # column names.
  mm <- reshape(mets, idvar='UID', direction='wide', timevar='METRIC')
  names(mm) <- gsub('RESULT\\.', '', names(mm))

  # Convert boatable depths from m to cm to match wadeable units, using the
  # protocols dataframe
  mm <- merge(mm, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  mm$xdepth <- ifelse(mm$PROTOCOL=='BOATABLE', mm$xdepth * 100, mm$xdepth)
  mm$sddepth <- ifelse(mm$PROTOCOL=='BOATABLE', mm$sddepth * 100, mm$sddepth)

  # Make zero slopes slighly positive so we can log them
  mm$xslope <- ifelse(mm$xslope<=0.01, 0.01, mm$xslope)

  # Calculate s_rp100 here.
  mm$s_rp100 <- 10**(-0.44767 +
                     1.25381*log10(mm$sddepth) +
                     -0.20675*log10(mm$xslope)
                    )

  intermediateMessage('.1')

  # Crude estimate of critical erodible substrate diamter (mm).  This estimate
  # of the hydrologic radius (Rbf) assumes flow through a very simple channel.
  critdia <- 13.7 * (0.5 * mm$xdepth * 10) * (mm$xslope / 100)
  mm$ltest <- ifelse(critdia > 0, log10(critdia), NA)
  mm$lrbs_tst <- mm$lsub_dmm - mm$ltest

  intermediateMessage('.2')

  # Refined estimate of critical erodible substrate diameter (mm), taking LWD
  # and residual pool 'roughness' into account.  Do this for actual pools and
  # estimated pools.  Remove Rw from Rbf up to 90% of Rbf value.
  Rbf <- 0.5 * ((mm$xdepth - mm$rp100) * 10 + mm$xbkf_h * 1000)
  s_Rbf <- 0.5 * ((mm$xdepth - mm$s_rp100) * 10 + mm$xbkf_h * 1000)

  Rw <-ifelse(is.na(mm$v1w_msq)
             ,ifelse(mm$xfc_lwd == 0    # fill in missing LWD volume as 0 when
                    ,0                  # no LWD fishcover
                    ,NA                 # otherwise we really can't guess.
                    )
             , mm$v1w_msq * 1000
             )

  Rbf <- ifelse(Rw >= 0.9 * Rbf, 0.1 * Rbf, Rbf - Rw)
  s_Rbf <- ifelse(Rw >= 0.9 * s_Rbf, 0.1 * s_Rbf, s_Rbf - Rw)

  critdia <- 13.7 * Rbf * mm$xslope / 100
  s_critdia <- 13.7 * s_Rbf * mm$xslope / 100

  mm$ldmb_bw5 <- log10(ifelse(critdia > 0, critdia, NA))
  mm$s_ldmb_bw5 <- log10(ifelse(s_critdia > 0, s_critdia, NA))

  intermediateMessage('.3')

  # Calculate old version of logged critical diameter that were based on the
  # wood density when it accidentally used the wetted widths instead of the
  # bankfull widths.
  Rw <- ifelse(!is.na(Rw) & !is.na(mm$xbkf_w) & mm$xwidth > 0
              ,Rw * mm$xbkf_w / mm$xwidth
              ,NA
              )

  Rbf <- ifelse(Rw >= 0.9 * Rbf, 0.1 * Rbf, Rbf - Rw)
  critdia <- 13.7 * Rbf * mm$xslope / 100
  mm$ldmb_bw4 <- log10(ifelse(critdia > 0, critdia, NA))

  intermediateMessage('.4')

  # Calculate log10 of the bed stability values based on previous estimates
  # of critical substrate diameter.
  mm$lrbs_bw4   <- mm$lsub_dmm - mm$ldmb_bw4
  mm$lrbs_bw5   <- mm$lsub_dmm - mm$ldmb_bw5
  mm$s_lrbs_bw5 <- mm$lsub_dmm - mm$s_ldmb_bw5
  mm$lrbs_bw6   <- mm$lsub2dmm - mm$ldmb_bw5
  mm$s_lrbs_bw6 <- mm$lsub2dmm - mm$s_ldmb_bw5

  intermediateMessage('.5')

  # Calculate some better than refined, research-based bed stability estimates
  # Kaufmann, P.R. et al., A roughness-corrected index of relative bed stability
  # for regional stream surveys.  Geomorphology (2007)
  rho <- 998
  rhoSed <- 2650
  g <- 9.807
  Dbf_th <- mm$xbkf_h + (mm$xdepth / 100)
  Rb3 <- 0.65 * Dbf_th
  rp <- mm$rp100 / 100
  s <- mm$xslope / 100
  viscosity <- 0.00000102
  v1w_msq <- mm$v1w_msq
  lsub_dmm <- mm$lsub_dmm

  # Total hydraulic resistance, eqn 13b
  Ct_rpwd <- 1.21 * (rp^1.08) * ((rp + v1w_msq)^0.638) * (Dbf_th^-3.32)

  # Hydraulic resistance due to particles
  tt <- (1/8) * (2.03 * log10(12.2 * Rb3 / (((10^lsub_dmm)/1000)) ))^(-2)
  Cp3_mill <- ifelse(tt < 0.002, 0.002, tt)

  # Intermediate calculations, with restriction as described for eqn 16
  tt <- Cp3_mill / Ct_rpwd
  Cp3Ctrpwd_rat <- ifelse(tt > 1, 1, tt)

  # Hydraulic resistance due to particles, eqn 10b
  Rrpw3 <- Rb3 * (Cp3Ctrpwd_rat^(1/3))

  # Reynolds number at bankfull, eqn 14
  ReyP3 <- ((g * Rb3 * s)^0.5) * ((10^lsub_dmm)/1000) / viscosity

  # Shields parameter, eqn 15a, 15b.  Note 15a uses an abbreviated value of
  # the exponent in the case of small Reynolds numbers
  Shld_Px3 <- ifelse(ReyP3>0
                    ,ifelse(ReyP3 < 26
                           ,0.04 * ReyP3^(-0.24)
                           ,0.5 * ((0.22 * (ReyP3^(-0.6))) +
                                   0.06 * (10^(-7.7 * (ReyP3^(-0.6))))
                                  )
                           )
                    ,NA
                    )

  # Bed surface particle critical diameter (mm) Dcbf* from eqn 16.
  mm$Dcbf_g08 <- 1000 * (rho * g * Rrpw3 * s) / (Shld_Px3 * (rhoSed - rho) * g)

  mm$ldcbf_g08 <- log10(mm$Dcbf_g08)
  mm$lrbs_g08 <- mm$lsub_dmm - mm$ldcbf_g08

  intermediateMessage('.6')
  # Transpose wide to long format, and clean up factors, rownames and attributes
  mm2 <- subset(mm, select=-c(xdepth,sddepth,xbkf_h,xbkf_w,xwidth,xslope
                             ,lsub_dmm,lsub2dmm,rp100,v1w_msq,xfc_lwd
                             ,PROTOCOL
                             )
               )
  tmm <- reshape(mm2, idvar=c('UID'), direction='long'
                ,varying=names(mm2)[names(mm2) != 'UID']
                ,times=names(mm2)[names(mm2) != 'UID']
                ,v.names='RESULT', timevar='METRIC'
#                ,drop=c('xdepth','xbkf_h','xbkf_w','xwidth','xslope','lsub_dmm'
#                       ,'lsub2dmm','rp100','s_rp100','v1w_msq','xfc_lwd'
#                       ,'PROTOCOL'
#                       )
                )
  row.names(tmm)<-NULL
  tmm$UID <- as.character(tmm$UID)
  tmm <- data.frame(tmm)

  intermediateMessage('  Done.', loc='end')
  return(tmm)
}


