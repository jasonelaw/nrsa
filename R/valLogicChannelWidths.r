# valLogicChannelWidths.r
#
# 04/23/10 cws recreated
#

valLogicChannelWidths <- function()
# Performs logical relationship tests on channel width data in the NRSA study:
# wadeable dist_lb and wet_wid.  Requires prior validation of tblBANKGEOMETRY2
# and tblCHANNELCROSSSECTION2.
#
{
  # Retrieve data to be validated, along with form metadata and site information
  chan <- odbcConnect('NRSA2IslandOne')
  on.exit(valLogicThalweg.cleanup(chan))

  bg <- fetchNRSATable(chan, 'tblBANKGEOMETRY2')
  if(!is.data.frame(bg)) return(bg)
  

  ccs <- fetchNRSATable(chan, 'tblCHANNELCROSSSECTION2')
  if(!is.data.frame(ccs)) return(ccs)

  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')
  if(!is.data.frame(siteInfo)) return(siteInfo)

  validationResults <- valLogicChannelWidths.1(bg, ccs, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d values with illogical channel width relations"
               ,nrow(validationResults)
               ))

  # Write validation results to a spreadsheet for review
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLogicChannelWidths.csv'
                                        ,sep=''
                                        )
                                  )
  
}

valLogicChannelWidths.1 <- function(bg, ccs, meta.df, siteInfo)
# Does all the work for valLogicChannelWidths()
#
# ARGUMENTS:
# bg        dataframe with wadeable portion of bank geometry data.  The
#             BANKWID and WETWID parameters must be present, others may be
#             missing.
# ccs       dataframe with wadeable portion of channel cross section data.  The
#             DIST_LB parameter must be present, others may be missing.
# meta.df   dataframe with parameter and form information from
#             tblPARAMETERDESCRIPTIONS2.
# siteInfo  dataframe with site information from tblVISITS2.
{
  # Create logical validation results using just wadeable data
  wadeableSites <- subset(siteProtocol(unique(c(bg$UID, ccs$UID)))
                         ,PROTOCOL=='WADEABLE'
                         )$UID
  bankwid <- rename(subset(bg, PARAMETER == 'BANKWID' & UID %in% wadeableSites)
                   ,'RESULT'
                   ,'BANKWID'
                   )
  wetwid <- rename(subset(bg, PARAMETER == 'WETWID' & UID %in% wadeableSites)
                   ,'RESULT'
                   ,'WETWID'
                   )
  dist_lb <- rename(subset(ccs, PARAMETER =='DIST_LB' & UID %in% wadeableSites)
                   ,'RESULT'
                   ,'DIST_LB'
                   )

  # Gather values onto same row for comparisons.
  aa <- merge(bankwid[c('UID','TRANSECT','BANKWID','SAMPLE_TYPE')]
             ,wetwid[c('UID','TRANSECT','WETWID')]
             ,by=c('UID','TRANSECT')
             )
  tt <-  merge(dist_lb[c('UID','TRANSECT','DIST_LB')]
              ,aa
              ,by=c('UID','TRANSECT')
              )
  tt <- rename(tt, c('RESULT.x','RESULT.y'), c('DIST_LB','WETWID'))
  tt$DIST_LB <- as.numeric(tt$DIST_LB)
  tt$BANKWID <- as.numeric(tt$BANKWID)
  tt$WETWID <- as.numeric(tt$WETWID)

  # Compare values, determine locations where they conflict and specify the
  # nature of the error.
  errWetDist <- subset(tt, WETWID < DIST_LB & !is.na(WETWID) & !is.na(DIST_LB))
  errWetDist$REASON <- "WETWID exceded by DIST_LB"
  errBankDist <- subset(tt, BANKWID < DIST_LB & !is.na(BANKWID) & !is.na(DIST_LB))
  errBankDist$REASON <- "BANKWID exceded by DIST_LB"
  errBankWet <- subset(tt, BANKWID < WETWID & !is.na(BANKWID) & !is.na(WETWID))
  errBankWet$REASON <- "BANKWID exceded by WETWID"


  # Construct validation results
  if(is.null(logicResults)) {
      vv <- "There were zero logical errors detected in the thalweg data."
  } else {
      vv <- constructNRSAValidationResults(logicResults, meta.df, siteInfo)
  }
  intermediateMessage('. Done.', loc='end')

  return(vv)
}

junk <- function()
{
###########################################
# look at dist_lb and wt_wid values

require(RODBC)

chan <- odbcConnect('NRSA2')
ccs <- subset(fetchNRSATable(chan, 'tblCHANNELCROSSSECTION2')
             ,PARAMETER=='DIST_LB'
             ,select=c(UID,TRANSECT,TRANSDIR,RESULT)
             )
bg <- subset(fetchNRSATable(chan, 'tblBANKGEOMETRY2')
            ,PARAMETER=='WETWID'
            ,select=c(UID,TRANSECT,RESULT)
            )

randy <- merge(ccs, bg, by=c('UID','TRANSECT'))
randy <- rename(randy, c('RESULT.x','RESULT.y'), c('DIST_LB','WETWID'))
randy$DIST_LB <- as.numeric(randy$DIST_LB)
randy$WETWID <- as.numeric(randy$WETWID)

bad <- subset(randy, WETWID*1.1 < DIST_LB & WETWID>0)

}