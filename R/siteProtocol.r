# siteProtocol.r
#
# 03/25/10 cws Changed protocol to PROTOCOL for consistency.

siteProtocol <- function(sites){
# Relates NRSA UID values to the protocol used to sample them, based on the
# information recorded on the Stream Verification form.
# Returns a dataframe with columns UID and protocol, and one row per unique
# UID value if successful, or a character string describing the error if
# a problem was encountered.
#
# ARGUMENTS:
# sites     A vector of UID values
#
# ASSUMPTIONS:
# Table tblVISITS2 has column SAMPLED, which may have values ALTERED, BOATABLE,
#   PARTIAL BOATABLE, PARTIAL WADEABLE, WADEABLE, WADEABLE INTERRUPTED or NA.
# Table tblVISITS2 has column VALXSITE, which may have values ACCDENIED,
#   ALTERED, BOATABLE, DRYNOVISIT, DRYVISIT, IMPOUNDED, INACCPERM, INACCTEMP,
#   INTWADE, MAPERROR, NOTBOAT, NOTWADE, OTHR_NSP, OTHR_NST, PARBYBOAT,
#   PARBYWADE, WADEABLE, WETLAND or NA
  # Get sampling information recorded on Stream Verification form
  dsn  <- nrsa.options()$NRSADSN
  chan <- odbcConnect(dsn)
  on.exit(odbcClose(chan))
  site.info <- sqlFetch(chan, 'VERIFICATION')
  levs      <- unique(site.info[, c('BATCHNO', 'VALXSITE')])
  levels(levs$VALXSITE) <- list(BOATABLE = c('BOATABLE','PARBYBOAT','ALTERED'),
                                WADEABLE = c('INTWADE','PARBYWADE','WADEABLE','ALTERED','DRY', 'INTWADE'))
  protocol <- mapvalues(sites, levs$BATCHNO, as.character(levs$VALXSITE),  warn_missing = FALSE)
  return(protocol[, drop = T])
}

siteProtocol.1 <- function(sites, siteInfo)
# Does the work for siteProtocol() relying on VALXSITE
# ARGUMENTS:
# sites     A vector of UID values
# siteInfo  A dataframe with site information found in tblVISITS2
{
  # Use information to assess which protocol was used.
  tt <- subset(siteInfo, UID %in% unique(sites))

  # Make easy determinations of BOATABLE, WADEABLE, NONE (unsampled) or
  # nothing at all if something slips through the cracks.
  tt$PROTOCOL <- ''

  selection <- tt$VALXSITE %in% c('BOATABLE','PARBYBOAT','ALTERED')
  if(any(selection)) tt[selection,]$PROTOCOL <- 'BOATABLE'

  selection <- tt$VALXSITE %in% c('INTWADE','PARBYWADE','WADEABLE','ALTERED','DRY')
  if(any(selection)) tt[selection,]$PROTOCOL <- 'WADEABLE'

  selection <- is.na(tt$VALXSITE) |
               !(tt$VALXSITE %in% c('BOATABLE','PARBYBOAT','ALTERED','INTWADE'
                                   ,'PARBYWADE','WADEABLE','DRY')
                )
  if(any(selection)) tt[selection,]$PROTOCOL <- 'NONE'


  tt <- subset(tt, select=c(UID, PROTOCOL))
  
  return(tt)
}

# end of file