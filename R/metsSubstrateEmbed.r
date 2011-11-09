#  metsSubstrateEmbed.r
#  
#  01/27/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() calls to several files
#  03/18/10 cws Unit test merge() with all=TRUE
#  03/22/10 cws moving creation of test dataframes for unit test to separate
#           functions.
#  03/31/10 cws Removing extra print() statements and commented-out code.
#  

require(RODBC)
require(RUnit)

metsSubstrateEmbed <- function()
#Calculates Substrate Embeddedness metrics:
#
#N33 N55 VCEMBED VEMBED XCEMBED XEMBED
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Substrate Embeddedness calculations', loc='start')

  intermediateMessage('.1 Read in data', loc='end')
  # read in Substrate Embeddedness data from database
  chan <- odbcConnect('NRSA2')
  on.exit(close(chan))
  rawdat <- fetchNRSATable(chan, 'tblCHANNELCROSSSECTION2')

  intermediateMessage('.2 call function metsSubstrateEmbed.1', loc='end')

  # calculate the calculations
  mets <- metsSubstrateEmbed.1(rawdat)
  if(is.character(mets)) return(mets)

  intermediateMessage('.3 Write results', loc='end')
  # write the results
  rc <- writeNRSACalcResults(mets, 'metsSubstrateEmbed.csv')

  intermediateMessage('  Done.', loc='end')
  return(rc)
}



metsSubstrateEmbed.1 <- function(indat)
# Does all the real work for metsSubstrateEmbed.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of canopy data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('.1.0Substrate Embeddedness mets', loc='end')

  intermediateMessage('.1.1 get embeddedness data observations from dataset', loc='end')

  #Create datasets needed for the calculations
  edata <- subset(indat, PARAMETER =='EMBED' )   #all embbedded data sites

  #set RESULT to numeric
  edata$RESULT<-as.numeric( edata$RESULT)

  cdata <- subset(edata, TRANSDIR %in% c('LC','CT','RC'))

  intermediateMessage('.1.2 sent dataset to summaryby function', loc='end')

  #Use summaryby for the three metrics of all transdirs
  ca <- summaryby(edata,'count',"n55")
  xa <- summaryby(edata,'mean',"xembed")
  va <- summaryby(edata,'sd',"vembed")

  #Use summaryby for the three metrics of the channel transdirs
  cc <- summaryby(cdata,'count',"n33")
  xc <- summaryby(cdata,'mean',"xcembed")
  vc <- summaryby(cdata,'sd',"vcembed")

  temp1<-NA_filler(indat,edata,list('n55','xembed','vembed'))
  temp2<-NA_filler(indat,edata,list('n33','xcembed','vcembed'))

  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')

  #add the datasets together and return
  mets <- rbind(ca,xa,va,cc,xc,vc)
  mets$RESULT<-ifelse(mets$RESULT=='NaN',NA,mets$RESULT)
 
  intermediateMessage('.1.4 Done with function metsSubstrateEmbed.1 ', loc='end')

  return(mets)

}


