#  metsLittoralDepth.r
#  
#  01/25/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r and summaryby.r
#  06/01/10 cws removed odd code that somehow showed up here.
#  
require(RODBC)
require(RUnit) 

metsLittoralDepth <- function()
#Calculates Littoral Depth metrics:
# Boatable Protocal:
#=xlit mxlit mnlit vlit
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Littoral Depth calculations', loc='start')
  intermediateMessage('.1 Read in data', loc='end')
  on.exit(close(chan))
  
  # read in densiometer readings from database
  chan <- odbcConnect('NRSA2')
  rawdat <- fetchNRSATable(chan, 'tblCHANDEPTH2')
          
  intermediateMessage('.2 call function metsLittoralDepth.1', loc='end')
         
  # calculate the metrics
  mets <- metsLittoralDepth.1(rawdat)
  #  if(is.character(mets)) return(mets)
        
  intermediateMessage('.3 Write results', loc='end')
  # write the results
  rc <- writeNRSACalcResults(mets, 'metsLittoralDepth.csv')

        
  intermediateMessage('  Done.', loc='end')
  return(rc)
}



metsLittoralDepth.1 <- function(indat)
# Does all the real work for metsLittoralDepth.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of littoral data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('.1.0Littoral Depth mets', loc='end')

  intermediateMessage('.1.1 check data and split into wadeable (mid and bank) and boatable (bank)', loc='end')
  cdData <- subset(indat,PARAMETER %in% c('SONAR','POLE'))
  #cdData <- merge(cdData, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')

  mdx <- summaryby(cdData,'mean',"xlit")
  mds <- summaryby(cdData,'sd',"vlit")
  mdm <- summaryby(cdData,'max',"mxlit")
  mdn <- summaryby(cdData,'min',"mnlit")

  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')
  mets <- rbind(mdx,mds,mdm,mdn)

  intermediateMessage('.1.4 Done with function metsLittoralDepth.1 ', loc='end')

  return(mets)
 
}

