#  metsChannelHabitat.r
#  
#  01/04/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r, NA_filler.r and summaryby.r
#  03/23/10 ssr moved creation of unit test dataframes to separate functions.
# 06/03/10 cws Removing pct_sb from calculations.  Is not legal channel unit
#          code, and does not occur in the 2008-2009 field data, and it collides
#          with the substrate metric pct_sb.
#
#  

require(RODBC)
require(RUnit) 

metsChannelHabitat <- function()
  #Calculates Channel Habitat metrics:
  # Wadeable Protocal:
  #
  #
  #Boatable Protocal:
  #
  #
  #These metrics are saved to a csv file in the directory specified by
  # NRSAMetricsLocation.
  #
  # Returns NULL on success or a character string describing the problem if one
  # occurs.
  # ARGUMENTS:
  # none
 
  {
      intermediateMessage('Channel Habitat calculations', loc='start')
 
      intermediateMessage('.1 Read in data', loc='end')      
        on.exit(odbcClose(indb))
        indb <- odbcConnect(dsn)
        data <- fetchNRSATable(indb, 'tblThalweg2')
   
      intermediateMessage('.2 call function metsChannelHabitat.1', loc='end')
        mets <- metsChannelHabitat.1(data)
 
      intermediateMessage('.3 Write results', loc='end')
      # write the results
        rc <- writeNRSACalcResults(mets, 'metsChannelHabitat.csv')

      intermediateMessage('  Done.', loc='end')
        return(rc)
  }

FormatHabitatData <- function(data){
  intermediateMessage('.1.1 subset data and check RESULTS for wadeable and boatable', loc='end')
  i <- with(data,
            {
              isChanuncd <- PARAMETER == 'CHANUNCD'
              notSidechan <- TRANSECT %in% LETTERS[1:11]
              isWadeResponse <- RESULT %in% c('FA','CA','RA','RI','GL','PB','PP',
                                              'PD','PL','PT','P','DR') &
                                                SAMPLE_TYPE == 'PHAB_THALW'
              isBoatResponse <- RESULT %in% c('FA','RA','RI','GL','PO','CA','DR') &
                SAMPLE_TYPE == 'PHAB_THAL'
              isChanuncd & notSidechan & (isWadeResponse | isBoatResponse)
            })
  # remove sidechannels (xa,xb etc.) and extraneous data
  data <- subset(data, subset = i)
  # ensure all habitat types present in RESULT
  data$RESULT <- factor(data$RESULT, 
                        levels = c('FA','CA','RA','RI','GL','PB',
                                   'PP','PD','PL','PT','P','DR', 'PO'))
  data
}

CalcHabitatProportions <- function(uid, habitat, sample.type){
  #compute individual metrics
  chan.hab.counts <- table(UID = uid, METRIC = habitat)
  chan.hab.pcts <- prop.table(chan.hab.counts, 1) * 100
  
  #compute summed metrics
  pct_fast <- rowSums(chan.hab.pcts[, c('FA', 'CA', 'RA', 'RI')])
  pct_slow <- rowSums(chan.hab.pcts[, c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'GL', 'PO')])
  pct_pool <- rowSums(chan.hab.pcts[, c('PP', 'PD', 'PB', 'PL', 'PT', 'P', 'PO')])
  composite.mets <- 
    data.frame(UID    = names(pct_fast),
               METRIC = rep(c('pct_fast', 'pct_slow', 'pct_pool'), each = length(pct_fast)),
               RESULT = c(pct_fast, pct_slow, pct_pool))
  
  # remove metrics that are specific to either wadeable or boatable.
  wade.uids <- unique(uid[sample.type == 'PHAB_THALW'])
  boat.uids <- unique(uid[sample.type == 'PHAB_THAL'])
  wade.only <- c('PB', 'PP', 'PD', 'PL', 'PT', 'P')
  boat.only <- 'PO'
  chan.hab.pcts <- as.data.frame(chan.hab.pcts, responseName = 'RESULT')
  chan.hab.pcts <- subset(chan.hab.pcts, 
                          subset = !((METRIC %in% boat.only & UID %in% wade.uids) |
                                     (METRIC %in% wade.only & UID %in% boat.uids)))
  # remove 'PO' sum because it is redudant with pct_pool
  chan.hab.pcts <- subset(chan.hab.pcts, METRIC != 'PO')
  chan.hab.pcts$METRIC <- paste('pct_', tolower(chan.hab.pcts$METRIC), sep = '')
  mets <- rbind(chan.hab.pcts, composite.mets)
  return(mets)
}

metsChannelHabitat.1 <- function(indat)
   # Does all the real work for metsChannelHabitat.
   # Returns a dataframe of calculations if successful
   # or a character string describing the problem if 
   # one was encountered.
   #
   # ARGUMENTS:
   # indat		dataframe of channel data.
   # protocols	dataframe relating UID to the 
   #			  sampling protocol used at the site.
   #			  
{
 
  intermediateMessage('.1.0Channel Habitat mets', loc='end')
  indat <- FormatHabitatData(indat)
  
  intermediateMessage('.1.2 calculate results', loc='end')
  mets <- CalcHabitatProportions(indat$UID, indat$RESULT, indat$SAMPLE_TYPE)
  
  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')
  
  intermediateMessage('.1.4 Done with function metsChannelHabitat.1 ', loc='end')
  return(mets) 

}

# metsChannelHabitat()


