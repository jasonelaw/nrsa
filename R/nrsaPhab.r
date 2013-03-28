# nrsaPhab.r
#
# 02/18/10 cws created
# 03/18/10 cws completed arguments for and calls to all NRSA mets functions.
# 03/31/10 cws Removed old, commented out code.
# 05/24/10 cws Moved residual pool calculations after slope & bearing due to
#          dependency on xslope.
# 06/03/10 cws split up metrics calculation from the assembly of nrsaphab.csv.
#          This will facilitate re-assembly after partial recalculation of mets.
#          Resulted in creation of a few helper functions.

nrsaphab <- function(bankMorphology=TRUE
                    ,bedStability=TRUE
                    ,canopyDensiometer=TRUE
                    ,channelCharacters=TRUE
                    ,channelHabitat=TRUE
                    ,channelMorphology=TRUE
                    ,fishCover=TRUE
                    ,general=TRUE
                    ,humanInfluence=TRUE
                    ,invasiveSpecies=TRUE
                    ,largeWoody=TRUE
                    ,legacyTree=TRUE
                    ,littoralDepth=TRUE
                    ,residualPools=TRUE
                    ,riparianVegetation=TRUE
                    ,slopeBearing=TRUE
                    ,substrateCharacterization=TRUE
                    ,substrateEmbeddedness=TRUE
                    )
# Calculates metrics, assembles the resulting .csv files into a single file
# (nrsaphab.csv), and updates the phab metric table on the server.
#
# ARGUMENTS:
# all arguments take on TRUE or FALSE values, depending on whether that
# particular set of metrics calculations is desired.
#
# ASSUMPTIONS:
# The input data has been validated.  Illegal and misentered values will
#   result in undefined behaviour and metrics which are absolutely useless.
# The data were recorded using the EPA NRSA protocol.  Changes in the
#   measurement units from the expected values will result in undefined
#   behaviour and metrics which are absolutely useless.
#
{
  flags<- nrsaphab.packFlags(bankMorphology, bedStability, canopyDensiometer
                            ,channelCharacters, channelHabitat, channelMorphology
                            ,fishCover, general, humanInfluence, invasiveSpecies
                            ,largeWoody, legacyTree, littoralDepth, residualPools
                            ,riparianVegetation, slopeBearing
                            ,substrateCharacterization, substrateEmbeddedness
                            )

  flags2 <- nrsaphab.calculate(flags)

  nrsaphab.assemble(flags2)

  #nrsaphab.dbUpdate()
}


nrsaphab.calculate <- function(flags)
# Calculate the NRSA physical habitat metrics using data stored on a server. The
# set of metrics calculated is dependent on the supplied arguments, and the
# results will be combined into a single table (UID, METRIC, RESULT) written to
# the server.
#
# ARGUMENTS:
# flags      boolean vector of several flags indicating whether to calculate
#            a specific group of metrics, or NULL to calculate all sets.
#
{
  # if nothing specified, assume that all available files will be assembled.
  if(is.null(flags)) {
      flags <- nrsaphab.allFlags()
  }

  # Create a base dataframe in which to accumulate metrics results.
  nrsaMets <- NULL

  # Calculate the metrics as specified.  This is done in alphabetic order,
  # except when a calculation requires the previous calculation of other
  # metrics.
  if(nrsaphab.unpackFlag(flags,'bankMorphology')) {
      rc <- metsBankMorphology()
      if(!is.null(rc)) {
          print(sprintf("Error calculating bank morphology mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'bankMorphology')
      }
  }
  if(nrsaphab.unpackFlag(flags,'canopyDensiometer')) {
      rc <- metsCanopyDensiometer()
      if(!is.null(rc)) {
          print(sprintf("Error calculating canopy densiometer mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'canopyDensiometer')
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelCharacters')) {
      rc <- metsChannelChar()
      if(!is.null(rc)) {
         print(sprintf("Error calculating channel characteristics: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'channelCharacters')
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelHabitat')) {
      rc <- metsChannelHabitat()
      if(!is.null(rc)) {
          print(sprintf("Error calculating channel habitat mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'channelHabitat')
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelMorphology')) {
      rc <- metsChannelMorphology()
      if(!is.null(rc)) {
         print(sprintf("Error calculating channel morphology mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'channelMorphology')
      }
  }
  if(nrsaphab.unpackFlag(flags,'fishCover')) {
      rc <- metsFishCover()
      if(!is.null(rc)) {
          print(sprintf("Error calculating fish cover mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'fishCover')
      }
  }
  if(nrsaphab.unpackFlag(flags,'general')) {
      rc <- metsGeneral()
      if(!is.null(rc)) {
          print(sprintf("Error calculating general mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'general')
      }
  }
  if(nrsaphab.unpackFlag(flags,'humanInfluence')) {
      rc <- metsHumanInfluence()
      if(!is.null(rc)) {
          print(sprintf("Error calculating human influence mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'humanInfluence')
      }
  }
  if(nrsaphab.unpackFlag(flags,'invasiveSpecies')) {
      rc <- metsInvasiveSpecies()
      if(!is.null(rc)) {
          print(sprintf("Error calculating invasive species mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'invasiveSpecies')
      }
  }
  # Large Woody depends on reachlen calculated in metsGeneral()
  if(nrsaphab.unpackFlag(flags,'largeWoody')) {
      rc <- metsLargeWoody()
      if(!is.null(rc)) {
          print(sprintf("Error calculating LWD mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'largeWoody')
      }
  }
  if(nrsaphab.unpackFlag(flags,'legacyTree')) {
      rc <- metsLegacyTree()
      if(!is.null(rc)) {
          print(sprintf("Error calculating legacy tree mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'legacyTree')
      }
  }
  if(nrsaphab.unpackFlag(flags,'littoralDepth')) {
      rc <- metsLittoralDepth()
      if(!is.null(rc)) {
          print(sprintf("Error calculating littoral depth mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'littoralDepth')
      }
  }
  if(nrsaphab.unpackFlag(flags,'riparianVegetation')) {
      rc <- metsRiparianVegetation()
      if(!is.null(rc)) {
          print(sprintf("Error calculating riparian vegetation mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'riparianVegetation')
      }
  }
  if(nrsaphab.unpackFlag(flags,'slopeBearing')) {
      rc <- metsSlopeBearing()
      if(!is.null(rc)) {
          print(sprintf("Error calculating slope & bearing mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'slopeBearing')
      }
  }
  if(nrsaphab.unpackFlag(flags,'substrateCharacterization')) {
      rc <- metsSubstrateCharacterization()
      if(!is.null(rc)) {
          print(sprintf("Error calculating substrate characterization mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'substrateCharacterization')
      }
  }
  if(nrsaphab.unpackFlag(flags,'substrateEmbeddedness')) {
      rc <- metsSubstrateEmbed()
      if(!is.null(rc)) {
          print(sprintf("Error calculating substrate embededness mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'substrateEmbeddedness')
      }
  }

  # Calculate residual pool metrics after slope & bearing are done.
  if(nrsaphab.unpackFlag(flags,'residualPools')) {
      rc <- metsResidualPools()
      if(!is.null(rc)) {
          print(sprintf("Error calculating residual pool mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'residualPools')
      }
  }
  
  # Calculate bed stability last, as it depends on the results of several other
  # metrics.
  if(nrsaphab.unpackFlag(flags,'bedStability')) {
      rc <- metsBedStability()
      if(!is.null(rc)) {
          print(sprintf("Error calculating bed stability mets: %s\n", rc))
          flags<-nrsaphab.resetFlag(flags,'bedStability')
      }
  }

  return(flags)
}


nrsaphab.assemble <- function(flags)
# Assembles the NRSA physical habitat metrics from the individual .csv files
# created by the metrics, as specified by the flags argument.
#
# ARGUMENTS:
# flags      boolean vector of several flags indicating whether to calculate
#            a specific group of metrics, or NULL to calculate all sets.
#
{
  if(is.null(flags) | tolower(flags)=='all') {
      # if nothing specified, assume that all available files will be assembled.
      flags <- nrsaphab.allFlags()
  }
  
  # Create a base dataframe in which to accumulate metrics results.
  nrsaMets <- NULL

  # Calculate the metrics as specified.  This is done in alphabetic order,
  # except when a calculation requires the previous calculation of other
  # metrics.
  if(nrsaphab.unpackFlag(flags,'bankMorphology')) {
      tt <- readNRSACalculationResults('metsBankMorphology.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading canopy densiometer mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'canopyDensiometer')) {
      tt <- readNRSACalculationResults('metsCanopyDensiometer.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading canopy densiometer mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelCharacters')) {
      tt <- readNRSACalculationResults('metsChannelChar.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading channel characteristics mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelHabitat')) {
      tt <- readNRSACalculationResults('metsChannelHabitat.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading channel habitat mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'channelMorphology')) {
      tt <- readNRSACalculationResults('metsChannelMorphology.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading channel morphology mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'fishCover')) {
      tt <- readNRSACalculationResults('metsFishCover.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading fish cover mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'general')) {
      tt <- readNRSACalculationResults('metsGeneral.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading general mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'humanInfluence')) {
      tt <- readNRSACalculationResults('metsHumanInfluence.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading human influence mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'invasiveSpecies')) {
      tt <- readNRSACalculationResults('metsInvasiveSpecies.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading invasive species mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'largeWoody')) {
      tt <- readNRSACalculationResults('metsLargeWoody.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading LWD mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'legacyTree')) {
      tt <- readNRSACalculationResults('metsLegacyTree.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading legacy tree mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'littoralDepth')) {
      tt <- readNRSACalculationResults('metsLittoralDepth.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading littoral depth mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'riparianVegetation')) {
      tt <- readNRSACalculationResults('metsRiparianVegetation.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading riparian vegetation mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'slopeBearing')) {
      tt <- readNRSACalculationResults('metsSlopeBearing.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading slope & bearing mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'substrateCharacterization')) {
      tt <- readNRSACalculationResults('metsSubstrateCharacterization.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading substrate characterization mets: %s\n", tt))
      }
  }
  if(nrsaphab.unpackFlag(flags,'substrateEmbeddedness')) {
      tt <- readNRSACalculationResults('metsSubstrateEmbed.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading bsubstrate embededness mets: %s\n", tt))
      }
  }

  if(nrsaphab.unpackFlag(flags,'residualPools')) {
      tt <- readNRSACalculationResults('metsResidualPools.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading residual pool mets: %s\n", tt))
      }
  }

  if(nrsaphab.unpackFlag(flags,'bedStability')) {
      tt <- readNRSACalculationResults('metsBedStability.csv')
      if(is.data.frame(tt)) {
          nrsaMets <- rbind(nrsaMets, tt)
      } else {
          print(sprintf("Error reading bed stability mets: %s\n", tt))
      }
  }

  # Write metrics results to csv file
  write.csv(nrsaMets, file.path(NRSACalcLocation, 'nrsaphab.csv')
           ,row.names=FALSE
           )

}

nrsaphab.dbUpdate <- function()
# Write metrics results to the server.  Insert the data the first time this
# is done, afterwards just update it.
{
  mets <- read.csv('L:/Priv/CORFiles/IM/Rwork/nrsa/results/nrsaphab.csv'
                  ,stringsAsFactors=FALSE
                  )
  mets <- rename(mets, c('UID','METRIC'), c('BATCHNO','PARAMETER'))
  mets$BATCHNO <- as.integer(mets$BATCHNO)
  mets$REASON <- ''

  nrsa <- odbcConnect('NRSA2')
  rc <- dbInsert(nrsa, 'tblPHABMET', mets, c('BATCHNO','PARAMETER'))
  if(!is.null(rc)) {
      print(sprintf("Error writing completed metrics to server: %s\n", rc))
  }
  odbcClose(nrsa)

}


nrsaphab.packFlags <- function(bankMorphology, bedStability, canopyDensiometer
                              ,channelCharacters, channelHabitat, channelMorphology
                              ,fishCover, general, humanInfluence, invasiveSpecies
                              ,largeWoody, legacyTree, littoralDepth, residualPools
                              ,riparianVegetation, slopeBearing, substrateCharacterization
                              ,substrateEmbeddedness
                              )
# Condenses individual boolean flags into vector of flags for easy passing
# between functions.
#
# ARGUMENTS:
# all arguments take on TRUE or FALSE values, depending on whether that
# particular set of metrics calculations is desired.
{
  flags <- c(bankMorphology             =bankMorphology
            ,bedStability               =bedStability
            ,canopyDensiometer          =canopyDensiometer
            ,channelCharacters          =channelCharacters
            ,channelHabitat             =channelHabitat
            ,channelMorphology          =channelMorphology
            ,fishCover                  =fishCover
            ,general                    =general
            ,humanInfluence             =humanInfluence
            ,invasiveSpecies            =invasiveSpecies
            ,largeWoody                 =largeWoody
            ,legacyTree                 =legacyTree
            ,littoralDepth              =littoralDepth
            ,residualPools              =residualPools
            ,riparianVegetation         =riparianVegetation
            ,slopeBearing               =slopeBearing
            ,substrateCharacterization  =substrateCharacterization
            ,substrateEmbeddedness      =substrateEmbeddedness
            )
  return(flags)
}

nrsaphab.allFlags <- function()
# Creates a vector of flags, all set to TRUE. Used to create a default case.
{
  flags <- c(bankMorphology             =TRUE
            ,bedStability               =TRUE
            ,canopyDensiometer          =TRUE
            ,channelCharacters          =TRUE
            ,channelHabitat             =TRUE
            ,channelMorphology          =TRUE
            ,fishCover                  =TRUE
            ,general                    =TRUE
            ,humanInfluence             =TRUE
            ,invasiveSpecies            =TRUE
            ,largeWoody                 =TRUE
            ,legacyTree                 =TRUE
            ,littoralDepth              =TRUE
            ,residualPools              =TRUE
            ,riparianVegetation         =TRUE
            ,slopeBearing               =TRUE
            ,substrateCharacterization  =TRUE
            ,substrateEmbeddedness      =TRUE
            )
  return(flags)
}

nrsaphab.unpackFlag <- function(flags, flag)
# Returns the value of a specific flag
#
# ARGUMENTS:
# flags     vector of flags
# flag      name of flag to 'unpack'
{
  return(flags[flag])
}


nrsaphab.resetFlag <- function(flags, flag)
# Sets a specified flag to false and returns the resulting vector.
#
# ARGUMENTS:
# flags     vector of flags
# flag      name of flag to set to FALSE
{
  flags[flag]<-FALSE
  return(flags)
}

# end of file