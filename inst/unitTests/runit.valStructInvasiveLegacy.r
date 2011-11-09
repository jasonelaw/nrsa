valStructInvasiveLegacyTest <- function()
# Tests valStructInvasiveLegacy()
{
  # Create well structured dataframe.  Start with all possible parameters
  # and then prune them back. Sites 1,2 have no invasives and no legacy trees;
  # sites 3,4,5 have invasives and no legacy trees; sites 6,7,8 have legacy
  # trees and no invasives; sites 9,10 have invasives and legacy trees.
  baseData <- expand.grid(UID = as.character(1:10)
                         ,TRANSECT = LETTERS[1:11]
                         ,PARAMETER = c("DBH", "E_WTRMILF", "FLWR_RUSH", "G_REED"
                                       ,"HEIGHT", "HYDRILLA", "MF_ROSE"
                                       ,"NO_INVASIVES", "NOT_VIS", "P_LSTRIFE"
                                       ,"SALT_CED", "SPECIES", "SPURGE", "TREE_TYP"
                                       ,"W_HYACINTH", "YLW_FLHEAR"
                                       )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$UNITS <- 'NONE'
  
  
  baseData <- subset(baseData,  (UID %in% c('1','2') &
                                 PARAMETER %in% c("NO_INVASIVES", "NOT_VIS")
                                 )
                                 |
                                 (UID %in% c('3','4','5') &
                                  PARAMETER %in% c("NOT_VIS"
                                                    ,"E_WTRMILF", "FLWR_RUSH"
                                                    ,"G_REED", "HYDRILLA"
                                                    ,"MF_ROSE", "P_LSTRIFE"
                                                    ,"SALT_CED", "SPURGE"
                                                    ,"W_HYACINTH", "YLW_FLHEAR"
                                                    )
                                 )
                                 |
                                 (UID %in% c('6','7','8') &
                                  PARAMETER %in% c("NO_INVASIVES", "DBH"
                                                    ,"HEIGHT", "SPECIES"
                                                    ,"TREE_TYP"
                                                    )
                                 )
                                 |
                                 (UID %in% c('9','10') &
                                  PARAMETER %in% c("E_WTRMILF", "FLWR_RUSH"
                                                  ,"G_REED", "HYDRILLA"
                                                  ,"MF_ROSE", "P_LSTRIFE"
                                                  ,"SALT_CED", "SPURGE"
                                                  ,"W_HYACINTH", "YLW_FLHEAR"
                                                  ,"DBH","HEIGHT", "SPECIES"
                                                  ,"TREE_TYP"
                                                  )
                                 )
                    )
  
  baseData$SAMPLE_TYPE <- 'RIPLEG'
  baseData$RESULT <- ''
  baseData[baseData$PARAMETER=='DBH',]$RESULT <-
    rep(c('0-0.1','.1-.3','.3-.75','.75-2','>2','')
       ,length.out=nrow(baseData[baseData$PARAMETER=='DBH',])
       )
  baseData[baseData$PARAMETER=='E_WTRMILF',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='E_WTRMILF',]))
  baseData[baseData$PARAMETER=='FLWR_RUSH',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='FLWR_RUSH',]))
  baseData[baseData$PARAMETER=='G_REED',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='G_REED',]))
  baseData[baseData$PARAMETER=='HEIGHT',]$RESULT <-
    rep(c('<5','5-15','15-30','>30','')
       ,length.out=nrow(baseData[baseData$PARAMETER=='HEIGHT',])
       )
  baseData[baseData$PARAMETER=='HYDRILLA',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='HYDRILLA',]))
  baseData[baseData$PARAMETER=='MF_ROSE',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='MF_ROSE',]))
  baseData[baseData$PARAMETER=='NO_INVASIVES',]$RESULT <-
    rep(c('X','')
       ,length.out=nrow(baseData[baseData$PARAMETER=='NO_INVASIVES',])
       )
  baseData[baseData$PARAMETER=='NOT_VIS',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='NOT_VIS',]))
  baseData[baseData$PARAMETER=='P_LSTRIFE',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='P_LSTRIFE',]))
  baseData[baseData$PARAMETER=='SALT_CED',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='SALT_CED',]))
  baseData[baseData$PARAMETER=='SPECIES',]$RESULT <-
    rep(c('ALDER','ASH','ASPEN','BIRCH','')
       ,length.out=nrow(baseData[baseData$PARAMETER=='SPECIES',])
       )
  baseData[baseData$PARAMETER=='SPURGE',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='SPURGE',]))
  baseData[baseData$PARAMETER=='TREE_TYP',]$RESULT <-
    rep(c('Broadleaf Evergreen','Coniferous','Deciduous','')
       ,length.out=nrow(baseData[baseData$PARAMETER=='TREE_TYP',])
       )
  baseData[baseData$PARAMETER=='W_HYACINTH',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='W_HYACINTH',]))
  baseData[baseData$PARAMETER=='YLW_FLHEAR',]$RESULT <-
    rep(c('X',''), length.out=nrow(baseData[baseData$PARAMETER=='YLW_FLHEAR',]))
  baseData$FLAG <- as.character(NA)
  
  
  # Make real data with problems:
  #   missing UID, TRANSECT, PARAMETER values
  #   unexpected values of TRANSECT and PARAMETER
  #   duplicate PARAMETER values within a TRANSECT
  #   unexpected PARAMETER value
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='B' & realData$PARAMETER=='NOT_VIS',]$UID <- NA
  realData[realData$UID=='1' & realData$TRANSECT=='C' & realData$PARAMETER=='NOT_VIS',]$TRANSECT <- NA
  realData[realData$UID=='2' & realData$TRANSECT=='D' & realData$PARAMETER=='NOT_VIS',]$PARAMETER <- NA
  realData[realData$UID=='3' & realData$TRANSECT=='B' & realData$PARAMETER=='E_WTRMILF',]$TRANSECT <- 'XB'
  realData[realData$UID=='3' & realData$TRANSECT=='C' & realData$PARAMETER=='E_WTRMILF',]$PARAMETER <- 'FLWR_RUSH'
  realData[realData$UID=='5' & realData$TRANSECT=='B' & realData$PARAMETER=='E_WTRMILF',]$TRANSECT <- 'A'
  realData[realData$UID=='5' & realData$TRANSECT=='C' & realData$PARAMETER=='E_WTRMILF',]$PARAMETER <- 'ASPEN'


  # Look for false positive results
  rr <- valStructInvasiveLegacy(baseData, test='all')
  checkEquals(NULL, rr, "Error: Errors detected when there are none")


  # Check if problems are detected
  rr <- valStructInvasiveLegacy(realData, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Unexpected value PARAMETER=(ASPEN) at (UID,TRANSECT)=(5,C)"
             ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(2,D)"
             )
  checkEquals(ee, rr, "Error: Errors detected when there are none")

}

# end of file