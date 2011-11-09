valStructDischargeTest <- function ()
# Tests valStructDischarge
{
  # Create dataframe with no errors.  Since different discharge measurement
  # methods use different parameters and data structures, these data are built
  # up separately and then combined:
  #   NBO:           UID run from 1 to 10
  #   QVAL:          UID run from 11 to 20
  #   TIMED FILLING: UID run from 21 to 30
  #   VELOCITY AREA: UID run from 31 to 40, and will have LINE range from 1 to
  #                    UID - 20, where UID is 31-40, as allowed in the protocol.
  #
  # Even UID use metric, odd UID use imperial dimensions when allowed.
  #
  baseNBO <- expand.grid(UID = 1:10
                        ,REP = 1:3
                        ,PARAMETER = c('AVGWIDTH','DEPTH_1','DEPTH_2','DEPTH_3'
                                       ,'DEPTH_4','DEPTH_5','FLOAT','TOTTIME'
                                       )
                        )
  attr(baseNBO,'out.attrs') <- NULL
  baseNBO$UID <- as.character(baseNBO$UID)
  baseNBO$PARAMETER <- as.character(baseNBO$PARAMETER)
  baseNBO$LINE <- as.integer(999)
  baseNBO$METHOD <- 'NBO'
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH','RESULT'] <- 5
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH','RESULT'] <- 1
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='FLOAT','RESULT'] <- 20
  baseNBO[baseNBO$PARAMETER == 'FLOAT' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[baseNBO$PARAMETER == 'FLOAT' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[baseNBO$PARAMETER =='TOTTIME','RESULT'] <- 60
  baseNBO[baseNBO$PARAMETER =='TOTTIME','UNITS'] <- 'SEC'
  baseNBO$FLAG <- as.character(NA)
  attr(baseNBO,'out.attrs') <- NULL
  baseNBO <- baseNBO[c(1,2,4,5,3,6,7,8)]
  
  baseQVAL <- data.frame(UID = as.character(11:20), stringsAsFactors=FALSE)
  baseQVAL$REP <- as.integer(99)
  baseQVAL$LINE <- as.integer(999)
  baseQVAL$METHOD <- 'QVAL'
  baseQVAL$PARAMETER <- 'QVAL'
  baseQVAL$RESULT <-  3
  baseQVAL$UNITS <- 'cfs'
  baseQVAL$FLAG <- as.character(NA)
  
  baseTF <- expand.grid(UID = 21:30
                       ,REP = 1:5
                       ,PARAMETER = c('TIME','VOLUME')
                       )
  attr(baseTF,'out.attrs') <- NULL
  baseTF$UID <- as.character(baseTF$UID)
  baseTF$PARAMETER <- as.character(baseTF$PARAMETER)
  baseTF$LINE <- as.integer(999)
  baseTF$METHOD <- 'TIMED FILLING'
  baseTF[baseTF$PARAMETER == 'TIME', 'RESULT'] <- 2
  baseTF[baseTF$PARAMETER == 'TIME', 'UNITS'] <- 'SEC'
  baseTF[baseTF$PARAMETER == 'TIME', 'RESULT'] <- 1
  baseTF[baseTF$PARAMETER == 'TIME', 'UNITS'] <- 'L'
  baseTF$FLAG <- as.character(NA)
  baseTF <- baseTF[c(1,2,4,5,3,6,7,8)]

  baseVA <- expand.grid(UID = 31:40
                       ,LINE = 1:20
                       ,PARAMETER = c('DEPTH','DISTBANK','VELOCITY')
                       )
  attr(baseVA,'out.attrs') <- NULL
  baseVA$UID <- as.character(baseVA$UID)
  baseVA$PARAMETER <- as.character(baseVA$PARAMETER)
  baseVA$REP <- as.integer(99)
  baseVA$METHOD <- 'VELOCITY AREA'
  baseVA[baseVA$PARAMETER == 'DEPTH','RESULT'] <- 5
  baseVA[baseVA$PARAMETER == 'DEPTH' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseVA[baseVA$PARAMETER == 'DEPTH' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseVA[baseVA$PARAMETER == 'DISTBANK','RESULT'] <- 20
  baseVA[baseVA$PARAMETER == 'DISTBANK' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseVA[baseVA$PARAMETER == 'DISTBANK' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseVA[baseVA$PARAMETER == 'VELOCITY','RESULT'] <- 0.4
  baseVA[baseVA$PARAMETER == 'VELOCITY' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m/s'
  baseVA[baseVA$PARAMETER == 'VELOCITY' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft/s'
  baseVA$FLAG <- as.character(NA)
  baseVA <- subset(baseVA, LINE <= as.integer(UID) - 20)
  baseVA <- baseVA[c(1,4,2,5,3,6,7,8)]

  baseTest <- rbind(baseNBO, baseQVAL, baseTF, baseVA)
  baseTest$SAMPLE_TYPE <- 'FLOW'


  # Create dataframe with structural problems based on correct data
  realTest <- baseTest
  
  #    Make some keys missing, which will also turn up as unexpected and absent
  realTest[1,]$UID <- ''
  realTest[2,]$REP <- ''
  realTest[3,]$LINE <- ''
  realTest[4,]$PARAMETER <- ''
  realTest[5,]$SAMPLE_TYPE <- ''
  realTest[6,]$METHOD <- ''

  #    Make some unexpected keys, which will make some absent as well.
  realTest[realTest$METHOD == 'NBO',][1,]$PARAMETER <- 'DEPTH'
  realTest[realTest$METHOD == 'QVAL',][1,]$PARAMETER <- 'AVGWIDTH'
  realTest[realTest$METHOD == 'TIMED FILLING',][1,]$PARAMETER <- 'TOTTIME'
  realTest[realTest$METHOD == 'VELOCITY AREA',][1,]$PARAMETER <- 'DEPTH_1'

  realTest[realTest$METHOD == 'NBO',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'QVAL',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'TIMED FILLING',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'VELOCITY AREA',][1,]$LINE <- -21

  realTest[100,]$SAMPLE_TYPE <- 'WRONG'
  realTest[101,]$METHOD <- 'GUESSING'

  #    Make some keys absent, which will also turn up as duplicates
  realTest[realTest$METHOD == 'NBO' &
           realTest$UID == 3 &
           realTest$REP == 1 &
           realTest$PARAMETER == 'DEPTH_5'
          ,]$REP <- 2
  realTest[realTest$METHOD == 'QVAL' &
           realTest$UID == 13
          ,]$UID <- 12
  realTest[realTest$METHOD == 'TIMED FILLING' &
           realTest$UID == 23 &
           realTest$REP == 1 &
           realTest$PARAMETER == 'VOLUME'
          ,]$REP <- 2
  realTest[realTest$METHOD == 'VELOCITY AREA' &
           realTest$UID == 33 &
           realTest$LINE == 3 &
           realTest$PARAMETER == 'VELOCITY'
          ,]$LINE <- 2


  # Test correct data to look for false positives
  rr <- valStructDischarge(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Structure check of correct data turned up errors"
             )

  # Test incorrect data to look for false negatives
  rr <- valStructDischarge(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column REP has 1 missing values"
             ,"Column LINE has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Column METHOD has 1 missing values"
             ,"Unexpected value REP=() at (UID)=(2)"
             ,"Unexpected value METHOD=() at (UID)=(6)"
             ,"Unexpected value METHOD=(GUESSING) at (UID)=(1)"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(3,2,999,DEPTH_5), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(12,99,999,QVAL), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(33,99,2,VELOCITY), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(23,2,999,VOLUME), n=2"
             ,"Absent REP=(2,3,4,5) value at UID,LINE=(21,-1) "
             ,"Absent REP=(2,3) value at UID,LINE=(,-1) "
             ,"Absent REP=(2,3) value at UID,LINE=(3,) "
             ,"Unexpected value PARAMETER=(DEPTH) at (UID,REP)=(,1)"
             ,"Unexpected value PARAMETER=() at (UID,REP)=(4,1)"
             ,"Unexpected value PARAMETER=(AVGWIDTH) at (UID)=(11)"
             ,"Unexpected value PARAMETER=(TOTTIME) at (UID,REP,LINE)=(21,1,-1)"
             ,"Unexpected value PARAMETER=(DEPTH_1) at (UID,REP,LINE)=(31,99,-21)"
             ,"Absent PARAMETER=(DEPTH) value at UID,REP,LINE=(31,99,-21) "
             ,"Absent PARAMETER=(DEPTH) value at UID,REP,LINE=(31,99,1) "
             ,"Absent PARAMETER=(VOLUME,TIME) value at UID,REP,LINE=(21,1,-1) "
             ,"Absent PARAMETER=(AVGWIDTH,DEPTH_1,DEPTH_2,DEPTH_3,DEPTH_4,DEPTH_5,FLOAT,TOTTIME) value at UID,REP,LINE=(,1,-1) "
             ,"Absent PARAMETER=(DEPTH_1,DEPTH_2,DEPTH_3,DEPTH_4,DEPTH_5,FLOAT,TOTTIME) value at UID,REP,LINE=(3,1,) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,REP,LINE)=(5,1,999)"
             ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,REP,LINE)=(10,1,999)"
             )
  checkEquals(ee, rr
             ,"Error: Structure check of correct data turned up errors"
             )
}

# end of file