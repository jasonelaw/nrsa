valStructThalwegTest <- function()
# Tests valStructThalweg()
{
  # Create well formed data.  This table contains data from both wadeable
  # and boatable thalweg forms, as well as two items from the slope & bearing
  # portion of the channel/Riparian transect form.  As such, the construction
  # is somewhat complicated.  UIDs that are even will have 10 stations per
  # transect, UIDs that are odd will have 15 or 12, depending on whether they
  # are wadeable or boatable, respectively.  Even boatable UIDs will have depth
  # units in meters, odd UIDs in ft.
  baseWT <- expand.grid(UID = 1:10                # Wadeable thalweg
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:14
                       ,SAMPLE_TYPE = 'PHAB_THAL'
                       ,PARAMETER = c('BACKWATER', 'BAR_PRES', 'CHANUNCD'
                                     ,'DEPTH', 'INCREMNT', 'POOLFMCD'
                                     ,'SEDIMENT', 'SIDCHN', 'WETWIDTH'
                                     , 'BARWIDTH'
                                     )
                       )
  baseWT[baseWT$PARAMETER == 'BACKWATER', 'RESULT'] <- rep(c('Y','N')
                                                          ,length.out=10*10*15
                                                          )
  baseWT[baseWT$PARAMETER == 'BAR_PRES', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'CHANUNCD' &
         baseWT$UID %in% as.character(1:10)
        ,'RESULT'] <- rep(c('PP','PT','PL','PB','PD','GL','RI','RA','CA','FA','DR')
                         ,length.out=10*10*15
                         )
  baseWT[baseWT$PARAMETER == 'DEPTH', 'RESULT'] <- rep(12,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'INCREMNT', 'RESULT'] <- rep(2,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'POOLFMCD', 'RESULT'] <- rep(c('N','W','W'
                                                           ,'R','B','F'
                                                           )
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SEDIMENT', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SIDCHN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'WETWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'BARWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT <- subset(baseWT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

  baseBT <- expand.grid(UID=11:20                 # Boatable thalweg
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:11
                       ,SAMPLE_TYPE = 'PHAB_THAL'
                       ,PARAMETER = c('CHANUNCD', 'DEP_POLE', 'DEP_SONR'
                                     ,'OFF_CHAN', 'SIZE_CLS', 'SNAG'
                                     )
                       )
  baseBT[baseBT$PARAMETER == 'CHANUNCD', 'RESULT'] <- rep(c('PO','GL','RI','RA','CA','FA','DR')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_POLE', 'RESULT'] <- rep(c(17,NA)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_SONR', 'RESULT'] <- rep(c(NA,18)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'OFF_CHAN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'SIZE_CLS', 'RESULT'] <- rep(c('BH','BL','CP','GR'
                                                          ,'SA','FN','OT'
                                                          )
                                                          ,length.out=10*10*12
                                                          )
  baseBT[baseWT$PARAMETER == 'SNAG', 'RESULT'] <- rep(c('Y','N')
                                                     ,length.out=10*10*12
                                                     )
  baseBT <- subset(baseBT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

#  baseBC <- expand.grid(UID=11:20                 # Boatable chan form
#                       ,TRANSECT = LETTERS[1:10]
#                       ,STATION = 'NONE'
#                       ,SAMPLE_TYPE = 'PHAB_CHANF'
#                       ,PARAMETER = c('ACTRANSP', 'INTDTRAN')
#                       )
#  baseBC[baseBC$PARAMETER == 'ACTRANSP', 'RESULT'] <- rep(c(NA,190)
#                                                         ,length.out=10*10*1
#                                                         )
#  baseBC[baseBC$PARAMETER == 'INTDTRAN', 'RESULT'] <- rep(c(NA,200)
#                                                         ,length.out=10*10*1
#                                                         )

  baseTest <- rbind(baseWT, baseBT)#, baseBC)
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$STATION <- as.character(baseTest$STATION)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$FLAG <- as.character(NA)
  baseTest$UNITS <- as.character(NA)
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 == 0,]$UNITS <- 'm'
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 != 0,]$UNITS <- 'ft'



  # Create poorly formed data based on correct data
  realTest <- baseTest
  
  #    Make some values missing
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$UID <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$TRANSECT <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='C' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='D' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$SAMPLE_TYPE <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='E' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- ''

  #    Make some numeric values non-numeric
  realTest[realTest$UID=='10' & realTest$TRANSECT=='A' &
           realTest$STATION=='2' & realTest$PARAMETER=='WETWIDTH'
          ,]$RESULT <- 'BOB'
  realTest[realTest$UID=='10' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BARWIDTH'
          ,]$RESULT <- ' '
  realTest[realTest$UID=='10' & realTest$TRANSECT=='C' &
           realTest$STATION=='2' & realTest$PARAMETER=='INCREMNT'
          ,]$RESULT <- '.'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='D' &
           realTest$STATION=='2' & realTest$PARAMETER=='DEP_POLE'
          ,]$RESULT <- '$'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='E' &
           realTest$STATION=='2' & realTest$PARAMETER=='DEP_SONR'
          ,]$RESULT <- ','

  #    Make some values absent, which will also create duplication errors
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$TRANSECT <- 'C'
  realTest[realTest$UID=='3' & realTest$TRANSECT=='C' &
           realTest$STATION=='3' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- '4'
  realTest[realTest$UID=='4' & realTest$TRANSECT=='D' &
           realTest$STATION=='3' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- 'BAR_PRES'

  #    Make some unexpected values
  realTest[realTest$UID=='5' & realTest$TRANSECT=='E' &
           realTest$STATION=='5' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- 'DEP_POLE'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='E' &
           realTest$STATION=='5' & realTest$PARAMETER=='DEP_POLE'
          ,]$PARAMETER <- 'DEPTH'
  realTest[realTest$UID=='6' & realTest$TRANSECT=='F' &
           realTest$STATION=='6' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- '15'
  realTest[realTest$UID=='16' & realTest$TRANSECT=='F' &
           realTest$STATION=='6' & realTest$PARAMETER=='DEP_POLE'
          ,]$PARAMETER <- '15'

  
  # Check for false positives with good data
  rr <- valStructThalweg(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected errors where there are none"
             )
  
  # Check for false negatives with bad data
  rr <- valStructThalweg(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column STATION has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value STATION=() at (UID,TRANSECT)=(1,C)"
             ,"Unexpected value STATION=(15) at (UID,TRANSECT)=(6,F)"
             ,"Unexpected value TRANSECT=() at (UID)=(1)"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(2,C,2,BACKWATER), n=2"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(3,C,4,BACKWATER), n=2"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(4,D,3,BAR_PRES), n=2"
             ,"Column RESULT has 6 non-numeric values"
             ,"Absent STATION=(0,1,3,4,5,6,7,8,9) value at UID,TRANSECT=(,A) "
             ,"Absent STATION=(0,1,3,4,5,6,7,8,9) value at UID,TRANSECT=(1,) "
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J) value at UID=() "
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT,STATION)=(1,E,2)"
             ,"Unexpected value PARAMETER=(15) at (UID,TRANSECT,STATION)=(16,F,6)"
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,STATION)=(1,D,2)"
             )
  checkEquals(ee, rr
             ,"Error: Did not properly detect errors where they exist"
             )

}

# end of file