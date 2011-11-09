valStructLittoralTest <- function()
# Tests valStructLittoral()
{
  # Create example of perfect data
  baseTest <- expand.grid(UID=1:10
                         ,TRANSECT=LETTERS[1:11]
                         ,PARAMETER=c('BOTTOMDOM', 'BOTTOMSEC', 'SHOREDOM'
                                     ,'SHORESEC', 'SUBOBS'
                                     )
                         )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE <- 'PHAB_CHANBFRONT'
  baseTest[baseTest$PARAMETER == 'SUBOBS','RESULT'] <- rep(c('JUDG','OBSV')
                                                          ,length.out=10*11
                                                          )
  baseTest[baseTest$PARAMETER != 'SUBOBS','RESULT'] <- rep(c('CB','FN','GC','GF'
                                                          ,'HP','OT','RR','RS'
                                                          ,'SA','SB','WD','XB'
                                                          )
                                                          ,length.out=10*11*4
                                                          )
  baseTest$FLAG <- as.character(NA)

  # Create imperfectly structured data based on perfect data.
  realTest <- baseTest
  
  #    Make some values missing, which will cause some absences and unexpected
  #    values appear as well
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A',][1,]$UID <- ''
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B',][1,]$TRANSECT <- ''
  realTest[realTest$UID=='3' & realTest$TRANSECT=='C',][1,]$PARAMETER <- ''
  realTest[realTest$UID=='4' & realTest$TRANSECT=='D',][1,]$SAMPLE_TYPE <- ''

  #    Make some values absent, which will cause some duplicates to appear too
  realTest[realTest$UID=='5' & realTest$TRANSECT=='E' &
           realTest$PARAMETER=='BOTTOMDOM',]$TRANSECT <- 'D'
  realTest[realTest$UID=='6' & realTest$TRANSECT=='F' &
           realTest$PARAMETER=='BOTTOMDOM',]$PARAMETER <- 'SHOREDOM'

  #    Make some unexpected values
  realTest[realTest$UID=='7' & realTest$TRANSECT=='G' &
           realTest$PARAMETER=='BOTTOMDOM',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='8' & realTest$TRANSECT=='H' &
           realTest$PARAMETER=='BOTTOMDOM',]$TRANSECT <- 'XH'
  realTest[realTest$UID=='9' & realTest$TRANSECT=='I' &
           realTest$PARAMETER=='BOTTOMDOM',]$SAMPLE_TYPE <- 'ODD TYPE'

  
  # Check for false positives with perfect data
  rr <- valStructLittoral(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected errors where there are none"
             )
  
  
  # Check for false negatives with imperfect data
  rr <- valStructLittoral(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value TRANSECT=() at (UID)=(2)"
             ,"Unexpected value TRANSECT=(XH) at (UID)=(8)"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(5,D,BOTTOMDOM), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(6,F,SHOREDOM), n=2"
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT)=(3,C)"
             ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(7,G)"
             ,"Absent PARAMETER=(BOTTOMSEC,SHOREDOM,SHORESEC,SUBOBS) value at UID,TRANSECT=(2,) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(2,B) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(3,C) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(5,E) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(6,F) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(7,G) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(8,H) "
             ,"Absent PARAMETER=(BOTTOMSEC,SHOREDOM,SHORESEC,SUBOBS) value at UID,TRANSECT=(8,XH) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,PARAMETER)=(4,D,BOTTOMDOM)"
             ,"Unexpected value SAMPLE_TYPE=(ODD TYPE) at (UID,TRANSECT,PARAMETER)=(9,I,BOTTOMDOM)"
             )
  checkEquals(ee, rr
             ,"Error: Detected errors where there are none"
             )


}

# end of file