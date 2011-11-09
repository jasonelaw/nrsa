valStructChanDepthTest <- function()
# Tests valStructChanDepth()
{
  # Create test data frame, odd UIDs done by pole, even UIDs done by sonar
  baseTest <- rbind(expand.grid(UID=as.character(1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,LINE=1:5
                               ,PARAMETER=c('POLE','SONAR')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE='PHAB_CHANBFRONT'
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 10))
  baseTest[(baseTest$PARAMETER == 'POLE' &
            as.integer(baseTest$UID) %% 2 == 0
           ) |
           (baseTest$PARAMETER == 'SONAR' &
            as.integer(baseTest$UID) %% 2 == 1
           )
          ,]$RESULT <- NA

  baseTest$UNITS <- 'm'
  baseTest$FLAG <- as.character(NA)
  attr(baseTest,'out.attrs') <- NULL  # get rid of odd attributes

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    Change some keys to NA, and make unexpected transect, parameter and
  #    sample_type values.
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A',][1,]$UID <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='B',][1,]$TRANSECT <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='C',][1,]$LINE <- NA
  realTest[realTest$UID=='1' & realTest$TRANSECT=='D',][1,]$PARAMETER <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='E',][1,]$SAMPLE_TYPE <- ''

  #    Create some unexpected values, and absent transect & parameter values
  realTest[realTest$UID=='2' & realTest$TRANSECT=='A',][1,]$TRANSECT <- 'XA'
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B',][1,]$LINE <- 12
  realTest[realTest$UID=='2' & realTest$TRANSECT=='C',][1,]$SAMPLE_TYPE <- 'PHAB'
  realTest[realTest$UID=='2' & realTest$TRANSECT=='D',][1,]$PARAMETER <- 'GUESS'

  #    Create duplicate rows, by one by duplicating a key, and by recording
  #    depths using more than one method i.e. PARAMETER.
  realTest[realTest$UID=='3' & realTest$TRANSECT=='A' & realTest$LINE==1
          ,]$TRANSECT <- 'K'
  realTest[realTest$UID=='3' & realTest$TRANSECT=='B' &
           realTest$LINE==1 & realTest$PARAMETER=='SONAR'
          ,]$RESULT <- 5
  realTest[realTest$UID=='4' & realTest$TRANSECT=='B' &
           realTest$LINE==1 & realTest$PARAMETER=='POLE'
          ,]$RESULT <- 5


  # Test perfect dataframe to look for false positives
  rr <- valStructChanDepth(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )

  # Test real dataframe to look for false negatives
  rr <- valStructChanDepth(realTest, test='all')
  ee <- as.matrix(rbind(
            "Column UID has 1 missing values"
           ,"Column TRANSECT has 1 missing values"
           ,"Column LINE has 1 missing values"
           ,"Column PARAMETER has 1 missing values"
           ,"Column SAMPLE_TYPE has 1 missing values"
           ,"Unexpected value TRANSECT=(XA) at (UID)=(2)"
           ,"Unexpected value TRANSECT=() at (UID)=(1)"
           ,"Unexpected value LINE=(12) at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(2,B,PHAB_CHANBFRONT,POLE)"
           ,"Unexpected value LINE=(NA) at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1,C,PHAB_CHANBFRONT,POLE)"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(3,B,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(4,B,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(3,K,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected value SAMPLE_TYPE=(PHAB) at (UID,TRANSECT,LINE,PARAMETER)=(2,C,1,POLE)"
           ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,LINE,PARAMETER)=(1,E,1,POLE)"
           ,"Unexpected value PARAMETER=() at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(1,D,PHAB_CHANBFRONT,1)"
           ,"Unexpected value PARAMETER=(GUESS) at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(2,D,PHAB_CHANBFRONT,1)"
           ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(,A) "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(1,) "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(2,XA) "
           ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file