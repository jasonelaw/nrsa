valStructBankGeometryTest <- function()
# Tests valStructBankGeometry()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('ANGLE','UNDERCUT')
                               )
                   ,expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR='NONE'
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('BANKHGT','BANKWID',
                                    'BARWID','INCISHGT','WETWID')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('NONE')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               ,PARAMETER=c('CONSTRT','SEEOVRBK','SHOR2RIP')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR='NONE'
                               ,SAMPLE_TYPE='PHAB_CHANBFRONT'
                               ,PARAMETER=c('ANGLE','BANKHT','BANKWID',
                                    'BARWID','INCISED','WETWID')
                                )
                     )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$UNITS <- 'NONE'
  baseTest$FLAG <- ''


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    make key values missing
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='LF' & realTest$PARAMETER=='ANGLE'
          ,]$UID <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$TRANSECT <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='C' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$TRANSDIR <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='D' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$PARAMETER <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='E' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$SAMPLE_TYPE <- NA

  #    Create unexpected values of keys
  realTest[realTest$UID=='1003' & realTest$TRANSECT=='A' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKHGT'
          ,]$TRANSECT <- 'L'
  realTest[realTest$UID=='1003' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKHGT'
          ,]$TRANSDIR <- 'AB'

          
  realTest[realTest$UID=='2009' &                     # stream values in a river
           realTest$TRANSECT=='A' &
           realTest$TRANSDIR %in% c('NONE')
          ,
          ]$TRANSDIR <- 'RT'
          
  realTest[realTest$UID=='1009' &  !is.na(realTest$UID) & # river values in a stream
           realTest$TRANSECT=='B' &
           realTest$TRANSDIR %in% c('LF')
          ,
          ]$TRANSDIR <- 'NONE'
          
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='C' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='UNDERCUT'
          ,]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='D' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKWID'
          ,]$PARAMETER <- 'WRONG'

  #    Create non-unique values of combinations of UID-TRANSECT-TRANSDIR-PARAMETER
  realTest$TRANSECT <- ifelse(realTest$UID==2008 & realTest$TRANSECT=='G' &
                              realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='CONSTRT'
                             ,'J'
                             ,realTest$TRANSECT
                             )
  
  #    Create absent transects by removing them from dataframe
  realTest <- subset(realTest
                    ,!(UID=='1004' & TRANSECT=='A' & TRANSDIR=='RT' &
                       PARAMETER=='ANGLE'
                      )
                    )

  #    Create absent transects by substituting them with legal values
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKWID'
          ,]$TRANSECT <- 'XB'

  #  Create absent TRANSDIR by removing them from the dataframe
  realTest <- subset(realTest, !(UID==1005 & TRANSECT=='F' & TRANSDIR=='RT'))
  realTest <- subset(realTest, !(UID==1005 & TRANSECT=='H' & TRANSDIR=='NONE'))
  realTest <- subset(realTest, !(UID==2005 & TRANSECT=='K' & TRANSDIR=='NONE'))
  realTest <- subset(realTest, !(UID==2005 & TRANSECT=='I' & TRANSDIR=='NONE'))

  #  Create non-numeric data
  realTest$RESULT <- ifelse(realTest$UID==1001 & realTest$TRANSECT=='A' 
                      & realTest$TRANSDIR=='LF' & realTest$PARAMETER=='ANGLE'
                      , '.', realTest$RESULT)
 
  # Test perfect dataframe to look for false positives
  rr <- valStructBankGeometry(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect bank geometry dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructBankGeometry(realTest, test='all')
  ee <- as.matrix(rbind(
      "Column UID has 1 missing values"
     ,"Column TRANSECT has 1 missing values"
     ,"Column TRANSDIR has 1 missing values"
     ,"Column PARAMETER has 1 missing values"
     ,"Column SAMPLE_TYPE has 1 missing values"
     ,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1002,C)"
     ,"Unexpected value TRANSDIR=(AB) at (UID,TRANSECT)=(1003,B)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
     ,"Unexpected value TRANSECT=(L) at (UID)=(1003)"
     ,"Unexpected row count at (UID,TRANSECT,TRANSDIR,PARAMETER)=(2008,J,NONE,CONSTRT), n=2"
     ,"Column RESULT has 1 non-numeric values"
     ,"Absent TRANSDIR=(LF,NONE) value at UID,TRANSECT=(1001,NA) "
     ,"Absent TRANSDIR=(LF,RT) value at UID,TRANSECT=(1003,L) "
     ,"Absent TRANSDIR=(LF,RT) value at UID,TRANSECT=(1004,XB) "
     ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1005,F) "
     ,"Absent TRANSDIR=(NONE) value at UID,TRANSECT=(1005,H) "
     ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1009,B) "
     ,"Absent TRANSDIR=(RT,NONE) value at UID,TRANSECT=(NA,B) "
     ,"Absent TRANSDIR=(NONE) value at UID,TRANSECT=(2009,A) "
     ,"Absent TRANSECT=(I,K) value at UID=(2005) "
     ,"Absent TRANSECT=(A,C,D,E,F,G,H,I,J,K) value at UID=(NA) "
     ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1002,D,RT)"
     ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,D,NONE)"
     ,"Absent PARAMETER=(BANKHGT) value at UID,TRANSECT=(1003,A) "
     ,"Absent PARAMETER=(BANKHGT) value at UID,TRANSECT=(1003,B) "
     ,"Absent PARAMETER=(BANKWID,BARWID,INCISHGT,WETWID) value at UID,TRANSECT=(1003,L) "
     ,"Absent PARAMETER=(BANKWID) value at UID,TRANSECT=(1004,B) "
     ,"Absent PARAMETER=(BANKWID) value at UID,TRANSECT=(1004,D) "
     ,"Absent PARAMETER=(BANKHGT,BARWID,INCISHGT,WETWID) value at UID,TRANSECT=(1004,XB) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1001,B,LF) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1001,B,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,C,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,D,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,E,RT) "
     ,"Absent PARAMETER=(ANGLE,UNDERCUT) value at UID,TRANSECT,TRANSDIR=(1003,B,AB) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1004,A,RT) "
     ,"Absent PARAMETER=(UNDERCUT) value at UID,TRANSECT,TRANSDIR=(1004,C,RT) "
     ,"Absent PARAMETER=(UNDERCUT) value at UID,TRANSECT,TRANSDIR=(NA,B,LF) "
     ,"Absent PARAMETER=(CONSTRT) value at UID,TRANSECT=(2008,G) "
     ,"Unexpected value SAMPLE_TYPE=(NA) at (UID,TRANSECT,TRANSDIR)=(1002,E,RT)"
     ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,C,RT)"
     ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# e