valStructChanCovTest <- function()
# Tests valStructChanCov()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CU','CD','CL','CR')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','UP','DN')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- 'DENSIOM'
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    remove transect by removing it from dataframe
  realTest <- subset(realTest, !(row.names(realTest) %in% c(201,301,401)))

  #    remove transect by substituting it with legal values
  realTest[c(02,102,502),]$TRANSECT <- paste('X'
                                          ,realTest[c(5,51,151),]$TRANSECT
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(103,503,903)
  is.na(realTest$TRANSECT) <- c(104,404,604)
  is.na(realTest$TRANSDIR) <- c(205,405,605)
  is.na(realTest$PARAMETER) <- c(106,206,306)

  #    unexpected values of keys
  realTest[c(08,308,508),]$TRANSECT <- 'L'
  realTest[c(109,509,609),]$TRANSDIR <- 'XX'           # always unexpected
  realTest[realTest$UID=='2001' &                      # stream values in a river
           realTest$TRANSECT=='A' &
           realTest$TRANSDIR %in% c('UP','DN')
          ,
          ]$TRANSDIR <- c('CU','CD')
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-TRANSECT-TRANSDIR
  realTest[c(113,413,713),]$TRANSECT <- ifelse(realTest[c(113,413,713),]$TRANSECT=='G'
                                              ,'H'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructChanCov(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )
             
  # Test real datafram to look for false negatives
  rr <- valStructChanCov(realTest, test='all')
  ee <- as.matrix(rbind(
                  "Column UID has 3 missing values"
                 ,"Column TRANSECT has 3 missing values"
                 ,"Column TRANSDIR has 3 missing values"
                 ,"Column PARAMETER has 3 missing values"
                 ,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1009,K)"
                 ,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1006,J)"
                 ,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1008,H)"
                 ,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1002,H)"
                 ,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1008,F)"
                 ,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1002,G)"
                 ,"Unexpected value TRANSDIR=(CU) at (UID,TRANSECT)=(2001,A)"
                 ,"Unexpected value TRANSDIR=(CD) at (UID,TRANSECT)=(2001,A)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1008)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1010)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(1006,G,CD), n=2"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(2006,G,LF), n=2"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(1003,G,RT), n=2"
                 ,"Absent TRANSDIR=(CD,CL) value at UID,TRANSECT=(1001,H) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1001,I) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1001,J) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(1001,L) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1002,A) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1002,G) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1002,H) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1002,J) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1002,K) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1002,XA) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1002,XF) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1003,A) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1003,K) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1004,K) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1004,NA) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1005,G) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(1005,XE) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1006,G) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1006,I) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1006,J) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1007,F) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1007,H) "
                 ,"Absent TRANSDIR=(LF,RT,CL,CU) value at UID,TRANSECT=(1007,NA) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1008,A) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1008,F) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1008,H) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1008,L) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1009,K) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1010,I) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1010,K) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CL,CR) value at UID,TRANSECT=(1010,L) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(NA,G) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(NA,K) "
                 ,"Absent TRANSDIR=(DN,UP) value at UID,TRANSECT=(2001,A) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(2003,E) "
                 ,"Absent TRANSDIR=(UP) value at UID,TRANSECT=(2006,C) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(2006,F) "
                 ,"Absent TRANSDIR=(DN,LF,RT) value at UID,TRANSECT=(NA,C) "
                 ,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1006,K,LF)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1001,A,RT)"
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1007,J,RT)"
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1008,I,CU)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1003,J,CU)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,H,CL)"
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1001,A,RT) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1003,J,CU) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1004,H,CL) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1006,K,LF) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1007,J,RT) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1008,I,CU) "
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1010,K,LF)"
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1002,J,CU)"
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(2003,E,RT)"
                 ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file