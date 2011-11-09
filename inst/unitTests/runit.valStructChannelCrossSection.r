valStructChannelCrossSectionTest <- function()
# Tests valStructChannelCrossSection()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CT','RC','LC')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CT','RC','LC', 'NONE')
                               ,SAMPLE_TYPE='PHAB_THALW'
                               ,PARAMETER=c('XSIZE_CLS', 'SUB_5_7')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
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
  realTest[c(109,509,609),]$TRANSDIR <- 'XX' 
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'SUB_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  

  # Test perfect dataframe to look for false positives
  rr <- valStructChannelCrossSection(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect ChannelCrossSection dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructChannelCrossSection(realTest, test='all')
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
,"Unexpected value TRANSECT=(L) at (UID)=(1008)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                            
,"Unexpected value TRANSECT=(L) at (UID)=(1010)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                            
,"Unexpected value TRANSECT=(L) at (UID)=(1001)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                            
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,K)"                
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,A)"             
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1007,J)"                
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1008,I)"                
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1003,J)"             
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1004,H)"             
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1010,K)"       
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1002,J)"       
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1003,E)"       
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(1001,L) "             
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1002,XA) "            
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1002,XF) "            
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1004,NA) "            
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(1005,XE) "            
,"Absent TRANSDIR=(RT,CT,LC) value at UID,TRANSECT=(1007,NA) "               
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1008,L) "             
,"Absent TRANSDIR=(LF,RT,RC,LC) value at UID,TRANSECT=(1010,L) "             
,"Absent TRANSDIR=(LF,RT,CT,LC) value at UID,TRANSECT=(NA,C) "               
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(NA,G) "               
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(NA,K) "               
,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "                      
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1001,L) " 
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1002,XA) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1002,XF) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1004,NA) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1005,XE) "
,"Absent PARAMETER=(DEPTH,DIST_LB) value at UID,TRANSECT=(1007,NA) "         
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1008,L) " 
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1010,L) " 
,"Absent PARAMETER=(EMBED,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,C) "      
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,G) "   
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,K) "   

            
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect ChannelCrossSection data'
             )

}

# end of file
