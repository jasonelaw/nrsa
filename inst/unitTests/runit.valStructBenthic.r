valStructBenthicTest <- function()
# Tests valStructBenthic()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BELGB'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                    ,expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BELGW'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BERWW'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  

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
  is.na(realTest$PARAMETER) <- c(106,206,306)

  #    unexpected values of keys
  realTest[c(08,308,508),]$TRANSECT <- 'L'
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'BEN_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-TRANSECT-PARAMETER
  realTest[c(113,413,713),]$TRANSECT <- ifelse(realTest[c(113,413,713),]$TRANSECT=='G'
                                              ,'H'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructBenthic(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect benthic dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructBenthic(realTest, test='all')
  ee <- as.matrix(rbind(
           "Column UID has 3 missing values"                                                                   
,"Column TRANSECT has 3 missing values"                                                              
,"Column PARAMETER has 3 missing values"                                                             
,"Unexpected value TRANSECT=(XA) at (UID)=(1002)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1008)"                                                     
,"Unexpected value TRANSECT=(XF) at (UID)=(1002)"                                                    
,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1010)"                                                     
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                                                    
,"Unexpected value TRANSECT=(XE) at (UID)=(1005)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1001)"                                                     
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                                                    
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,K)"                                        
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,A)"                                     
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1007,J)"                                        
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1008,I)"                                        
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1003,J)"                                     
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1004,H)"                                     
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1010,K)"                               
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1002,J)"                               
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1003,E)"                               
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1006,G,CHANNEL,BELGB), n=2"           
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1003,G,SECOND_HABITAT,BELGB), n=2"    
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1006,G,HABITAT,BELGW), n=2"           
,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "                                              
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(1001,L) "   
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1002,XA) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1002,XF) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1004,NA) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(1005,XE) "  
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,SUBSTRATE,HABITAT) value at UID,TRANSECT=(1007,NA) "     
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1008,L) " 
,"Absent PARAMETER=(SECOND_HABITAT,CHANNEL,SUBSTRATE,EDGE,HABITAT) value at UID,TRANSECT=(1010,L) "  
,"Absent PARAMETER=(SECOND_HABITAT,CHANNEL,SUBSTRATE,EDGE,HABITAT) value at UID,TRANSECT=(NA,C) "    
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(NA,G) "     
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(NA,K) "   

            
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Benthic data'
             )

}

# end of file
