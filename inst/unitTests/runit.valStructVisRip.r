valStructVisRipTest <- function()
# Tests valStructVisRip()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:2)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT')
                               ,SAMPLE_TYPE=c('PHAB_CHANW', 'PHAB_CHANB')
                               ,PARAMETER=c('BARE', 'BUILD', 'CANBTRE', 'CANSTRE'
                                 ,'CANVEG', 'GCNWDY', 'GCWDY', 'LANDFL'
                                 ,'LOG', 'MINE','PARK','PAST','PAVE','PIPES'
                                 ,'ROAD','ROW','UNDERVEG','UNDNWDY','UNDWDY'
                                 ,'WALL')
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
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-TRANSECT-PARAMETER
  realTest[c(113,413,713),]$TRANSECT <- ifelse(realTest[c(113,413,713),]$TRANSECT=='G'
                                              ,'H'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructVisRip(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect visRip dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructVisRip(realTest, test='all')
  ee <- as.matrix(rbind(

"Column UID has 3 missing values"                                                                                                                                         
,"Column TRANSECT has 3 missing values"                                                                                                                                    
,"Column TRANSDIR has 3 missing values"                                                                                                                                    
,"Column PARAMETER has 3 missing values"                                                                                                                                   
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSDIR)=(1001,XX)"                                                                                                              
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSDIR)=(1002,NA)"                                                                                                              
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSDIR)=(1002,NA)"                                                                                                              
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSDIR)=(1002,XX)"                                                                                                              
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSDIR)=(1002,NA)"                                                                                                              
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSDIR)=(1002,XX)"                                                                                                              
,"Unexpected value TRANSECT=(L) at (UID)=(1002)"                                                                                                                           
,"Unexpected value TRANSECT=(NA) at (UID)=(1002)"                                                                                                                          
,"Unexpected value TRANSECT=(L) at (UID)=(1002)"                                                                                                                           
,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"                                                                                                                          
,"Unexpected value TRANSECT=(L) at (UID)=(1001)"                                                                                                                           
,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"                                                                                                                          
,"Unexpected value PARAMETER=(NA) at (UID)=(1002)"                                                                                                                         
,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                                                                                      
,"Unexpected value PARAMETER=(NA) at (UID)=(1001)"                                                                                                                         
,"Unexpected value PARAMETER=(NA) at (UID)=(1002)"                                                                                                                         
,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                                                                                      
,"Unexpected value PARAMETER=(WRONG) at (UID)=(1002)"                                                                                                                      
,"Unexpected row count at (UID,TRANSECT,TRANSDIR,PARAMETER,SAMPLE_TYPE)=(1002,G,LF,CANVEG,PHAB_CHANB), n=2"                                                                
,"Unexpected row count at (UID,TRANSECT,TRANSDIR,PARAMETER,SAMPLE_TYPE)=(1001,G,RT,BUILD,PHAB_CHANW), n=2"                                                                 
,"Unexpected row count at (UID,TRANSECT,TRANSDIR,PARAMETER,SAMPLE_TYPE)=(1002,G,LF,LOG,PHAB_CHANW), n=2"                                                                   
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1002,L) "                                                                                                                    
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1002,XC) "                                                                                                                   
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1002,XD) "                                                                                                                   
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1002,NA) "                                                                                                                   
,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(NA,B) "                                                                                                                      
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(NA,H) "                                                                                                                      
,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1001,L) "                                                                                                                    
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1001,XJ) "                                                                                                                   
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1002,L) "                                                                                                                    
,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(NA,K) "                                                                                                                      
,"Absent TRANSECT=(A,C,D,E,F,G,I,J) value at UID=(NA) "                                                                                                                    
,"Absent PARAMETER=(BUILD,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PARK,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(1002,L) " 
,"Absent PARAMETER=(BUILD,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PARK,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(1002,XC) "
,"Absent PARAMETER=(BARE,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PARK,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(1002,XD) " 
,"Absent PARAMETER=(BARE,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PARK,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(1002,NA) " 
,"Absent PARAMETER=(BARE,BUILD,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(NA,B) "   
,"Absent PARAMETER=(BARE,CANBTRE,CANSTRE,CANVEG,GCNWDY,GCWDY,LANDFL,LOG,MINE,PARK,PAST,PAVE,PIPES,ROAD,ROW,UNDERVEG,UNDNWDY,UNDWDY,WALL) value at UID,TRANSECT=(NA,H) "   
            
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect VisRip data'
             )

}

# end of file
