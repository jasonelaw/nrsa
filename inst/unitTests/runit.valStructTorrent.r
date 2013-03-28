valStructTorrentTest <- function()
# Tests valStructTorrent()

#cseq = function(from, to, by=1){
#       from=charToInt (from)
#       to=charToInt (to)
#       intToChar(seq(from,to,by))
#       }
       


{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='TORR'
                               ,PARAMETER=c('TSD01', 'TSD02', 'TSD03', 'TSD04', 'TSD05'
                                 ,'TSD06', 'TSD07', 'TSD08', 'TSD09', 'TSD10', 'TSD11')
                                )
                
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  
  #    change parameter by substituting it with illegal values
  realTest[c(2,10,50),]$PARAMETER <- paste('X'
                                          ,realTest[c(5,51,109),]$PARAMETER
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(10,22)
  is.na(realTest$PARAMETER) <- c(11,21)
 

  #    unexpected values of keys
  
  realTest[c(11,31,31),]$SAMPLE_TYPE <- 'DODO'
  realTest[c(11,31,51),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-PARAMETER
  realTest[c(11,41,45),]$PARAMETER <- ifelse(realTest[c(11,41,45),]$PARAMETER=='TSD11'
                                              ,'TSD11'
                                              ,'TSD11'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructTorrent(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect torrent dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructTorrent(realTest, test='all')
  ee <- as.matrix(rbind(
              "Column UID has 2 missing values"                           
             ,"Column PARAMETER has 1 missing values"                     
             ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"       
             ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"       
             ,"Unexpected value PARAMETER=(XTSD01) at (UID)=(1002)"       
             ,"Unexpected value PARAMETER=(XTSD06) at (UID)=(NA)"         
             ,"Unexpected value PARAMETER=(NA) at (UID)=(1001)"           
             ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"        
             ,"Unexpected value PARAMETER=(XTSD11) at (UID)=(1010)"      
             ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"        
             ,"Unexpected row count at (UID,PARAMETER)=(1001,TSD11), n=3"
             ,"Unexpected row count at (UID,PARAMETER)=(1005,TSD11), n=2"
             ,"Unexpected row count at (UID,PARAMETER)=(1001,WRONG), n=2" 
             

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect torrent data'
             )

}
                  
# end of file
