valStructOtherInvasivesTest <- function()
# Tests valStructOtherInvasives()

#cseq = function(from, to, by=1){
#       from=charToInt (from)
#       to=charToInt (to)
#       intToChar(seq(from,to,by))
#       }
       


{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='INVA'
                               ,PARAMETER=c('CONFIDENCE', 'PREVALANCE')
                               ,COMMON_NAME=LETTERS[1:26]
                               )
                
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$COMMON_NAME <- as.character(baseTest$COMMON_NAME)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  
  #    change parameter by substituting it with illegal values
  realTest[c(02,102,502),]$PARAMETER <- paste('X'
                                          ,realTest[c(5,51,151),]$PARAMETER
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(103,220)
  is.na(realTest$PARAMETER) <- c(104,212)
  is.na(realTest$COMMON_NAME) <- c(63,88)
  

  #    unexpected values of keys
  
  realTest[c(110,310,311),]$SAMPLE_TYPE <- 'DODO'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-COMMON_NAME-PARAMETER
  realTest[c(113,413,450),]$COMMON_NAME <- ifelse(realTest[c(113,413,450),]$COMMON_NAME=='G'
                                              ,'G'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructOtherInvasives(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect other invasives dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructOtherInvasives(realTest, test='all')
  ee <- as.matrix(rbind(
             "Column UID has 2 missing values"                                             
              ,"Column PARAMETER has 2 missing values"                                       
              ,"Column COMMON_NAME has 2 missing values"                                     
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1010)"                         
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1010)"                         
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                         
              ,"Unexpected value PARAMETER=(XCONFIDENCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(XPREVALANCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(NA) at (UID)=(1004)"                             
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected value PARAMETER=(NA) at (UID)=(1002)"                             
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected value PARAMETER=(XPREVALANCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected row count at (UID,PARAMETER,COMMON_NAME)=(1010,CONFIDENCE,G), n=2"
              ,"Unexpected row count at (UID,PARAMETER,COMMON_NAME)=(1003,PREVALANCE,G), n=3"

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Other Invasives data'
             )

}

# end of file
