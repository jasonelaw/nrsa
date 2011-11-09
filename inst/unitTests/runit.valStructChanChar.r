valStructChanCharTest <- function()
# Tests valStructChanChar()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT='NONE'
                               ,SAMPLE_TYPE='PHAB_CHCON'
                               ,PARAMETER=c('BANKFULL','CONSTRNT','FEATURES',
                               'PATTERN','PERCENT','VALLEY','VALLYBOX')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    make key values missing
  is.na(realTest$UID) <- c(13)
  is.na(realTest$TRANSECT) <- c(24)
  is.na(realTest$PARAMETER) <- c(46)
  is.na(realTest$SAMPLE_TYPE) <- c(02)

  #    unexpected values of keys
  realTest[38,]$TRANSECT <- 'A'
  realTest[5,]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[19,]$PARAMETER <- 'WRONG'

  # Test perfect dataframe to look for false positives
  rr <- valStructChanChar(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )
             
  # Test real datafram to look for false negatives
  rr <- valStructChanChar(realTest, test='all')
  
  ee <- as.matrix(rbind(
   "Column UID has 1 missing values"                                                                      
  ,"Column TRANSECT has 1 missing values"                                                                 
  ,"Column PARAMETER has 1 missing values"                                                                
  ,"Column SAMPLE_TYPE has 1 missing values"                                                              
  ,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                                                       
  ,"Unexpected value TRANSECT=(A) at (UID)=(1008)"                                                        
  ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1009,NONE)"
  ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,NONE)"
  ,"Absent PARAMETER=(BANKFULL,CONSTRNT,PATTERN,PERCENT,VALLEY,VALLYBOX) value at UID,TRANSECT=(1004,NA) "
  ,"Absent PARAMETER=(BANKFULL,CONSTRNT,FEATURES,PERCENT,VALLEY,VALLYBOX) value at UID,TRANSECT=(1008,A) "
  ,"Unexpected value SAMPLE_TYPE=(NA) at (UID,TRANSECT)=(1002,NONE)"
  ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT)=(1005,NONE)"
                ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file