valStructWoodTest <- function()
# Tests valStructWood()

#cseq = function(from, to, by=1){
#       from=charToInt (from)
#       to=charToInt (to)
#       intToChar(seq(from,to,by))
#       }
       


{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_CHANBFRONT')
                               ,PARAMETER=c('DLDLL', 'DLDML','DLDSL'
                                    ,'DMDLL','DMDML','DMDSL'
                                    ,'DSDLL','DSDML','DSDSL'
                                    ,'DXDLL','DXDML','DXDSL'
                                    ,'WLDLL','WLDML','WLDSL'
                                    ,'WMDLL','WMDML','WMDSL'
                                    ,'WSDLL','WSDML','WSDSL'
                                    ,'WXDLL','WXDML','WXDSL'
                                    )
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
  realTest[c(201,1000,506),]$PARAMETER <- paste('X'
                                          ,realTest[c(50,510,1090),]$PARAMETER
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(10,22)
  is.na(realTest$PARAMETER) <- c(11,21)
 

  #    unexpected values of keys
  
  realTest[c(11,31,31),]$SAMPLE_TYPE <- 'DODO'
  realTest[c(11,31,51),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-PARAMETER
  realTest[c(11,41,45),]$PARAMETER <- ifelse(realTest[c(11,41,45),]$PARAMETER=='WXDSL'
                                              ,'WXDSL'
                                              ,'WXDSL'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructWood(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect other invasives dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructWood(realTest, test='all')
  ee <- as.matrix(rbind(
               "Column UID has 2 missing values"                                                            
              ,"Column PARAMETER has 1 missing values"                                                      
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                                        
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                                        
              ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1001,3)"                                 
              ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,4)"                              
              ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,6)"                              
              ,"Unexpected value PARAMETER=(XDLDLL) at (UID,TRANSECT)=(1001,10)"                            
              ,"Unexpected value PARAMETER=(XDMDML) at (UID,TRANSECT)=(1006,7)"                             
              ,"Unexpected value PARAMETER=(XDLDSL) at (UID,TRANSECT)=(1010,1)"                             
              ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1001,5,PHAB_THALW,WXDSL), n=2"
              ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1005,5,PHAB_THALW,WXDSL), n=2"
                        
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Other Invasives data'
             )

}

# end of file
