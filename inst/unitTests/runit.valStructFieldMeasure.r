valStructFieldMeasureTest <- function()
# Tests valStructFieldMeasure()

{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='FIELDMEAS'
                               ,PARAMETER= c('CONDUCTIVITY','CORRECTED','DO','pH'
                                   ,'TEMPERATURE','TIME'
                                   ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                                   )
                               ,TRANSECT  = LETTERS[1:11]
                               ,BANK =  c('CENTER', 'NOT MARKED', 'OTHER', 'LB'
                                   ,'MID-CHANNEL', 'RC', 'RB', 'LC') 
                           )
                        )
                               
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$BANK <- as.character(baseTest$BANK)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

 
 
  #    make key values missing
  is.na(realTest$UID) <- c(103)
  is.na(realTest$PARAMETER) <- c(106)

  #    unexpected values of keys
  realTest[c(110),]$SAMPLE_TYPE <- 'BADSAMP'
  realTest[c(111),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID--PARAMETER
  realTest[c(113),]$PARAMETER <- ifelse(realTest[c(113),]$PARAMETER=='TIME'
                                              ,'TIME'
                                              ,'TIME'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructFieldMeasure(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Field Measure dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructFieldMeasure(realTest, test='all')
  ee <- as.matrix(rbind(
             "Column UID has 1 missing values"                                                                               
             ,"Column PARAMETER has 1 missing values"                                                                         
             ,"Unexpected row count at (UID,PARAMETER,TRANSECT,BANK)=(1003,TIME,B,CENTER), n=2"                               
             ,"Unexpected value PARAMETER=(NA) at (UID)=(1006)"                                                               
             ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                            
             ,"Absent PARAMETER=(CONDUCTIVITY,DO,pH,TEMPERATURE,TIME,DISAPPEARS,REAPPEARS,CLEAR_TO_BOTTOM) value at UID=(NA) "

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Field Measure data'
             )

}

# end of file