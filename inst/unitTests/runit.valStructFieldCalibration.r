valStructFieldCalibrationTest <- function()
# Tests valStructFieldCalibration()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='CALI'
                               ,PARAMETER=c('TEMP_SENSOR', 'DO_ELEVATION', 'PH_STD2_DESCRIPTION'
                                 ,'DO_BARO_PRESSURE', 'CAL_INST_OPERATOR', 'PH_QCS_MEASURED'
                                 ,'PH_QCS_TRUE', 'COND_QCS_DESC', 'CAL_INST_MODEL'
                                 ,'DO_DISPLAYED_VALUE','COND_STD1_VALUE','PH_STD1_DESCRIPTION'
                                 ,'TEMP_THERMOMETER','COND_STD1_DESC','COND_STD2_VALUE'
                                 ,'PH_QCS_DESC','COND_QCS_MEASURED','COND_QCS_TRUE'
                                 ,'DO_CALIBRATION_VALUE','PH_STD2_VALUE','PH_STD1_VALUE'
                                 ,'DO_PRESSURE','COND_STD2_DESC','CAL_INST_ID'
                                 )
                           )
                        )
                               
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
  baseTest$UNITS <- ' '
  
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

 
 
  #    make key values missing
  is.na(realTest$UID) <- c(103)
  is.na(realTest$PARAMETER) <- c(106)

  #    unexpected values of keys
  realTest[c(110),]$SAMPLE_TYPE <- 'BADSAMP'
  realTest[c(111),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID--PARAMETER
  realTest[c(113),]$PARAMETER <- ifelse(realTest[c(113),]$PARAMETER=='TEMP_THERMOMETER'
                                              ,'TEMP_THERMOMETER'
                                              ,'TEMP_THERMOMETER'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructFieldCalibration(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect FieldCalibration dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructFieldCalibration(realTest, test='all')
  ee <- as.matrix(rbind(
            "Column UID has 1 missing values"                                                                                                                                                                                                                                                                                                                                                                          
            ,"Column PARAMETER has 1 missing values"                                                                                                                                                                                                                                                                                                                                                                    
            ,"Unexpected value SAMPLE_TYPE=(BADSAMP) at (UID)=(1010)"                                                                                                                                                                                                                                                                                                                                                   
            ,"Unexpected value PARAMETER=(NA) at (UID)=(1006)"                                                                                                                                                                                                                                                                                                                                                          
            ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                                                                                                                                                                                                                                                                                                                       
            ,"Unexpected row count at (UID,PARAMETER)=(1003,TEMP_THERMOMETER), n=2"                                                                                                                                                                                                                                                                                                                                     
            ,"Absent PARAMETER=(PH_STD1_DESCRIPTION) value at UID=(1001) "                                                                                                                                                                                                                                                                                                                                              
            ,"Absent PARAMETER=(COND_STD1_VALUE,PH_STD1_DESCRIPTION) value at UID=(1003) "                                                                                                                                                                                                                                                                                                                              
            ,"Absent PARAMETER=(COND_STD1_VALUE) value at UID=(1006) "                                                                                                                                                                                                                                                                                                                                                  
            ,"Absent PARAMETER=(TEMP_SENSOR,DO_ELEVATION,PH_STD2_DESCRIPTION,DO_BARO_PRESSURE,CAL_INST_OPERATOR,PH_QCS_MEASURED,PH_QCS_TRUE,COND_QCS_DESC,CAL_INST_MODEL,DO_DISPLAYED_VALUE,PH_STD1_DESCRIPTION,TEMP_THERMOMETER,COND_STD1_DESC,COND_STD2_VALUE,PH_QCS_DESC,COND_QCS_MEASURED,COND_QCS_TRUE,DO_CALIBRATION_VALUE,PH_STD2_VALUE,PH_STD1_VALUE,DO_PRESSURE,COND_STD2_DESC,CAL_INST_ID) value at UID=(NA) "

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Field Calibration data'
             )

}

# end of file
