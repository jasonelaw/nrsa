valStructFishCovTest <- function()
# Tests valStructFishCov()
{

  # Create correctly formated test data
  baseData <- rbind(expand.grid(UID = 1:10
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                   ,expand.grid(UID = 11:20
                               ,TRANSECT = c(LETTERS[1:11]
                                            ,paste('X', LETTERS[1:10], sep='')
                                            )
                               ,SAMPLE_TYPE = 'PHAB_CHANW'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                   )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$SAMPLE_TYPE <- as.character(baseData$SAMPLE_TYPE)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- rep(as.character(0:4), length.out=nrow(baseData))
  baseData$FLAG <- as.character(NA)
  
  # Create incorrectly formated test data base based on the good data
  realData <- baseData
  
  #    Make values missing, which will also make other values appear as absent
  #    and unexpected.
  realData[realData$UID==1,][1,]$UID <- ''
  realData[realData$UID==1,][2,]$TRANSECT <- ''
  realData[realData$UID==1,][3,]$PARAMETER <- ''
  realData[realData$UID==1,][4,]$SAMPLE_TYPE <- ''
  
  #    Make values absent, which will cause duplicates to appear
  realData[realData$UID == 2 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$TRANSECT<-'C'
  realData[realData$UID == 2 & realData$TRANSECT=='B' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'BOULDR'
  realData[realData$UID == 12 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'WOODY'

  #    Make unexpected values, which will cause some to be absent
  realData[realData$UID == 3 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$TRANSECT<-'XB'
  realData[realData$UID == 3 & realData$TRANSECT=='B' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'WRONG'

  # Look for structure errors where there are none
  rr <- valStructFishCov(baseData, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected structure errors where there are none"
             )
  
  # Look for structure errors where known ones exist.
  rr <- valStructFishCov(realData, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value TRANSECT=() at (UID)=(1)"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(2,C,ALGAE), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(2,B,BOULDR), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(12,A,WOODY), n=2"
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
             ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(3,B)"
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT)=(1,D)"
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(12,A) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(,A) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(1,) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,C) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,D) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,E) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(2,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(2,B) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(3,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(3,B) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(3,XB) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT)=(1,E)"
             )
  checkEquals(ee, rr
             ,"Error: Failed to detect structure errors where they exist"
             )

}

# end of file