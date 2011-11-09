valStructChannelGeometryTest <- function()
# Tests valStructChannelGeometry()
{

  # Make base dataframe from two rather different dataframes, one from each
  # of the forms in the channel geometry table, to make logic easier.
  boatable <- expand.grid(UID=as.character(1:10)
                         ,TRANSECT=LETTERS[1:11]
                         ,LINE=as.character(1:4)
                         ,PARAMETER=c(# Top of form
                                      'ARRIVE','LEAVE', 'LINE','LATDD_TOP'
                                     ,'LATMM_TOP','LATSS_TOP','LONGDD_TOP'
                                     ,'LONGMM_TOP','LONGSS_TOP'
                                      # Bottom of form
                                     ,'ACTRANSP','INTDTRAN','SLOPE_ND'
                                     ,'SLOPE', 'BEAR','DISTANCE'
                                     ,'WAYPT','LATDD','LATMM','LATSS','LONGDD'
                                     ,'LONGMM','LONGSS'
                                     )
                         ,TRANLINE=c('BANK','MID','NONE')
                         )
  boatable <- subset(boatable, TRANLINE=='NONE' &
                               PARAMETER %in% c('ACTRANSP','INTDTRAN'
                                               ,'SLOPE_ND','SLOPE','BEAR'
                                               ,'DISTANCE','WAYPT','LATDD'
                                               ,'LATMM','LATSS','LONGDD'
                                               ,'LONGMM','LONGSS'
                                               ,'ARRIVE','LEAVE'
                                               )
                               |
                               TRANLINE %in% c('BANK','MID') &
                               PARAMETER %in% c('LATDD_TOP'
                                               ,'LATMM_TOP','LATSS_TOP'
                                               ,'LONGDD_TOP','LONGMM_TOP'
                                               ,'LONGSS_TOP'
                                               )
                    )
  boatable$UID <- as.character(boatable$UID)
  boatable$TRANSECT <- as.character(boatable$TRANSECT)
  boatable$LINE <- as.character(boatable$LINE)
  boatable$PARAMETER <- as.character(boatable$PARAMETER)
  boatable$BANK <- ifelse(boatable$TRANSECT %in% LETTERS[1:5], 'Right', 'Left')
  boatable$SAMPLE_TYPE <- 'PHAB_CHANBFRONT'
  boatable$UNITS <- 'NONE'
  boatable$METHOD <- 'NONE'
  boatable$RESULT <- ''
  boatable[boatable$PARAMETER=='ACTRANSP',]$RESULT <- '365'
  boatable[boatable$PARAMETER=='INTDTRAN',]$RESULT <- '350'
  boatable[boatable$PARAMETER=='SLOPE_ND',]$RESULT <- 'Y'
  boatable[boatable$PARAMETER=='SLOPE',]$RESULT <- '2.46'
  boatable[boatable$PARAMETER=='BEAR',]$RESULT <- '123'
  boatable[boatable$PARAMETER=='DISTANCE',]$RESULT <- '350'
  boatable[boatable$PARAMETER=='WAYPT',]$RESULT <- '1'      # Scarcely used, so
  boatable <- subset(boatable, PARAMETER != 'WAYPT' |       # leave it in 1 uid
                               PARAMETER=='WAYPT' & UID=='2'
                    )
  boatable[boatable$PARAMETER=='LATDD',]$RESULT <- '45'
  boatable[boatable$PARAMETER=='LATMM',]$RESULT <- '35'
  boatable[boatable$PARAMETER=='LATSS',]$RESULT <- '25'
  boatable[boatable$PARAMETER=='LONGDD',]$RESULT <- '45'
  boatable[boatable$PARAMETER=='LONGMM',]$RESULT <- '35'
  boatable[boatable$PARAMETER=='LONGSS',]$RESULT <- '25'
  boatable[boatable$PARAMETER=='ARRIVE',]$RESULT <- '12:34'
  boatable[boatable$PARAMETER=='LEAVE',]$RESULT <- '23:45'
  boatable[boatable$PARAMETER=='LATDD_TOP',]$RESULT <- '45'
  boatable[boatable$PARAMETER=='LATMM_TOP',]$RESULT <- '35'
  boatable[boatable$PARAMETER=='LATSS_TOP',]$RESULT <- '25'
  boatable[boatable$PARAMETER=='LONGDD_TOP',]$RESULT <- '45'
  boatable[boatable$PARAMETER=='LONGMM_TOP',]$RESULT <- '35'
  boatable[boatable$PARAMETER=='LONGSS_TOP',]$RESULT <- '25'
  boatable$FLAG <- as.character(NA)

  slope <- expand.grid(UID=as.character(11:20)
                      ,TRANSECT=LETTERS[1:10]
                      ,PARAMETER=c('BEARING','PROP','SLOPE')
                      ,LINE = as.character(1:4)
                      )
  slope$UID <- as.character(slope$UID)
  slope$TRANSECT <- as.character(slope$TRANSECT)
  slope$PARAMETER <- as.character(slope$PARAMETER)
  slope$LINE <- as.character(slope$LINE)
  slope$TRANLINE <- 'NONE'
  slope$BANK <- 'NONE'
  slope$SAMPLE_TYPE <- 'PHAB_SLOPE'
  slope$UNITS <- ifelse(slope$UID %in% c('11','13','15','17','19'),'CM','Percent')
  slope$METHOD <- ifelse(slope$UID %in% c('11','13','15','17','19'),'TR','CL')
  slope$RESULT <- ''
  slope[slope$PARAMETER=='BEARING',]$RESULT <- 234
  slope[slope$PARAMETER=='PROP',]$RESULT <- 100
  slope[slope$PARAMETER=='SLOPE',]$RESULT <- 1.23
  slope$FLAG <- as.character(NA)

  baseData <- rbind(boatable, slope)
  

  # Make up some data with real issues
  realData <- baseData
  
  # boatable errs - problems with UNITS and METHOD are left for later QA and
  # won't show up in the structure checks.
  realData[realData$UID=='2' & realData$TRANSECT=='B',]$TRANSECT='XB'            #
  realData <- subset(realData, !(UID=='3' & TRANSECT=='C'))                      #
  realData <- subset(realData, !(UID=='4' & PARAMETER=='ACTRANSP'))              #
  realData[realData$UID=='5' & realData$TRANSECT=='B',]$UNITS <- NA
  realData[realData$UID=='6' &
           realData$TRANSECT=='A' &
           realData$PARAMETER=='LEAVE'
          ,
          ]$METHOD <- NA
  realData[realData$UID=='6' &
           realData$TRANSECT=='C' &
           realData$PARAMETER=='ARRIVE'
          ,
          ]$TRANLINE <- 'NONE'                                                   #
  realData[realData$UID=='6' &
           realData$TRANSECT=='D' &
           realData$PARAMETER=='SLOPE_ND'
          ,
          ]$TRANLINE <- 'BANK'                                                   #
  realData[realData$UID=='6' &
           realData$TRANSECT=='D' &
           realData$PARAMETER=='LATDD_TOP'
          ,
          ]$TRANLINE <- 'NONE'                                                   #
  realData[realData$UID=='7' & realData$TRANSECT=='A',]$TRANSECT <- NA           #

  # slope errs - problems with UNITS and METHOD are left for later QA and
  # won't show up in the structure checks.
  realData[realData$UID=='11' & realData$TRANSECT=='B',]$TRANSECT='XB'           #
  realData <- subset(realData, !(UID=='11' & TRANSECT=='C'))                     #
  realData <- subset(realData, !(UID=='12' & PARAMETER=='SLOPE'))                #
  realData[realData$UID=='13' & realData$TRANSECT=='B',]$UNITS <- NA
  realData[realData$UID=='14' & realData$TRANSECT=='A',]$METHOD <- 'clinometer'


  # Look for structure problems where there are none
  rr <- valStructChannelGeometry(baseData, test='all')
  checkEquals(NULL,rr
             ,"Error: Detected structure problems where there are none"
             )
  
  
  # Look again for structure problems where there are none, and no WAYPT
  tt <- subset(baseData, PARAMETER != 'WAYPT')
  rr <- valStructChannelGeometry(tt, test='all')
  checkEquals(NULL,rr
             ,"Error: Detected structure problems where there are none"
             )


  # Make sure structure problems are detected
  rr <- valStructChannelGeometry(realData, test='all')
  ee <- rbind(   "Column TRANSECT has 104 missing values"                                      
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS_TOP)"             
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS_TOP)"            
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ARRIVE)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ARRIVE)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ARRIVE)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ARRIVE)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LEAVE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LEAVE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LEAVE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LEAVE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ACTRANSP)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ACTRANSP)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ACTRANSP)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,ACTRANSP)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,INTDTRAN)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,INTDTRAN)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,INTDTRAN)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,INTDTRAN)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE_ND)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE_ND)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE_ND)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE_ND)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,SLOPE)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,BEAR)"                  
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,BEAR)"                  
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,BEAR)"                  
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,BEAR)"                  
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,DISTANCE)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,DISTANCE)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,DISTANCE)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,DISTANCE)"              
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATDD)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATMM)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LATSS)"                 
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGDD)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGMM)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS)"                
,"Unexpected value TRANSECT=(NA) at (UID,PARAMETER)=(7,LONGSS)"                
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,BEARING)"              
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,PROP)"                 
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,SLOPE)"                
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,BEARING)"              
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,PROP)"                 
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,SLOPE)"                
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,BEARING)"              
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,PROP)"                 
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,SLOPE)"                
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,BEARING)"              
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,PROP)"                 
,"Unexpected value TRANSECT=(XB) at (UID,PARAMETER)=(11,SLOPE)"                
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(NONE) at (UID,TRANSECT,PARAMETER)=(6,D,LATDD_TOP)"
,"Unexpected value TRANLINE=(BANK) at (UID,TRANSECT,PARAMETER)=(6,D,SLOPE_ND)" 
,"Unexpected value TRANLINE=(BANK) at (UID,TRANSECT,PARAMETER)=(6,D,SLOPE_ND)" 
,"Unexpected value TRANLINE=(BANK) at (UID,TRANSECT,PARAMETER)=(6,D,SLOPE_ND)" 
,"Unexpected value TRANLINE=(BANK) at (UID,TRANSECT,PARAMETER)=(6,D,SLOPE_ND)" 
,"Absent TRANSECT=(B) value at UID=(2) "                                       
,"Absent TRANSECT=(C) value at UID=(3) "                                       
,"Absent TRANSECT=(A) value at UID=(7) "                                       
,"Absent TRANSECT=(B,C) value at UID=(11) "                                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,A) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,B) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,C) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,D) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,E) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,F) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,G) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,H) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,I) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,J) "                    
,"Absent PARAMETER=(ACTRANSP) value at UID,TRANSECT=(4,K) "                    
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,A) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,B) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,C) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,D) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,E) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,F) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,G) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,H) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,I) "                      
,"Absent PARAMETER=(SLOPE) value at UID,TRANSECT=(12,J) "        
             )
  checkEquals(ee,rr
             ,"Error: Detected structure problems where there are none"
             )

}

# end of file
