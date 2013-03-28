valStructSamplesTest <- function()
# Tests valStructSamples()
{
  # Create test data frame
  fishtv.a <- expand.grid(UID=as.character(1000 + 1:10)
                               ,PAGE=(1)
                               ,LINE=(1:3)
                               ,SUBREACH=c('AB')
                               ,SAMPLE_TYPE='FISHTV'
                               ,PARAMETER=c('ACTUAL_DATE','PRESERVED','SAMPLE_ID','COMMON_NAME','FROZEN','TTL_LENGTH')
                               ,RESULT=NA
                               )
  fishtv.b <- expand.grid(UID=as.character(1000 + 1:10)
                               ,PAGE=(2)
                               ,LINE=(1:3)
                               ,SUBREACH=c('DE')
                               ,SAMPLE_TYPE='FISHTV'
                               ,PARAMETER=c('ACTUAL_DATE','PRESERVED','SAMPLE_ID','COMMON_NAME','FROZEN','TTL_LENGTH')
                               ,RESULT=NA
                               )
  fishtv <- rbind(fishtv.a,fishtv.b)                             
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='PRESERVED','Y',fishtv$RESULT)
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='COMMON_NAME','BRIDGELIP SUCKER',fishtv$RESULT)
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='SAMPLE_ID','123456',fishtv$RESULT)
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='ACTUAL_DATE','01/01/1999',fishtv$RESULT)
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='FROZEN','Y',fishtv$RESULT)
  fishtv$RESULT <- ifelse(fishtv$PARAMETER=='TTL_LENGTH','123',fishtv$RESULT)
  
  
                                 
  belg <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='BELGW'
                               ,PARAMETER=c('SAMPLE_ID','JAR_NO','PRESERVED')
                               ,RESULT=NA
                               )
  belg$RESULT <- ifelse(belg$PARAMETER=='SAMPLE_ID','234567',belg$RESULT)
  belg$RESULT <- ifelse(belg$PARAMETER=='JAR_NO','9',belg$RESULT)
  belg$RESULT <- ifelse(belg$PARAMETER=='PRESERVED','Y',belg$RESULT)

 berw <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='BERWW'
                               ,PARAMETER=c('SAMPLE_ID','JAR_NO','PRESERVED')
                               ,RESULT=NA
                               )
  berw$RESULT <- ifelse(berw$PARAMETER=='SAMPLE_ID','234568',berw$RESULT)
  berw$RESULT <- ifelse(berw$PARAMETER=='JAR_NO','10',berw$RESULT)
  berw$RESULT <- ifelse(berw$PARAMETER=='PRESERVED','Y',berw$RESULT)

 chem <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='CHEMB'
                               ,PARAMETER=c('SAMPLE_ID','CHILLED')
                               ,RESULT=NA
                               )
  chem$RESULT <- ifelse(chem$PARAMETER=='SAMPLE_ID','345678',chem$RESULT)
  chem$RESULT <- ifelse(chem$PARAMETER=='CHILLED','Y',chem$RESULT)
                               
 ente <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='ENTEW'
                               ,PARAMETER=c('SAMPLE_ID','DEPTH_COLLECTED','FILTER_1',
                               'FILTER_2','FILTER_3','FILTER_4','FILTER_END_TIME',
                               'FILTER_START_TIME','SAMPLE_VOL','TIME_COLLECTED',
                               'TIME_FROZEN')
                               ,RESULT=NA
                               )
  ente$RESULT <- ifelse(ente$PARAMETER=='SAMPLE_ID','456789',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='DEPTH_COLLECTED','10',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_1','10',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_2','15',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_3','20',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_4','30',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_END_TIME','13:15',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='FILTER_START_TIME','12:45',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='SAMPLE_VOL','10',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='TIME_COLLECTED','10:30',ente$RESULT)
  ente$RESULT <- ifelse(ente$PARAMETER=='TIME_FROZEN','13:30',ente$RESULT)

 kick <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='KICKB'
                               ,PARAMETER=c('SAMPLE_ID','JAR_NO','TYPE')
                               ,RESULT=NA
                               )
  kick$RESULT <- ifelse(kick$PARAMETER=='SAMPLE_ID','567890',kick$RESULT)
  kick$RESULT <- ifelse(kick$PARAMETER=='JAR_NO','11',kick$RESULT)
  kick$RESULT <- ifelse(kick$PARAMETER=='TYPE','Surrogate',kick$RESULT)

 papa <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PAPAB'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','FROZEN',
                               'TRAN_NO','VOLUME')
                               ,RESULT=NA
                               )
  papa$RESULT <- ifelse(papa$PARAMETER=='SAMPLE_ID','678901.4',papa$RESULT)
  papa$RESULT <- ifelse(papa$PARAMETER=='COMP_VOL','5000',papa$RESULT)
  papa$RESULT <- ifelse(papa$PARAMETER=='FROZEN','Y',papa$RESULT)
  papa$RESULT <- ifelse(papa$PARAMETER=='TRAN_NO','5',papa$RESULT)
  papa$RESULT <- ifelse(papa$PARAMETER=='VOLUME','250',papa$RESULT)

 pbio <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PBIOW'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','FROZEN',
                               'TRAN_NO','VOLUME')
                               ,RESULT=NA
                               )
  pbio$RESULT <- ifelse(pbio$PARAMETER=='SAMPLE_ID','678901.3',pbio$RESULT)
  pbio$RESULT <- ifelse(pbio$PARAMETER=='COMP_VOL','5000',pbio$RESULT)
  pbio$RESULT <- ifelse(pbio$PARAMETER=='FROZEN','Y',pbio$RESULT)
  pbio$RESULT <- ifelse(pbio$PARAMETER=='TRAN_NO','5',pbio$RESULT)
  pbio$RESULT <- ifelse(pbio$PARAMETER=='VOLUME','250',pbio$RESULT)

 pchl <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PCHLW'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','FROZEN',
                               'TRAN_NO','VOLUME')
                               ,RESULT=NA
                               )
  pchl$RESULT <- ifelse(pchl$PARAMETER=='SAMPLE_ID','678901.2',pchl$RESULT)
  pchl$RESULT <- ifelse(pchl$PARAMETER=='COMP_VOL','5000',pchl$RESULT)
  pchl$RESULT <- ifelse(pchl$PARAMETER=='FROZEN','Y',pchl$RESULT)
  pchl$RESULT <- ifelse(pchl$PARAMETER=='TRAN_NO','5',pchl$RESULT)
  pchl$RESULT <- ifelse(pchl$PARAMETER=='VOLUME','250',pchl$RESULT)

 peri <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PERIB'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','PRESERVED',
                               'TRAN_NO','VOLUME')
                               ,RESULT=NA
                               )
  peri$RESULT <- ifelse(peri$PARAMETER=='SAMPLE_ID','678901.1',peri$RESULT)
  peri$RESULT <- ifelse(peri$PARAMETER=='COMP_VOL','5000',peri$RESULT)
  peri$RESULT <- ifelse(peri$PARAMETER=='PRESERVED','Y',peri$RESULT)
  peri$RESULT <- ifelse(peri$PARAMETER=='TRAN_NO','5',peri$RESULT)
  peri$RESULT <- ifelse(peri$PARAMETER=='VOLUME','250',peri$RESULT)

 phyt <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PHYTB'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','LOCATIONS')
                               ,RESULT=NA
                               )
  phyt$RESULT <- ifelse(phyt$PARAMETER=='SAMPLE_ID','789012',phyt$RESULT)
  phyt$RESULT <- ifelse(phyt$PARAMETER=='COMP_VOL','2500',phyt$RESULT)
  phyt$RESULT <- ifelse(phyt$PARAMETER=='LOCATIONS','5',phyt$RESULT)

 ppcp <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='PPCPB'
                               ,PARAMETER=c('SAMPLE_ID','CHILLED')
                               ,RESULT=NA
                               )
  ppcp$RESULT <- ifelse(ppcp$PARAMETER=='SAMPLE_ID','789012',ppcp$RESULT)
  ppcp$RESULT <- ifelse(ppcp$PARAMETER=='CHILLED','Y',ppcp$RESULT)

 sede <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='SEDEW'
                               ,PARAMETER=c('SAMPLE_ID','COMP_VOL','CHILLED',
                               'TRAN_NO','JAR_NO','PRESERVED')
                               ,RESULT=NA
                               )
  sede$RESULT <- ifelse(sede$PARAMETER=='SAMPLE_ID','890123',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='COMP_VOL','3000',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='CHILLED','Y',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='TRAN_NO','11',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='JAR_NO','1',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='PRESERVED','Y',sede$RESULT)

 snag <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='SNAGB'
                               ,PARAMETER=c('SAMPLE_ID','JAR_NO','TYPE','TRAN_NO')
                               ,RESULT=NA
                               )
  snag$RESULT <- ifelse(snag$PARAMETER=='SAMPLE_ID','901234',snag$RESULT)
  snag$RESULT <- ifelse(snag$PARAMETER=='JAR_NO','2',snag$RESULT)
  snag$RESULT <- ifelse(snag$PARAMETER=='TYPE','Natural',snag$RESULT)
  snag$RESULT <- ifelse(snag$PARAMETER=='TRAN_NO','11',snag$RESULT)

 wchl <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='WCHLW'
                               ,PARAMETER=c('SAMPLE_ID','VOLUME_FILTERED','FROZEN',
                               'CHILLED','COMP_VOL')
                               ,RESULT=NA
                               )
  sede$RESULT <- ifelse(sede$PARAMETER=='SAMPLE_ID','987654',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='FROZEN','Y',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='VOLUME_FILTERED','50',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='CHILLED','Y',sede$RESULT)
  sede$RESULT <- ifelse(sede$PARAMETER=='COMP_VOL','2000',sede$RESULT)

 wpfc <- expand.grid(UID=as.character(1000 + 1:10)
                                ,PAGE=999
                               ,LINE=99
                               ,SUBREACH="NOT MARKED"
                               ,SAMPLE_TYPE='WPFCB'
                               ,PARAMETER=c('SAMPLE_ID','TIME_COLLECTED')
                               ,RESULT=NA
                               )
  wpfc$RESULT <- ifelse(wpfc$PARAMETER=='SAMPLE_ID','876543',wpfc$RESULT)
  wpfc$RESULT <- ifelse(wpfc$PARAMETER=='TIME_COLLECTED','10:04',wpfc$RESULT)
  
  baseTest <- rbind(belg, berw,chem,ente,fishtv,kick,papa,pbio,pchl,peri,
                    phyt,ppcp,sede,snag,wchl,wpfc)
                    
  baseTest$SAMPLE_CAT <- 'P'
  baseTest$FLAG <- ''                   

  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SUBREACH <- as.character(baseTest$PARAMETER)


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    remove transect by removing it from dataframe
  realTest <- subset(realTest, !(row.names(realTest) %in% c(201,301,401)))

  #    remove transect by substituting it with legal values
  realTest[c(02,102,502),]$SAMPLE_CAT <- paste('X'
                                          ,realTest[c(5,51,151),]$SAMPLE_CAT
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(103,503,903)
  is.na(realTest$SAMPLE_CAT) <- c(104,404,604)
  is.na(realTest$PARAMETER) <- c(106,206,306)
  is.na(realTest$SUBREACH) <- c(50,100,300)
  #    unexpected values of keys
  realTest[c(08,308,508),]$SAMPLE_CAT <- 'L'
  realTest[c(109,509,609),]$SUBREACH <- 'XX'           # always unexpected
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'SAMP_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'



  # Test perfect dataframe to look for false positives
  rr <- valStructSamples(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Samples dataframe'
             )
             
  # Test real datafram to look for false negatives
  rr <- valStructSamples(realTest, test='all')
  ee <- as.matrix(rbind(
                   "Column UID has 3 missing values"                                                                                                                                                                    
,"Column SAMPLE_CAT has 3 missing values"                                                                                                                                                             
,"Column PARAMETER has 3 missing values"                                                                                                                                                              
,"Unexpected value SAMPLE_TYPE=(SAMP_WRONG) at (UID)=(1010)"                                                                                                                                          
,"Unexpected value SAMPLE_TYPE=(SAMP_WRONG) at (UID)=(1002)"                                                                                                                                          
,"Unexpected value SAMPLE_TYPE=(SAMP_WRONG) at (UID)=(1003)"                                                                                                                                          
,"Unexpected value SAMPLE_CAT=(XP) at (UID)=(1002)"                                                                                                                                                   
,"Unexpected value SAMPLE_CAT=(L) at (UID)=(1008)"                                                                                                                                                    
,"Unexpected value SAMPLE_CAT=(XP) at (UID)=(1002)"                                                                                                                                                   
,"Unexpected value SAMPLE_CAT=(NA) at (UID)=(1004)"                                                                                                                                                   
,"Unexpected value SAMPLE_CAT=(L) at (UID)=(1010)"                                                                                                                                                    
,"Unexpected value SAMPLE_CAT=(NA) at (UID)=(1007)"                                                                                                                                                   
,"Unexpected value SAMPLE_CAT=(XP) at (UID)=(1005)"                                                                                                                                                   
,"Unexpected value SAMPLE_CAT=(L) at (UID)=(1001)"                                                                                                                                                    
,"Unexpected value SAMPLE_CAT=(NA) at (UID)=(1007)"                                                                                                                                                   
,"Unexpected value PARAMETER=(NA) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1006,ENTEW,P)"
,"Unexpected value PARAMETER=(WRONG) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1001,ENTEW,P)"
,"Unexpected value PARAMETER=(NA) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1007,FISHTV,P)"                                                                                                                    
,"Unexpected value PARAMETER=(NA) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1008,FISHTV,P)"                                                                                                                    
,"Unexpected value PARAMETER=(WRONG) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1003,FISHTV,P)"                                                                                                                 
,"Unexpected value PARAMETER=(WRONG) at (UID,SAMPLE_TYPE,SAMPLE_CAT)=(1004,FISHTV,P)"                                                                                                                 
,"Absent PARAMETER=(JAR_NO,PRESERVED) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1002,BELGW,XP) "
,"Absent PARAMETER=(JAR_NO,PRESERVED) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1008,BELGW,L) "
,"Absent PARAMETER=(DEPTH_COLLECTED,FILTER_2,FILTER_3,FILTER_4,FILTER_END_TIME,FILTER_START_TIME,SAMPLE_ID,SAMPLE_VOL,TIME_COLLECTED,TIME_FROZEN) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1002,ENTEW,XP) "
,"Absent PARAMETER=(FILTER_1) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1004,ENTEW,P) "
,"Absent PARAMETER=(DEPTH_COLLECTED,FILTER_2,FILTER_3,FILTER_4,FILTER_END_TIME,FILTER_START_TIME,SAMPLE_ID,SAMPLE_VOL,TIME_COLLECTED,TIME_FROZEN) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1004,ENTEW,NA) "
,"Absent PARAMETER=(COMP_VOL,SAMPLE_ID,TRAN_NO,VOLUME) value at UID,SAMPLE_TYPE,SAMPLE_CAT=(1007,PAPAB,NA) "


                 ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Sample data'
             )

}

# end of file