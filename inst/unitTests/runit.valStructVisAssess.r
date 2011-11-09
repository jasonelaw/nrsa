valStructVisAssessTest <- function()
# Tests valStructVisAssess()
{
#  checkTrue(FALSE, "valStructVisAssessTest not implemented yet")
  
  # Create test data frame with perfect data.
  baseTest <- rbind(expand.grid(UID = as.character(1:10)
                               ,PARAMETER = c('AGR_CROP', 'AGR_IRRG', 'AGR_ORCH'
                                             ,'AGR_PAS', 'AGR_POUL', 'AGR_STCK'
                                             ,'AGR_WITH', 'APPEALING', 'BEAVER'
                                             ,'BVFLMOD', 'DLANDUSE', 'FORAGECL'
                                             ,'GEN_COM', 'IND_COML', 'IND_FIRE'
                                             ,'IND_IND', 'IND_LOG', 'IND_MINE'
                                             ,'IND_ODOR', 'IND_OIL', 'IND_POWR'
                                             ,'MAN_ANGL', 'MAN_CHAN', 'MAN_DAMS'
                                             ,'MAN_DRED', 'MAN_DWT', 'MAN_FISH'
                                             ,'MAN_LIME', 'MAN_WLF', 'PRISTINE'
                                             ,'REC_FILM', 'REC_PARK', 'REC_PRIM'
                                             ,'REC_TRAS', 'REC_TRLS', 'RES_BRID'
                                             ,'RES_CON', 'RES_DUMP', 'RES_LAWN'
                                             ,'RES_PIPE', 'RES_RES', 'RES_ROAD'
                                             ,'RES_SEWG', 'WEATHCOM'
                                             )
                               ))
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE <-'ASSE'
  baseTest[baseTest$PARAMETER %in% c('AGR_CROP', 'AGR_IRRG', 'AGR_ORCH'
                                    ,'AGR_PAS', 'AGR_POUL', 'AGR_STCK'
                                    ,'AGR_WITH', 'IND_COML', 'IND_FIRE'
                                    ,'IND_IND', 'IND_LOG', 'IND_MINE'
                                    ,'IND_ODOR', 'IND_OIL', 'IND_POWR'
                                    ,'MAN_ANGL', 'MAN_CHAN', 'MAN_DAMS'
                                    ,'MAN_DRED', 'MAN_DWT', 'MAN_FISH'
                                    ,'MAN_LIME', 'MAN_WLF', 'PRISTINE'
                                    ,'REC_FILM', 'REC_PARK', 'REC_PRIM'
                                    ,'REC_TRAS', 'REC_TRLS', 'RES_BRID'
                                    ,'RES_CON', 'RES_DUMP', 'RES_LAWN'
                                    ,'RES_PIPE', 'RES_RES', 'RES_ROAD'
                                    ,'RES_SEWG'
                                    )
          ,'RESULT'] <- rep(c('H','L','M'), length.out=370)
  baseTest[baseTest$PARAMETER=='APPEALING',]$RESULT <- rep(as.character(1:5), length.out=10)
  baseTest[baseTest$PARAMETER=='BEAVER',]$RESULT <- rep(c('Absent','Common','Rare'), length.out=10)
  baseTest[baseTest$PARAMETER=='BVFLMOD',]$RESULT <- rep(c('Major','Minor','None'), length.out=10)
  baseTest[baseTest$PARAMETER=='DLANDUSE',]$RESULT <- rep(c('Agriculture','Forest','Range','Suburban/Town','Urban'), length.out=10)
  baseTest[baseTest$PARAMETER=='FORAGECL',]$RESULT <- rep(c('>75','0-25','25-75'), length.out=10)
  baseTest[baseTest$PARAMETER=='GEN_COM',]$RESULT <- 'SEE COM FILE'
  baseTest[baseTest$PARAMETER=='WEATHCOM',]$RESULT <- 'SEE COM FILE'
  baseTest$FLAG <- as.character(NA)
  
  
  # Create real data with errors
  realTest <- baseTest
  
  #    Make some keys missing, which will also generate some absent PARAMETER
  #    messages.
  realTest[realTest$UID==1,][1,]$UID <-''
  realTest[realTest$UID==2,][1,]$PARAMETER <-''
  realTest[realTest$UID==3,][1,]$SAMPLE_TYPE <-''
  
  #    Make some parameters appear to be absent, others duplicated
  realTest[realTest$UID==4 & realTest$PARAMETER=='AGR_CROP',]$PARAMETER <- 'AGR_IRRG'
  
  #    Make up some unexpected values, which will make some values absent
  realTest[realTest$UID==5 & realTest$PARAMETER=='MAN_DRED',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID==6,][1,]$SAMPLE_TYPE <- 'AGR_IRRG'


  # Look for false positives with good data
  rr <- valStructVisAssess(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected structure problems in perfect Visual Assessment data"
             )
  
  # Look for false negatives with bad data
  rr <- valStructVisAssess(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Absent PARAMETER=(AGR_PAS,AGR_STCK,BEAVER,BVFLMOD,DLANDUSE,GEN_COM,APPEALING,FORAGECL,IND_FIRE,MAN_DAMS,PRISTINE,REC_PARK,RES_ROAD,REC_TRAS,REC_TRLS,WEATHCOM,RES_BRID,MAN_CHAN,REC_FILM,RES_LAWN,RES_PIPE,RES_RES,IND_COML,RES_SEWG,IND_IND,MAN_ANGL,RES_CON,AGR_IRRG,AGR_WITH,MAN_WLF,IND_OIL,IND_MINE,REC_PRIM,RES_DUMP,MAN_FISH,IND_LOG,IND_ODOR,IND_POWR,AGR_ORCH,MAN_DWT,MAN_DRED,AGR_POUL,MAN_LIME) value at UID=() "
             ,"Absent PARAMETER=(AGR_CROP) value at UID=(1) "
             ,"Absent PARAMETER=(AGR_CROP) value at UID=(2) "
             ,"Absent PARAMETER=(AGR_CROP) value at UID=(4) "
             ,"Absent PARAMETER=(MAN_DRED) value at UID=(5) "
             ,"Unexpected row count at (UID,SAMPLE_TYPE,PARAMETER)=(4,ASSE,AGR_IRRG), n=2"
             ,"Unexpected value PARAMETER=() at (UID)=(2)"
             ,"Unexpected value PARAMETER=(WRONG) at (UID)=(5)"
             ,"Unexpected value SAMPLE_TYPE=() at (UID)=(3)"
             ,"Unexpected value SAMPLE_TYPE=(AGR_IRRG) at (UID)=(6)"
             )
  checkEquals(ee, rr
             ,"Error: Did not correctly detect structure problems in imperfect Visual Assessment data"
             )

}

# end of file
