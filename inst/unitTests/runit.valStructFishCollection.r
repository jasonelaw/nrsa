valStructFishCollectionTest <- function()
# Tests valStructFishCollection()
{
  # Create test data frame by combining dataframes for FISHW, FISHB and LMSAMP
  fishb <- rbind(expand.grid(UID=as.character(1000 + 1:4)
                            ,TRANSECT=c(LETTERS[1:10], 'NOT MARKED')
                            ,SAMPLE_TYPE='FISHB'
                            ,PAGE=as.character(1:5)
                            ,LINE=as.character(1:2)
                            ,PARAMETER=c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                        ,'BANK_OTHER', 'COUNT', 'COVER'
                                        ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                        ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                        ,'NO_FISH', 'PHOTO', 'SHOCK_TIME'
                                        ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                        ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                        )
                            )
                 )
  fishb$UID <- as.character(fishb$UID)
  fishb$PAGE <- as.character(fishb$PAGE)
  fishb$LINE <- as.character(fishb$LINE)
  fishb$TRANSECT <- as.character(fishb$TRANSECT)
  fishb$SAMPLE_TYPE <- as.character(fishb$SAMPLE_TYPE)
  fishb$PARAMETER <- as.character(fishb$PARAMETER)
  fishb$RESULT <- floor(runif(nrow(fishb), 0, 18))
  fishb$FLAG <- ''
  fishb <- subset(fishb
                 ,!(
#                    TRANSECT %in% c(LETTERS(A:J)) &
#                    PARAMETER %in% c('AC_COUNT', 'AC_SIDE', 'BANK', 'BANK_OTHER'
#                                    ,'COVER', 'COVER_OTHER', 'DEPTH'
#                                    ,'DIST_SAMPLED', 'NO_FISH', 'SHOCK_TIME'
#                                    ,'SUB_OTHER', 'SUBSTRATE'
#                                    )
#                    |
                    TRANSECT=='NOT_MARKED' & PARAMETER=='NO_FISH'
#                    PARAMETER %in% c('AC_COUNT', 'AC_SIDE', 'NO_FISH', 'TRANA'
#                                    ,'TRANB', 'TRANC', 'TRAND', 'TRANE', 'TRANF'
#                                    ,'TRANG', 'TRANH', 'TRANI', 'TRANJ'
#                                    )
                    |
                    LINE != 1 & PARAMETER=='NO_FISH'
                   )
                 )

  fishw <- rbind(expand.grid(UID=as.character(1100 + 1:4)
                            ,TRANSECT='NONE'
                            ,SAMPLE_TYPE='FISHW'
                            ,PAGE=as.character(1:5)
                            ,LINE=as.character(1:2)
                            ,PARAMETER=c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                        ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                        ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                        ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                        ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                        ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                        )
                            )
                 )
  fishw$UID <- as.character(fishw$UID)
  fishw$PAGE <- as.character(fishw$PAGE)
  fishw$LINE <- as.character(fishw$LINE)
  fishw$TRANSECT <- as.character(fishw$TRANSECT)
  fishw$SAMPLE_TYPE <- as.character(fishw$SAMPLE_TYPE)
  fishw$PARAMETER <- as.character(fishw$PARAMETER)
  fishw$RESULT <- floor(runif(nrow(fishw), 0, 18))
  fishw$FLAG <- ''

  lmsamp <- expand.grid(UID=as.character(1000 + 1:2)
                       ,TRANSECT=c(LETTERS[1:10])
                       ,SAMPLE_TYPE='LMSAMP'
                       ,PAGE='999'
                       ,LINE='99'
                       ,PARAMETER=c('AC_COUNT', 'AC_SIDE')
                       )
  lmsamp$UID <- as.character(lmsamp$UID)
  lmsamp$PAGE <- as.character(lmsamp$PAGE)
  lmsamp$LINE <- as.character(lmsamp$LINE)
  lmsamp$TRANSECT <- as.character(lmsamp$TRANSECT)
  lmsamp$SAMPLE_TYPE <- as.character(lmsamp$SAMPLE_TYPE)
  lmsamp$PARAMETER <- as.character(lmsamp$PARAMETER)
  lmsamp$RESULT <- floor(runif(nrow(lmsamp), 0, 18))
  lmsamp$FLAG <- ''

  baseTest <- rbind(fishb, fishw, lmsamp)
  
  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  # Make some transects absent by removing them
  realTest<-subset(realTest, !(UID=='1002' & TRANSECT=='A' &
                   SAMPLE_TYPE=='FISHB' & PAGE=='1' & LINE=='1' &
                   PARAMETER=='ACTUAL_DATE')
                  )
  realTest<-subset(realTest, !(UID=='1102' & realTest$TRANSECT=='NONE' &
                   SAMPLE_TYPE=='FISHW' & PAGE=='1' & LINE=='1' &
                   PARAMETER=='ACTUAL_DATE')
                  )
  realTest<-subset(realTest, !(UID=='1002' & realTest$TRANSECT=='A' &
                   SAMPLE_TYPE=='LMSAMP' & PAGE=='999' & LINE=='99' &
                   PARAMETER=='AC_COUNT')
                  )

  # remove transect by substituting it with legal values.  Note FISHW data
  # has only NONE valued transects, so there are no substitutions possible
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- 'C'
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- 'C'

  # make key values missing
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='A' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$UID <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$UID <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='A' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$UID <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='D' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='2' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='D' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PARAMETER <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='E' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$LINE <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ANOM_CT',]$LINE <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='E' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$LINE <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='F' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='FINAL_CT',]$TRANSECT <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='F' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- NA

  realTest[realTest$UID == '1001' & realTest$TRANSECT %in% 'G' &
           realTest$SAMPLE_TYPE == 'FISHB' & realTest$PAGE == '1' &
           realTest$LINE == '1' & realTest$PARAMETER == 'ACTUAL_DATE',]$PAGE <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='TL_MAX',]$PAGE <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'G' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PAGE <- NA

  #    unexpected values of keys
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'H' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- 'L'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='MORT_CT',]$TRANSECT <- 'L'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'H' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- 'L'

  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'I' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$SAMPLE_TYPE <- 'WRONG'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='TAG',]$SAMPLE_TYPE <- 'WRONG'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'I' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$SAMPLE_TYPE <- 'WRONG'

  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'J' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='PHOTO',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'J' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PARAMETER <- 'WRONG'


  # Test perfect dataframe to look for false positives
  rr <- valStructFishCollection(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect FishCollection dataframe'
             )

  # Test real dataframe to look for false negatives
  rr <- valStructFishCollection(realTest, test='all')
  ee <- as.matrix(rbind(
       "Column UID has 3 missing values"
      ,"Column TRANSECT has 3 missing values"
      ,"Column PAGE has 3 missing values"
      ,"Column LINE has 3 missing values"
      ,"Column PARAMETER has 3 missing values"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1101)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1101)"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(NA,A) "
      ,"Absent PARAMETER=(ANOM_CT,COUNT,FINAL_CT,MORT_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID=(NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,A) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,B) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,D) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,F) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,H) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,I) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,J) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1002,A) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(NA,A) "
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1001,I)"
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1101,NONE)"
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1001,I)"
      ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,NOT MARKED) value at UID=(NA) "
      ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J) value at UID=(NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(NA,A) "
      ,"Absent PARAMETER=(ACTUAL_DATE,ANOM_CT,COUNT,FINAL_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID,TRANSECT=(1101,L) "
      ,"Absent PARAMETER=(ACTUAL_DATE,ANOM_CT,COUNT,MORT_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID,TRANSECT=(1101,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,A) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,B) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,D) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,F) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,H) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,I) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,J) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1002,A) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(NA,A) "
      )
    ,ncol=1) # end of as.matrix
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect FishCollection data'
             )

}

# end of file