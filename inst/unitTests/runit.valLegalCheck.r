valLegalCheckTest <- function()
# tests valLegalCheck.1
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=1:10
                         ,PARAMETER=c('BAR_PRES','BACKWATER','CHANUNCD'
                                     ,'BARWIDTH' ,'WETWIDTH','DEP_POLE'
                                     ,'DEP_SONR','OTHERSTUFF'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- ''
  baseData[baseData$PARAMETER=='BAR_PRES'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BAR_PRES',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BACKWATER'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BACKWATER',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='CHANUNCD'
          ,]$RESULT <- rep(c('','CA','DR','FA','GL','PP','PB','PI','PD','PL'
                            ,'PT','PO','RA','RI'
                            )
                          ,length.out=length(baseData[baseData$PARAMETER=='CHANUNCD',]$RESULT)
                          )
  baseData$SAMPLE_TYPE <- 'PHAB_THAL'
  
  meta.df <- data.frame(PARAMETER=  c('BAR_PRES' , 'BACKWATER' , 'CHANUNCD'                               )
                       ,LEGALVALUES=c('|Y|N'     , '|Y|N'      , '|CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI')
                       ,FORMABBR  = c('Thal'    , 'Thal'    , 'Thal'                                      )
                       ,SAMPLE_TYPE  = c('PHAB_THAL'    , 'PHAB_THAL'    , 'PHAB_THAL'                    )
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Perform legal value check with perfect data
  rr <- valLegalCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No illegal values were detected.", rr
             ,paste("Error: Problem detecting illegal values in perfect data: "
                   ,rr
                   )
             )
  
  # Perform legal value check with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION==1 &
           realData$PARAMETER=='BAR_PRES',]$RESULT <- 'Wrong'
  realData[realData$UID=='1' & realData$TRANSECT=='B' & realData$STATION==2 &
           realData$PARAMETER=='BACKWATER',]$RESULT <- 'Wrong'
  realData[realData$UID=='3' & realData$TRANSECT=='C' & realData$STATION==3 &
           realData$PARAMETER=='CHANUNCD',]$RESULT <- 'Wrong'
  realData[realData$UID=='4' & realData$TRANSECT=='D' & realData$STATION==4 &
           realData$PARAMETER=='BARWIDTH',]$RESULT <- '123456'

  rr <- valLegalCheck.1(realData, meta.df, siteInfo)
  expected <- data.frame(UID=c('1','1','3')
                        ,SITE_ID=c('site1', 'site1', 'site2')
                        ,VISIT_NO=as.integer(c(1,1,1))
                        ,DATE_COL=c("2008-4-1", "2008-4-1", "2008-4-2")
                        ,SAMPLE_TYPE=rep('PHAB_THAL', 3)
                        ,TRANSECT=c('A', 'B', 'C')
                        ,STATION=as.integer(c(1,2,3))
                        ,PARAMETER=c("BAR_PRES", "BACKWATER", "CHANUNCD")
                        ,TESTDESCRIPTION=c("Value must be |Y|N"
                                          ,"Value must be |Y|N"
                                          ,"Value must be |CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI"
                                          )
                        ,RESULT=c('Wrong', 'Wrong', 'Wrong')
                        ,FORMIMAGE=c('=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_B.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif\" , \"Thal\")'
                                    )
                        ,COMMENTS=rep("                                              ", 3)
                        ,stringsAsFactors=FALSE
                        )
  checkEquals(expected, rr
             ,"Error: Did not correctly identify illegal values"
             )
  
}
# end of file
