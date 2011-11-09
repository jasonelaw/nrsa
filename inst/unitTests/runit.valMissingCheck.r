valMissingCheckTest <- function()
# Tests valMissingCheckTest.1()
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=1:10
                         ,PARAMETER=c('BAR_PRES','BACKWATER','CHANUNCD'
                                     ,'BARWIDTH' ,'WETWIDTH','DEP_POLE'
                                     ,'DEP_SONR'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- ''
  baseData$UNITS <- ''
  baseData$FLAG <- as.character(NA)
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
  baseData[baseData$PARAMETER=='BARWIDTH'
          ,]$RESULT <- rep(c(0,1,2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='BARWIDTH',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BARWIDTH',]$UNITS <- 'm'
  baseData[baseData$PARAMETER=='WETWIDTH'
          ,]$RESULT <- rep(c(1,5,10,20,50,100)
                          ,length.out=length(baseData[baseData$PARAMETER=='WETWIDTH',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='WETWIDTH',]$UNITS <- ''
  baseData[baseData$PARAMETER=='DEP_POLE'
          ,]$RESULT <- rep(c(2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_POLE',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='DEP_POLE'
          ,]$UNITS <- rep(c('ft','m')
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_POLE',]$UNITS)
                          )
  baseData[baseData$PARAMETER=='DEP_SONR'
          ,]$RESULT <- rep(c(2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_SONR',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='DEP_SONR',]$UNITS <- 'm'
  baseData[is.na(baseData$RESULT) | baseData$RESULT=='',]$FLAG <-'F1'

  baseData$SAMPLE_TYPE <- 'PHAB_THAL'

  meta.legal <- data.frame(PARAMETER=  c('BAR_PRES' , 'BACKWATER' , 'CHANUNCD'                               )
                          ,LEGALVALUES=c('|Y|N'     , '|Y|N'      , '|CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI')
                          ,FORMABBR  = c('Thal'    , 'Thal'    , 'Thal'                                      )
                          ,SAMPLE_TYPE  = c('PHAB_THAL'    , 'PHAB_THAL'    , 'PHAB_THAL'                    )
                          ,stringsAsFactors=FALSE
                          )
  meta.range <- data.frame(PARAMETER  =c('BARWIDTH' , 'WETWIDTH' , 'DEP_POLE' , 'DEP_POLE', 'DEP_SONR')
                          ,RANGETYPE  =c(''         , ''         , ''         , ''        , '')
                          ,RANGELOW   =c(0.0        , 0.0        , 0.0        , 0.0       , 0.0)
                          ,RANGEHIGH  =c(10         , 100        , 7          , 22        , 10)
                          ,UNITS      =c('m'        ,''          ,'m'         ,'ft'       , 'm')
                          ,FORMABBR   =c('Thal'     , 'Thal'     , 'Thal'     , 'Thal', 'Thal')
                          ,SAMPLE_TYPE=c('PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL')
                          ,stringsAsFactors=FALSE
                          )
  meta.df <- merge(meta.legal, meta.range
                  ,by=c('PARAMETER','FORMABBR','SAMPLE_TYPE')
                  ,all=TRUE, sort=FALSE
                  )
  meta.df[is.na(meta.df$LEGALVALUES),]$LEGALVALUES <- ''
  meta.df[is.na(meta.df$RANGETYPE),]$RANGETYPE <- ''
  meta.df[is.na(meta.df$RANGELOW),]$RANGELOW <- ''
  meta.df[is.na(meta.df$RANGEHIGH),]$RANGEHIGH <- ''
  meta.df[is.na(meta.df$UNITS),]$UNITS <- ''
  
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

  # Perform missing value check with perfect data
  rr <- valMissingCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No missing values were detected.", rr
             ,paste("Error: Problem detecting missing values in perfect data: "
                   ,rr
                   )
             )

  # Perform missing value check with imperfect data
  realData <- baseData
  realData[realData$UID==1 & realData$TRANSECT=='A' &
           realData$STATION==1 & realData$PARAMETER=='BARWIDTH',]$UNITS <- ''
  realData[realData$UID==2 & realData$TRANSECT=='B' &
           realData$STATION==1 & realData$PARAMETER=='DEP_POLE',]$UNITS <- ''
  realData[realData$UID==2 & realData$TRANSECT=='B' &
           realData$STATION==2 & realData$PARAMETER=='DEP_POLE',]$UNITS <- ''
  realData[realData$UID==3 & realData$TRANSECT=='C' &
           realData$STATION==3 & realData$PARAMETER=='WETWIDTH',]$RESULT <- ''
  realData[realData$UID==4 & realData$TRANSECT=='D' &
           realData$STATION==4 & realData$PARAMETER=='BARWIDTH',]$RESULT <- ''

  ee <- data.frame(UID=c('1','2','2','3','4')
                  ,SITE_ID=c('site1','site1','site1','site2','site2')
                  ,VISIT_NO=as.integer(c(1,2,2,1,2))
                  ,DATE_COL=c('2008-4-1','2008-5-1','2008-5-1','2008-4-2','2008-5-2')
                  ,SAMPLE_TYPE=rep('PHAB_THAL', 5)
                  ,TRANSECT=c('A','B','B','C','D')
                  ,STATION=as.integer(c(1,1,2,3,4))
                  ,PARAMETER=c('BARWIDTH','DEP_POLE','DEP_POLE','WETWIDTH','BARWIDTH')
                  ,TESTDESCRIPTION=rep('Missing value of RESULT, UNITS', 5)
                  ,RESULT=c('0','2','10','','')
                  ,UNITS=c('','','','','m')
                  ,FLAG=rep(as.character(NA), 5)
                  ,FORMIMAGE=c('=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V2_D.tif" , "Thal")'
                              )
                  ,stringsAsFactors=FALSE
                  )
  rr <- valMissingCheck.1(realData, meta.df, siteInfo)
  rr$COMMENTS<-NULL
 checkEquals(ee, rr
             ,paste("Error: Problem detecting missing values in imperfect data: "
                   ,rr
                   )
             )


}

# end of file
