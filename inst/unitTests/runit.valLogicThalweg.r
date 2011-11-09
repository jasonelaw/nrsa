valLogicThalwegTest <- function()
# Unit test for valLogicThalweg.1()
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=0:9
                         ,PARAMETER=c('BARWIDTH','WETWIDTH'
                                     ,'DEP_SONR','INCREMNT','REACHLENGTH'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData <- subset(baseData
                    ,PARAMETER %in% c('DEP_SONR','INCREMNT','REACHLENGTH') |
                     (PARAMETER %in% c('BARWIDTH','WETWIDTH') &
                      STATION %in% c(0,5)
                     )
                    )
  baseData$RESULT <- ''
  baseData[baseData$PARAMETER=='INCREMNT',]$RESULT <- c(rep(1.5, times=100)
                                                       ,rep(2.5, times=100)
                                                       ,rep(3.5, times=100)
                                                       ,rep(4.5, times=100)
                                                       ,rep(5.5, times=100)
                                                       )
  baseData[baseData$PARAMETER=='REACHLENGTH',]$RESULT <- c(rep(150, times=100)
                                                          ,rep(250, times=100)
                                                          ,rep(350, times=100)
                                                          ,rep(450, times=100)
                                                          ,rep(550, times=100)
                                                          )
  baseData[baseData$PARAMETER=='BARWIDTH',]$RESULT <- rep(c(0,2), times=50)
  baseData[baseData$PARAMETER=='WETWIDTH',]$RESULT <- rep(3, times=100)
  baseData$SAMPLE_TYPE<-''
  baseData[baseData$PARAMETER %in%
           c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
          ,]$SAMPLE_TYPE <- 'PHAB_THALW'

  meta.df <- data.frame(PARAMETER  =c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
                       ,LEGALVALUES=c(NA,        NA,        NA,        NA)
                       ,FORMABBR   =c('Thal',    'Thal',    'Thal',   'Thal')
                       ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THALW','PHAB_THALW','PHAB_THALW')
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=as.POSIXct(paste('2008', 4:5, rep(1:25, each=2), sep='-'))
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Test with perfect data
  rr <- valLogicThalweg.1(baseData, meta.df, siteInfo)
  ee <- "There were zero logical errors detected in the thalweg data."
  checkEquals(ee,rr, "Error: Detected logical thalweg errors where there are none")
  
  # Test with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION=='5' &
           realData$PARAMETER=='BARWIDTH',]$RESULT <- 20
  realData[realData$UID=='2' & realData$TRANSECT=='A' & realData$STATION=='0' &
           realData$PARAMETER=='INCREMNT',]$RESULT <- 15
  realData[realData$UID=='3' & realData$TRANSECT=='A' & realData$STATION=='0' &
           realData$PARAMETER=='REACHLENGTH',]$RESULT <- 15

  rr <- valLogicThalweg.1(realData, meta.df, siteInfo)
  ee <- data.frame(UID=c('1','1','2','2','3','3')
                  ,SITE_ID=c('site1','site1','site1','site1','site2','site2')
                  ,VISIT_NO=c(1,1,2,2,1,1)
                  ,DATE_COL=c("2008-04-01","2008-04-01","2008-05-01"
                             ,"2008-05-01","2008-04-02","2008-04-02"
                             )
                  ,SAMPLE_TYPE=rep('PHAB_THALW',6)
                  ,TRANSECT=rep("A", 6)
                  ,STATION=c(5,5,0,0,0,0)
                  ,PARAMETER=c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH','INCREMNT','REACHLENGTH')
                  ,TESTDESCRIPTION=c("Bar width MUST be less than wetted width"
                                    ,"Bar width MUST be less than wetted width"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    )
                  ,RESULT=c('20','3','15','150','1.5','15')
                  ,FORMIMAGE=c('=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_A.tif" , "Thal")'
                              )
                  ,COMMENTS=rep(paste(rep(' ',46),collapse=''), 6)
                  ,stringsAsFactors=FALSE
                  )
  ee$VISIT_NO <- as.integer(ee$VISIT_NO)
  ee$DATE_COL <- as.POSIXct(ee$DATE_COL)
  ee$STATION <- as.integer(ee$STATION)

  checkEquals(ee,rr, "Error: Incorrectly detected existing logical thalweg errors")
}