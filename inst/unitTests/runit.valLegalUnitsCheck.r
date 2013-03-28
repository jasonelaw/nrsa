valLegalUnitsCheckTest <- function()
# tests valLegalUnitsCheck.1
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
  baseData$UNITS<-''
  baseData[baseData$PARAMETER=='DEP_SONR' & baseData$TRANSECT %in% c('A','C'),]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='DEP_SONR' & !(baseData$TRANSECT %in% c('A','C')),]$UNITS <- 'FT'
  baseData[baseData$PARAMETER=='INCREMNT',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='REACHLENGTH',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='BARWIDTH',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='WETWIDTH',]$UNITS <- 'M'
  baseData$SAMPLE_TYPE<-''
  baseData[baseData$PARAMETER %in%
           c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
          ,]$SAMPLE_TYPE <- 'PHAB_THALW'
  baseData[baseData$PARAMETER=='DEP_SONR',]$SAMPLE_TYPE <- 'PHAB_THAL'
  
  meta.df <- data.frame(PARAMETER  =c('BARWIDTH','WETWIDTH','DEP_SONR','DEP_SONR','INCREMNT','REACHLENGTH')
                       ,LEGALVALUES=c(NA,        NA,        NA,        NA,        NA,        NA)
                       ,FORMABBR   =c('Thal',    'Thal',    'Thal',   'Thal',   'Thal',   'Thal')
                       ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THALW','PHAB_THAL','PHAB_THAL','PHAB_THALW','PHAB_THALW')
                       ,UNITS      =c('M', 'M', 'M', 'FT', 'M', 'M')
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=as.character(as.POSIXct(paste('2008', 4:5, rep(1:25, each=2), sep='-')))
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Perform legal value check with perfect data
  rr <- valLegalUnitsCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No illegal units were detected.", rr
             ,paste("Error: Problem detecting illegal values in perfect data: "
                   ,rr
                   )
             )
  
  # Perform legal value check with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION==0 &
           realData$PARAMETER=='BARWIDTH',]$UNITS <- 'Furlongs'
  realData[realData$UID=='2' & realData$TRANSECT=='B' & realData$STATION==2 &
           realData$PARAMETER=='DEP_SONR',]$UNITS <- 'Fathoms'
  realData[realData$UID=='3' & realData$TRANSECT=='C' & realData$STATION==0 &
           realData$PARAMETER=='REACHLENGTH',]$UNITS <- as.character(NA)

  rr <- valLegalUnitsCheck.1(realData, meta.df, siteInfo)
  expected <- data.frame(UID=c('1','2','3')
                        ,SITE_ID=c('site1', 'site1', 'site2')
                        ,VISIT_NO=as.integer(c(1,2,1))
                        ,DATE_COL=c("2008-04-01", "2008-05-01", "2008-04-02")
                        ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THAL','PHAB_THALW')
                        ,TRANSECT=c('A', 'B', 'C')
                        ,STATION=as.integer(c(0,2,0))
                        ,PARAMETER=c("BARWIDTH", "DEP_SONR", "REACHLENGTH")
                        ,TESTDESCRIPTION=c("UNITS value must be  M"
                                          ,"UNITS value must be  M, FT"
                                          ,"UNITS value must be  M"
                                          )
                        ,RESULT=c('0', '', '150')
                        ,UNITS=c('Furlongs', 'Fathoms', NA)
                        ,FORMIMAGE=c('=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif\" , \"Thal\")'
                                    )
                        ,COMMENTS=rep("                                              ", 3)
                        ,stringsAsFactors=FALSE
                        )
#  rr$UID <- NULL
  checkEquals(expected, rr
             ,"Error: Did not correctly identify illegal values"
             )
  
}
# end of file
