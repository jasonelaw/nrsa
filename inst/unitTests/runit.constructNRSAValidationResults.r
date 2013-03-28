constructNRSAValidationResultsTest <- function()
# Tests constructNRSAValidationResults()
{
  constructNRSAValidationResultsTest.1() # Test with one intra site key and FLAG
  constructNRSAValidationResultsTest.2() # Test with three intra site keys, no FLAG
  constructNRSAValidationResultsTest.3() # Test with zero intra site keys and FLAG
  constructNRSAValidationResultsTest.4() # Test with wadeable & boatable ChRip data
  constructNRSAValidationResultsTest.5() # Test with one intra site key and FLAG
                                         # and prepared for Open Office Calc.
}

constructNRSAValidationResultsTest.1 <- function()
# Tests constructNRSAValidationResults() with one intrasite key, one data year
# and no UNITS column.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANLINE=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "
  
  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with one intrasite key"
             )
}

constructNRSAValidationResultsTest.2 <- function()
# Tests constructNRSAValidationResults() with three intrasite keys, UNITS column
# and multiple years.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANSECT=c('A','C','E','B','B','XJ')
                        ,TRANSDIR=c('LF','LC','CT','RC','RC','RT')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,ANALYTE=c('CaO','CaW','CrOW','AgCH3','AgCH3','Hg')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste(c('2008','2009'), 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,format(as.POSIXct(expected$DATE_COL), '%Y')
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANSECT','TRANSDIR'
                        ,'ANALYTE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)

  checkIdentical(expected, rr
    ,"Error: Incorrect construction of validation results with three intrasite keys"
    )
}

constructNRSAValidationResultsTest.3 <- function()
# Tests constructNRSAValidationResults() with zero intrasite keys, UNITS column
# and multiple years.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,UNITS='furlongs'
                        ,SAMPLE_TYPE='test'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste(c('2008','2009','2010'), 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,format(as.POSIXct(expected$DATE_COL), '%Y')
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  # Insert spacer lines to visually distinguish separate issues.  Note minor
  # reordering of rows, as without intrasite keys, TESTDESCRIPTION now affects
  # ordering.
  expected <- rbind(expected[1,]
                   ,expected[3,]
                   ,expected[2,]
                   ,expected[4,]
                   ,expected[5,]
                   ,expected[6,]
                   )

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
    ,"Error: Incorrect construction of validation results with zero intrasite keys"
    )
}

constructNRSAValidationResultsTest.4 <- function()
# Tests constructNRSAValidationResults() with wadeable and boatable ChRip data
# in both 2008 and 2009
{
  baseData <- data.frame(UID=c('1','2','3','4','4','4')
                        ,TRANSECT=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE=c(rep('PHAB_CHANW',2)
                                      ,rep('PHAB_CHANB',4)
                                      )
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c("CANBTRE", "CANSTRE"
                                    ,"CANVEG", "CANBTRE", "CANSTRE", "SHOREDOM"
                                    )
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=c("CANBTRE", "CANSTRE", "CANVEG"
                                    ,"CANBTRE", "CANSTRE", "SHOREDOM"
                                    )
                        ,FORMABBR='ChRip'
                        ,SAMPLE_TYPE=c(rep('PHAB_CHANW',2)
                                      ,rep('PHAB_CHANB',4)
                                      )
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:40)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:20, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, each=2, times=10)
                     ,DATE_COL=as.POSIXct(paste(c('2008','2009')
                                               ,4:5
                                               ,rep(1:20, each=2)
                                               ,sep='-'
                                               )
                                         )
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  # Create 'expected' dataframe
  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  tt <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)

  tt1 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANW')             # wadeable data
  tt2 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANB' & UID=='3')  # boatable data 2008
  tt3 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANB' & UID=='4')  # boatable data 2009
  tt1$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt1$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt1$FORMABBR
                              ,tt1$SITE_ID
                              ,paste('V', tt1$VISIT_NO, sep='')
                              ,tt1$TRANSECT
                              ,sep='_'
                              )
                        ,'.tif'
                        , '" , "', tt1$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  tt2$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt2$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt2$FORMABBR
                              ,tt2$SITE_ID
                              ,paste('V', tt2$VISIT_NO, sep='')
                              ,tt2$TRANSECT
                              ,sep='_'
                              )
                        ,'.tif'
                        , '" , "', tt2$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  tt3$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt3$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt3$FORMABBR
                              ,tt3$SITE_ID
                              ,paste('V', tt3$VISIT_NO, sep='')
                              ,tt3$TRANSECT
                              ,c('P2','P2','P1')
                              ,sep='_'
                              )
                        ,'.tif'
                        ,'" , "', tt3$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  expected <- rbind(tt1, tt2, tt3)
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANSECT','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with ChRip-like data"
             )
}

constructNRSAValidationResultsTest.5 <- function()
# Tests constructNRSAValidationResults() with one intrasite key, one data year
# and no UNITS column, and prepared for Open Office Calc instead of Excel.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANLINE=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" ; "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites, ssFmt='OO')
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with one intrasite key formatted for OO Calc"
             )
}

# end of file
