# constructNRSAValidationUpdate.r
#
# 10/30/09 cws created
# 11/25/09 cws Updated unit test to include FLAG column in valResults.
# 12/15/09 cws No longer assumes UNITS is table column.  Orders columns in
#          output to match the table to be updated.  Changes UID back to
#          BATCHNO in return value.  Unit test updated accordingly.
#  1/06/10 cws Explicitely casting columns of valResults to the correct types
#          expected when writing
#  1/13/10 cws Now explicitly casting STATION as character; this was being
#          written as a floating point value which when implicitly cast as
#          nchar[10] during the write to the server resulted in values like 3.0
#  2/25/10 cws moved source() of db.r to NRSAvalidation.r
#  5/20/10 cws Fixed unit test, which previously compared only first column of
#          dataframes.  Changed casting of BATCHNO from character to int in
#          expected result.
#
require(RODBC)

constructNRSAValidationUpdate <- function(valResults, siteInfo, currentData)
# Prepares a dataframe for updating the specified table, based on the current
# contents in the database and the values in a validation dataframe.  This
# function is intended to be called as part of the process of updating a table
# with validated data.  It is like constructNRSAValidationResults() but goes
# in the opposite direction.
#
# Returns the prepared dataframe ready to be used as an update to the table,
# or a character string describing the problem if an error occurs.
#
# ARGUMENTS:
# valResults  dataframe with results of validation for a single table
# siteInfo    dataframe with site information, relating SITE_ID, DATE_COL and
#               VISIT_NO to UID
# currentData dataframe with current contents of the table being updated.
#
# ASSUMPTIONS:
# The validation results are based on the table being updated.
# Dates are formatted as YYYY-MM-DD in the validation results
#
{
  # Convert SITE_ID, DATE_COL, VISIT_NO back to UID using database table.
  siteResults <- merge(valResults
                      ,siteInfo[c('SITE_ID','DATE_COL','VISIT_NO','UID')]
                      ,by=c('SITE_ID','DATE_COL','VISIT_NO')
                      ,all.x=TRUE, all.y=FALSE
                      )

  # Determine keys used in table.  Retain only columns in validation results
  # which are keys or columns that may have been edited.
  keysUsed <- names(siteResults)[names(siteResults) %in% NRSAKeyColumns]

  siteResults <- siteResults[c(keysUsed, 'PARAMETER', 'RESULT'
                              ,na.omit(ifelse('UNITS' %in% names(siteResults)
                                             ,'UNITS', NA
                                             )
                                      )
                              ,'COMMENTS'
                              )
                            ]
  siteResults <- rename(siteResults, 'COMMENTS', 'REASON')

  # Fold in rows of current table that do not occur in the validation table,
  # overwriting old RESULTS (and UNITS) values with validated RESULTS (and
  # UNITS) values.
  updateData <- merge(currentData, siteResults, by=c(keysUsed, 'PARAMETER'),
                     ,all.x=FALSE, all.y=TRUE
                     )

  updateData$RESULT <- updateData$RESULT.y
  updateData <- subset(updateData, select=-c(RESULT.x, RESULT.y))

  if('UNITS' %in% names(currentData)) {
      updateData$UNITS <- updateData$UNITS.y
      updateData <- subset(updateData, select=-c(UNITS.x, UNITS.y))
  }
  
  # Rename UID back to BATCHNO.
  updateData <- rename(updateData, 'UID', 'BATCHNO')
  
  # Cast columns in update table to match target table.  All tables have
  # RESULT and BATCHNO, and handle special cases.
  if('BATCHNO' %in% names(updateData)) {
      updateData$BATCHNO <- as.integer(updateData$BATCHNO)
  }
  if('RESULT' %in% names(updateData)) {
      updateData$RESULT<- as.character(updateData$RESULT)
  }
  if('STATION' %in% names(updateData)) {
      updateData$STATION<- as.character(as.integer(updateData$STATION))
  }
  if('LINE' %in% names(updateData)) {
      # for tblCHANNELGEOMETRY2
      updateData$LINE<- as.character(updateData$LINE)
  }


  return(updateData)
}

constructNRSAValidationUpdateTest <- function()
# Tests constructNRSAValidationUpdate()
{
  valData <- data.frame(SITE_ID=paste('site', c(1,1,1,1,1,2), sep='')
                       ,VISIT_NO=c(1,1,1,2,2,1)
                       ,DATE_COL=c(rep('2008-4-1', 3)
                                  ,rep('2008-5-1', 2)
                                  ,'2008-4-2'
                                  )
                       ,TRANSECT=c('A','C','E','B','B','XJ')
                       ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                       ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                         ,rep('odd relationship', 2)
                                         ,'just odd'
                                         )
                       ,RESULT=c('one','two','three','four','five','six')
                       ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                       ,FLAG=c('',NA,'F1','','','')
                       ,COMMENTS=paste('Because I said so', 1:6)
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
  currentData <- expand.grid(UID=1:5
                            ,TRANSECT=c(LETTERS[1:11]
                                       ,paste('X', LETTERS[1:10], sep='')
                                       )
                            ,PARAMETER=paste('p', 1:10, sep='')
                            ,SAMPLE_TYPE=c('Type A','Type B')
                            ,RESULT=3.14
                            ,UNITS='furlongs'
                            )
  currentData$UID <- as.character(currentData$UID)
  currentData$TRANSECT <- as.character(currentData$TRANSECT)
  currentData$PARAMETER <- as.character(currentData$PARAMETER)
  currentData$SAMPLE_TYPE <- as.character(currentData$SAMPLE_TYPE)
  currentData$RESULT <- as.character(currentData$RESULT)
  currentData$UNITS <- as.character(currentData$UNITS)
  currentData <- subset(currentData, (PARAMETER %in% c('p1','p2','p3','p4','p5') &
                                      SAMPLE_TYPE == 'Type A'
                                     )
                                     |
                                     (PARAMETER %in% c('p6','p7','p8','p9','p10') &
                                      SAMPLE_TYPE == 'Type B'
                                     )
                       )

  expected <- data.frame(BATCHNO=c(1,1,1,2,2,3)
                        ,TRANSECT=c('A','C','E','B','B','XJ')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,SAMPLE_TYPE=rep('Type A', 6)
                        ,RESULT=c('one','two','three','four','five','six')
                        ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                        ,REASON=paste('Because I said so', 1:6)
                        ,stringsAsFactors=FALSE
                        )
  expected$BATCHNO <- as.integer(expected$BATCHNO)

  # Check if names are correct, as this is assumed in the next step when the
  # columns are reordered.
  rr <- constructNRSAValidationUpdate(valData, sites, currentData)
  checkTrue(all(names(expected) %in% names(rr)) &
            all(names(rr) %in% names(expected))
           ,"Error: Constructed validation update does not have right names"
           )

  # Reorder columns and rows of expected and actual results dataframes, as these
  # are inconsequential aspects of the function.
  expected2 <- expected[names(rr)]
  rr2 <-  rr[order(rr$BATCHNO, rr$TRANSECT, rr$PARAMETER),]
  rownames(rr2) <- NULL
  checkEquals(expected2, rr2
                ,"Error: Did not correctly construct validation update"
                )
}

# end of file