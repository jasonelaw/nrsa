neightborBasedValidationTest <- function()
# unit test for neightborBasedValidation()
{
  testData <- data.frame('k1'=rep(1:4, each=10)
                        ,'k2'=rep(1:10, times=4)
                        ,'par'=rep('dist', times=40)
                        ,'val'=c(1, 10,  1,  1,  1, 10, 10,  1,  1, 10
                                ,10, 1,  1, 10,100, 10,  1, 10, 10, 10
                                ,NA, 1,  1,  0, 10,  1, 10,  1, NA, 10
                                ,1, NA,  1, NA,  1,  1,  0, 10,  0,  1
                                )
                        ,stringsAsFactors=FALSE
                        )

  # with full arguments
  rr <- neightborBasedValidation(testData, c('k1','k2'), 'par', 'val', 'dist', 5, 0, 7)
  rownames(rr) <- NULL
  ee <- subset(testData
              ,k1==1 & k2 %in% c(1,2,3,5,6,7,8,9,10) |
               k1==2 & k2 %in% c(1:8) |
               k1==3 & k2 %in% c(6,7,8,10)
              )
  ee$TESTDESCRIPTION <- 'Value varies considerably from its neighbors'
  ee[ee$k1==3 & ee$k2==10,]$TESTDESCRIPTION <- "Value exceeds specified range (0,7)"
  rownames(ee) <- NULL

  checkEquals(ee,rr
             ,'Error: Did not correctly detect odd neighboring values with range check'
             )

  # no static range checks
  rr <- neightborBasedValidation(testData, c('k1','k2'), 'par', 'val', 'dist', 5, NULL, NULL)
  rownames(rr) <- NULL
  ee <- subset(testData
              ,k1==1 & k2 %in% c(1,2,3,5,6,7,8,9,10) |
               k1==2 & k2 %in% c(1:8) |
               k1==3 & k2 %in% c(6,7,8)
              )
  ee$TESTDESCRIPTION <- 'Value varies considerably from its neighbors'
  rownames(ee) <- NULL

  checkEquals(ee,rr,
             'Error: Did not correctly detect odd neighboring values with no range check'
             )

}


# end of file