interpolatePercentileTest <- function()
# Tests interpolatePercentile()
{
  # Define test data and class boundaries.
  sizes <- data.frame('CLASS'=c('FN','SA','GF','GC','CB','SB','XB')
                     ,'min'=c(0.001, 0.06,2,   16,  64,  250, 1000)
                     ,'max'=c(0.06,  2,   16,  64,  250, 1000,4000)
                     ,stringsAsFactors=FALSE
                     )
  sizes$min <- log10(sizes$min)
  sizes$max <- log10(sizes$max)

  testData <- interpolatePercentile.testData()
  testData <- subset(testData
                    ,SIZE_CLS %in% c('FN','SA','GF','GC','CB','SB','XB')
                    )
  expectedResults <- interpolatePercentile.expectedResults()

  # calculate the values for which we have expected results
  c16 <- interpolatePercentile(testData, 'SIZE_CLS', 16, 'lsub2d16InoR', sizes)
  c50 <- interpolatePercentile(testData, 'SIZE_CLS', 50, 'lsub2d50InoR', sizes)
  c84 <- interpolatePercentile(testData, 'SIZE_CLS', 84, 'lsub2d84InoR', sizes)
  calcs <- merge(c16
                ,merge(c50, c84, by='UID', all=TRUE)
                ,by='UID'
                ,all=TRUE
                )

  # Compare results, which should be within a nominal calculation error
  errs <- dfCompare(expectedResults, calcs, 'UID', zeroFudge=1e-8)
  checkEquals(NULL, errs
              ,"Error: interpolatePercentile() results are incorrect"
              )
}


interpolatePercentile.testData <- function()
# Creates the dataframe of EMAP substrate data used by interpolatePercentileTest()
# Values taken from WEMAP:
# 2002 WAZP99-0590 1    Has FN and others but no SA
# 2002 WCOP01-0725 1    Has single class (FN)
# 2002 WNDP02-R001 1    Has two disparate classes FN and CB
# 2002 WORP99-0866 1    Has all classes present
{
  testData <-read.table(textConnection(
                  "UID TRANSECT TRANSDIR SIZE_CLS
                  '2002 WAZP99-0590 1' A CT CB
                  '2002 WAZP99-0590 1' A LC CB
                  '2002 WAZP99-0590 1' A LF FN
                  '2002 WAZP99-0590 1' A RC SB
                  '2002 WAZP99-0590 1' A RT FN
                  '2002 WAZP99-0590 1' A mm GF
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm SB
                  '2002 WAZP99-0590 1' B CT CB
                  '2002 WAZP99-0590 1' B LC CB
                  '2002 WAZP99-0590 1' B LF FN
                  '2002 WAZP99-0590 1' B RC SB
                  '2002 WAZP99-0590 1' B RT GC
                  '2002 WAZP99-0590 1' B mm GF
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm RS
                  '2002 WAZP99-0590 1' C CT CB
                  '2002 WAZP99-0590 1' C LC XB
                  '2002 WAZP99-0590 1' C LF GF
                  '2002 WAZP99-0590 1' C RC SB
                  '2002 WAZP99-0590 1' C RT FN
                  '2002 WAZP99-0590 1' C mm RS
                  '2002 WAZP99-0590 1' C mm SB
                  '2002 WAZP99-0590 1' C mm CB
                  '2002 WAZP99-0590 1' C mm CB
                  '2002 WAZP99-0590 1' C mm FN
                  '2002 WAZP99-0590 1' D CT CB
                  '2002 WAZP99-0590 1' D LC CB
                  '2002 WAZP99-0590 1' D LF FN
                  '2002 WAZP99-0590 1' D RC XB
                  '2002 WAZP99-0590 1' D RT GF
                  '2002 WAZP99-0590 1' D mm FN
                  '2002 WAZP99-0590 1' D mm CB
                  '2002 WAZP99-0590 1' D mm SB
                  '2002 WAZP99-0590 1' D mm CB
                  '2002 WAZP99-0590 1' D mm GC
                  '2002 WAZP99-0590 1' E CT SB
                  '2002 WAZP99-0590 1' E LC CB
                  '2002 WAZP99-0590 1' E LF GF
                  '2002 WAZP99-0590 1' E RC CB
                  '2002 WAZP99-0590 1' E RT GC
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm SB
                  '2002 WAZP99-0590 1' E mm FN
                  '2002 WAZP99-0590 1' F CT CB
                  '2002 WAZP99-0590 1' F LC CB
                  '2002 WAZP99-0590 1' F LF FN
                  '2002 WAZP99-0590 1' F RC CB
                  '2002 WAZP99-0590 1' F RT FN
                  '2002 WAZP99-0590 1' F mm FN
                  '2002 WAZP99-0590 1' F mm CB
                  '2002 WAZP99-0590 1' F mm GC
                  '2002 WAZP99-0590 1' F mm CB
                  '2002 WAZP99-0590 1' F mm FN
                  '2002 WAZP99-0590 1' G CT CB
                  '2002 WAZP99-0590 1' G LC CB
                  '2002 WAZP99-0590 1' G LF FN
                  '2002 WAZP99-0590 1' G RC CB
                  '2002 WAZP99-0590 1' G RT RS
                  '2002 WAZP99-0590 1' G mm FN
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm FN
                  '2002 WAZP99-0590 1' H CT CB
                  '2002 WAZP99-0590 1' H LC GC
                  '2002 WAZP99-0590 1' H LF GC
                  '2002 WAZP99-0590 1' H RC CB
                  '2002 WAZP99-0590 1' H RT FN
                  '2002 WAZP99-0590 1' H mm CB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' H mm XB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' I CT CB
                  '2002 WAZP99-0590 1' I LC CB
                  '2002 WAZP99-0590 1' I LF FN
                  '2002 WAZP99-0590 1' I RC CB
                  '2002 WAZP99-0590 1' I RT GC
                  '2002 WAZP99-0590 1' I mm FN
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm FN
                  '2002 WAZP99-0590 1' J CT GC
                  '2002 WAZP99-0590 1' J LC CB
                  '2002 WAZP99-0590 1' J LF RS
                  '2002 WAZP99-0590 1' J RC CB
                  '2002 WAZP99-0590 1' J RT GC
                  '2002 WAZP99-0590 1' J mm RS
                  '2002 WAZP99-0590 1' J mm CB
                  '2002 WAZP99-0590 1' J mm CB
                  '2002 WAZP99-0590 1' J mm GC
                  '2002 WAZP99-0590 1' J mm FN
                  '2002 WAZP99-0590 1' K CT XB
                  '2002 WAZP99-0590 1' K LC XB
                  '2002 WAZP99-0590 1' K LF FN
                  '2002 WAZP99-0590 1' K RC CB
                  '2002 WAZP99-0590 1' K RT FN
                  '2002 WCOP01-0725 1' A CT FN
                  '2002 WCOP01-0725 1' A LC FN
                  '2002 WCOP01-0725 1' A LF FN
                  '2002 WCOP01-0725 1' A RC FN
                  '2002 WCOP01-0725 1' A RT FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' B CT FN
                  '2002 WCOP01-0725 1' B LC FN
                  '2002 WCOP01-0725 1' B LF FN
                  '2002 WCOP01-0725 1' B RC FN
                  '2002 WCOP01-0725 1' B RT FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' C CT FN
                  '2002 WCOP01-0725 1' C LC FN
                  '2002 WCOP01-0725 1' C LF FN
                  '2002 WCOP01-0725 1' C RC FN
                  '2002 WCOP01-0725 1' C RT FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' D CT FN
                  '2002 WCOP01-0725 1' D LC FN
                  '2002 WCOP01-0725 1' D LF FN
                  '2002 WCOP01-0725 1' D RC FN
                  '2002 WCOP01-0725 1' D RT FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' E CT FN
                  '2002 WCOP01-0725 1' E LC FN
                  '2002 WCOP01-0725 1' E LF FN
                  '2002 WCOP01-0725 1' E RC FN
                  '2002 WCOP01-0725 1' E RT FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' F CT FN
                  '2002 WCOP01-0725 1' F LC FN
                  '2002 WCOP01-0725 1' F LF FN
                  '2002 WCOP01-0725 1' F RC FN
                  '2002 WCOP01-0725 1' F RT FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' G CT FN
                  '2002 WCOP01-0725 1' G LC FN
                  '2002 WCOP01-0725 1' G LF FN
                  '2002 WCOP01-0725 1' G RC FN
                  '2002 WCOP01-0725 1' G RT FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' H CT FN
                  '2002 WCOP01-0725 1' H LC FN
                  '2002 WCOP01-0725 1' H LF FN
                  '2002 WCOP01-0725 1' H RC FN
                  '2002 WCOP01-0725 1' H RT FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' I CT FN
                  '2002 WCOP01-0725 1' I LC FN
                  '2002 WCOP01-0725 1' I LF FN
                  '2002 WCOP01-0725 1' I RC FN
                  '2002 WCOP01-0725 1' I RT FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' J CT FN
                  '2002 WCOP01-0725 1' J LC FN
                  '2002 WCOP01-0725 1' J LF FN
                  '2002 WCOP01-0725 1' J RC FN
                  '2002 WCOP01-0725 1' J RT FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' K CT FN
                  '2002 WCOP01-0725 1' K LC FN
                  '2002 WCOP01-0725 1' K LF FN
                  '2002 WCOP01-0725 1' K RC FN
                  '2002 WCOP01-0725 1' K RT FN
                  '2002 WNDP02-R001 1' A CT FN
                  '2002 WNDP02-R001 1' A LC FN
                  '2002 WNDP02-R001 1' A LF FN
                  '2002 WNDP02-R001 1' A RC FN
                  '2002 WNDP02-R001 1' A RT FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' B CT FN
                  '2002 WNDP02-R001 1' B LC FN
                  '2002 WNDP02-R001 1' B LF FN
                  '2002 WNDP02-R001 1' B RC FN
                  '2002 WNDP02-R001 1' B RT FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' C CT FN
                  '2002 WNDP02-R001 1' C LC FN
                  '2002 WNDP02-R001 1' C LF FN
                  '2002 WNDP02-R001 1' C RC FN
                  '2002 WNDP02-R001 1' C RT FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' D CT FN
                  '2002 WNDP02-R001 1' D LC FN
                  '2002 WNDP02-R001 1' D LF FN
                  '2002 WNDP02-R001 1' D RC FN
                  '2002 WNDP02-R001 1' D RT FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' E CT FN
                  '2002 WNDP02-R001 1' E LC FN
                  '2002 WNDP02-R001 1' E LF FN
                  '2002 WNDP02-R001 1' E RC FN
                  '2002 WNDP02-R001 1' E RT HP
                  '2002 WNDP02-R001 1' E mm FN
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' F CT CB
                  '2002 WNDP02-R001 1' F LC CB
                  '2002 WNDP02-R001 1' F LF CB
                  '2002 WNDP02-R001 1' F RC CB
                  '2002 WNDP02-R001 1' F RT CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' G CT CB
                  '2002 WNDP02-R001 1' G LC CB
                  '2002 WNDP02-R001 1' G LF HP
                  '2002 WNDP02-R001 1' G RC CB
                  '2002 WNDP02-R001 1' G RT CB
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm FN
                  '2002 WNDP02-R001 1' H CT HP
                  '2002 WNDP02-R001 1' H LC HP
                  '2002 WNDP02-R001 1' H LF HP
                  '2002 WNDP02-R001 1' H RC HP
                  '2002 WNDP02-R001 1' H RT FN
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' I CT OT
                  '2002 WNDP02-R001 1' I LC FN
                  '2002 WNDP02-R001 1' I LF HP
                  '2002 WNDP02-R001 1' I RC HP
                  '2002 WNDP02-R001 1' I RT FN
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' J CT HP
                  '2002 WNDP02-R001 1' J LC HP
                  '2002 WNDP02-R001 1' J LF FN
                  '2002 WNDP02-R001 1' J RC HP
                  '2002 WNDP02-R001 1' J RT FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' K CT FN
                  '2002 WNDP02-R001 1' K LC FN
                  '2002 WNDP02-R001 1' K LF FN
                  '2002 WNDP02-R001 1' K RC FN
                  '2002 WNDP02-R001 1' K RT FN
                  '2002 WORP99-0866 1' A CT GC
                  '2002 WORP99-0866 1' A LC GC
                  '2002 WORP99-0866 1' A LF GC
                  '2002 WORP99-0866 1' A RC GC
                  '2002 WORP99-0866 1' A RT GC
                  '2002 WORP99-0866 1' A mm SA
                  '2002 WORP99-0866 1' A mm GF
                  '2002 WORP99-0866 1' A mm GC
                  '2002 WORP99-0866 1' A mm RR
                  '2002 WORP99-0866 1' A mm RR
                  '2002 WORP99-0866 1' B CT CB
                  '2002 WORP99-0866 1' B LC GC
                  '2002 WORP99-0866 1' B LF GC
                  '2002 WORP99-0866 1' B RC GC
                  '2002 WORP99-0866 1' B RT CB
                  '2002 WORP99-0866 1' B mm GC
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' C CT GF
                  '2002 WORP99-0866 1' C LC GC
                  '2002 WORP99-0866 1' C LF CB
                  '2002 WORP99-0866 1' C RC CB
                  '2002 WORP99-0866 1' C RT FN
                  '2002 WORP99-0866 1' C mm GC
                  '2002 WORP99-0866 1' C mm SB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' D CT GF
                  '2002 WORP99-0866 1' D LC SB
                  '2002 WORP99-0866 1' D LF CB
                  '2002 WORP99-0866 1' D RC CB
                  '2002 WORP99-0866 1' D RT SB
                  '2002 WORP99-0866 1' D mm FN
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' E CT SB
                  '2002 WORP99-0866 1' E LC CB
                  '2002 WORP99-0866 1' E LF XB
                  '2002 WORP99-0866 1' E RC GF
                  '2002 WORP99-0866 1' E RT XB
                  '2002 WORP99-0866 1' E mm SB
                  '2002 WORP99-0866 1' E mm CB
                  '2002 WORP99-0866 1' E mm OT
                  '2002 WORP99-0866 1' E mm CB
                  '2002 WORP99-0866 1' E mm SB
                  '2002 WORP99-0866 1' F CT GC
                  '2002 WORP99-0866 1' F LC GC
                  '2002 WORP99-0866 1' F LF FN
                  '2002 WORP99-0866 1' F RC CB
                  '2002 WORP99-0866 1' F RT XB
                  '2002 WORP99-0866 1' F mm XB
                  '2002 WORP99-0866 1' F mm CB
                  '2002 WORP99-0866 1' F mm FN
                  '2002 WORP99-0866 1' F mm CB
                  '2002 WORP99-0866 1' F mm OT
                  '2002 WORP99-0866 1' G CT GC
                  '2002 WORP99-0866 1' G LC XB
                  '2002 WORP99-0866 1' G LF FN
                  '2002 WORP99-0866 1' G RC SB
                  '2002 WORP99-0866 1' G RT XB
                  '2002 WORP99-0866 1' G mm CB
                  '2002 WORP99-0866 1' G mm GF
                  '2002 WORP99-0866 1' G mm SB
                  '2002 WORP99-0866 1' G mm CB
                  '2002 WORP99-0866 1' G mm GC
                  '2002 WORP99-0866 1' H CT CB
                  '2002 WORP99-0866 1' H LC CB
                  '2002 WORP99-0866 1' H LF XB
                  '2002 WORP99-0866 1' H RC SA
                  '2002 WORP99-0866 1' H RT SB
                  '2002 WORP99-0866 1' H mm XB
                  '2002 WORP99-0866 1' H mm GF
                  '2002 WORP99-0866 1' H mm CB
                  '2002 WORP99-0866 1' H mm CB
                  '2002 WORP99-0866 1' H mm FN
                  '2002 WORP99-0866 1' I CT GC
                  '2002 WORP99-0866 1' I LC CB
                  '2002 WORP99-0866 1' I LF FN
                  '2002 WORP99-0866 1' I RC GC
                  '2002 WORP99-0866 1' I RT SB
                  '2002 WORP99-0866 1' I mm SA
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' I mm XB
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' J CT SA
                  '2002 WORP99-0866 1' J LC FN
                  '2002 WORP99-0866 1' J LF XB
                  '2002 WORP99-0866 1' J RC CB
                  '2002 WORP99-0866 1' J RT XB
                  '2002 WORP99-0866 1' J mm FN
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GC
                  '2002 WORP99-0866 1' K CT GF
                  '2002 WORP99-0866 1' K LC GF
                  '2002 WORP99-0866 1' K LF OT
                  '2002 WORP99-0866 1' K RC GC
                  '2002 WORP99-0866 1' K RT OT
                  "
                  ), header=TRUE, stringsAsFactors=FALSE
                  )
  return(testData)
}

interpolatePercentile.expectedResults <- function()
# Creates the results dataframe used by interpolatePercentileTest()
# Values taken from WEMAP
# 2002 WAZP99-0590 1    Has FN and others but no SA
# 2002 WCOP01-0725 1    Has single class (FN)
# 2002 WNDP02-R001 1    Has two disparate classes FN and CB
# 2002 WORP99-0866 1    Has all classes present
{
  testResults <-read.table(textConnection(
                  "UID lsub2d16InoR lsub2d50InoR lsub2d84InoR
                  '2002 WAZP99-0590 1' -1.706799091   1.9698582815  2.3979400087
                  '2002 WCOP01-0725 1' -2.7154958    -2.110924375  -1.50635295
                  '2002 WNDP02-R001 1' -2.650199754  -1.906874231   1.8907171218
                  '2002 WORP99-0866 1'  0.5341914105  1.9019058619  2.7086029642
                  "
                  ), header=TRUE, stringsAsFactors=FALSE
                  )
  return(testResults)
}

# end of file