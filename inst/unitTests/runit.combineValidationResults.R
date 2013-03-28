combineValidationResultsTest <- function()
{
  a <- data.frame('uid'=c(1,2,3)
                 ,'a'=c(1,1,1)
                 ,'testDescription'=c('a','a','a')
                 ,stringsAsFactors=FALSE
                 )
  b <- data.frame('uid'=c(1,3,4)
                 ,'a'=c(2,2,2)
                 ,'testDescription'=c('b','b','b')
                 ,stringsAsFactors=FALSE
                 )
  abTrue <- data.frame('uid'=c(1,2,3,4)
                      ,testDescription=c('a; b', 'a', 'a; b', 'b')
                      ,stringsAsFactors=FALSE
                      )
  ab <- combineValidationResults(a,b, 'uid')
  identical(ab,abTrue)
}