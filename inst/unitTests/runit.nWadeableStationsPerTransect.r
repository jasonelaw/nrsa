nWadeableStationsPerTransectTest <- function()
# tests nWadeableStationsPerTransect()
{
  fakeThal <- rbind(data.frame(UID=rep('std. stream A-K', 101)
                              ,TRANSECT=c(rep(LETTERS[1:10], each=10), 'K')
                              ,STATION=c(rep(0:9, 10), 0)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('std. stream A-J', 100)
                              ,TRANSECT=rep(LETTERS[1:10], each=10)
                              ,STATION=rep(0:9, 10)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('narrow stream A-J', 150)
                              ,TRANSECT=rep(LETTERS[1:10], each=15)
                              ,STATION=rep(0:14, 10)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w 2 long transects', 106)
                              ,TRANSECT=c(rep(LETTERS[1:8], each=10)
                                         ,rep(c('I','J'), each=13)
                                         )
                              ,STATION=c(rep(0:9, 8), 0:12, 0:12)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w 2 short transects', 98)
                              ,TRANSECT=c(rep(LETTERS[1:8], each=10)
                                         ,rep(c('I','J'), each=9)
                                         )
                              ,STATION=c(rep(0:9, 8), 0:8, 0:8)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w two modes', 100)
                              ,TRANSECT=c(rep(LETTERS[1:5], each=11)
                                         ,rep(LETTERS[6:10], each=9)
                                         )
                              ,STATION=c(rep(0:10, 5), rep(0:8, 5))
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   )


  expected <- rbind(data.frame(UID='std. stream A-K'
                              ,TRANSECT=LETTERS[1:11]
                              ,nSta=c(rep(10, 10), 1)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='std. stream A-J'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(10, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='narrow stream A-J'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(15, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w 2 long transects'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=c(rep(10, 8), 13, 13)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w 2 short transects'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(10, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w two modes'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=c(rep(11, 5), rep(9, 5))
                              ,stringsAsFactors=FALSE
                              )
                   )
  expected <- expected[order(expected$UID, expected$TRANSECT),]
  rownames(expected) <- NULL
 
  results <- nWadeableStationsPerTransect(fakeThal)
  results <- results[order(results$UID, results$TRANSECT),]
  rownames(results) <- NULL

  checkEquals(expected, results
             ,"Error: nWadeableStationsPerTransect is broken"
             )

}

# end of file
