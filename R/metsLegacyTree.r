## metsLegacyTree.r
##
## Created SSR 01/29/2010
#  02/18/10 cws removed source() of NRSAValidation.r
#  03/22/10 cws added missing calls to checkEquals in unit test!
#   3/25/10 cws Changed diff() calls to dfCompare().

require(reshape2)

metsLegacyTree <- function()
## Creating mhtree.csv which contains the metrics:
# ltmxcnt -  Legacy number of largest trees
# ltmxdbh -  Legacy largest tree dbh
# ltmxdist -  Legacy largest tree distance
# ltmxht -  Legacy largest tree height
# ltmxsize -  Legacy largest tree size class (SMLX)
# ltmxspp -  Legacy largest tree species
# ltsplist -  Legacy tree species list comma delim.
# ltfracs - Legacy fraction of reach trees >= small
# ltfracm - Legacy fraction of reach trees >= medium
# ltfracl - Legacy fraction of reach trees >= large
# ltfracx - Legacy fraction of reach trees >= Xlarge
# ltmddist - Legacy mean dist of trees >= median size
# ltmddom - Legacy dominant sp.
# ltmddomn - Legacy dominant sp. Count
# ltmdsub -  Legacy subdominant sp. >= median size
# ltmdsubn -  Legacy subdominant sp. count

{
intermediateMessage('Legacy tree metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')
on.exit(close(chan))

##  Reading in data from tblINVASIVES2
chan <- odbcConnect('NRSA2')
tblIL <- fetchNRSATable(chan,'tblINVASIVELEGACY2')
df1 <- subset (tblIL, PARAMETER %in% c('TREE_TYP', 'SPECIES', 'DBH',
              'HEIGHT', 'DISTANCE'), 
              select=c('UID','TRANSECT','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsLegacyTree.1', loc='end')
mets <- metsLegacyTree.1(df1)

## Write the results to metsLegacyTree.csv
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsLegacyTree.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsLegacyTree.1 <- function(df1)
{
##  Calculating metrics

##  Renaming PARAMETER and RESULT to variable and value
df1 <-rename(df1, c('PARAMETER', 'RESULT'), c('variable', 'value'))

# Casting (transforming) data using reshape package
lt <- as.data.frame(dcast(df1, UID + TRANSECT ~ variable))
lt$DBH      <- as.character(lt$DBH)
#lt$NOT_VIS  <- as.character(lt$NOT_VIS)
lt$SPECIES  <- as.character(lt$SPECIES)
lt$TREE_TYP <- as.character(lt$TREE_TYP)
lt$HEIGHT   <- as.character(lt$HEIGHT)
lt$DISTANCE <- as.character(lt$DISTANCE)
lt$DISTANCE <- as.numeric(lt$DISTANCE)

lt$METRIC <- ''
lt$numdbh  <- NA 
lt$numht   <- NA 
lt$size    <- ''
lt$sizenum <- NA
lt$sizen   <- NA 

#  Creating values of DBH and HEIGHT with simpler codes
#  newDBH: 0-0.1 = 1; .1-.3 = 2; .3-.75 = 3; .75-2=4; >2=5
#  newHEIGHT: <5 = 1; 5-15 = 2; 15-30 = 3; >30 = 4
## Matrices for tree size
##
##  Determining sizen
##                        DBH
##              <5     5-15  15-30   >30
##   HEIGHT
##     0-0.1     1      2      3      4
##
##   0.1-0.3     2      3      4      5
##
##  0.3-0.75     3      4      5      6
##
##    0.75-2     4      5      6      7
##
##        >2     5      6      7      8
##
##
##  Determining size
##                        DBH
##              <5     5-15  15-30   >30
##   HEIGHT
##     0-0.1     S      S      M      M
##
##   0.1-0.3     S      M      M      M
##
##  0.3-0.75     M      M      L      L
##
##    0.75-2     L      L      L      X
##
##        >2     L      X      X      X
##

lt$numdbh <- ifelse(lt$DBH=='0-0.1', 1, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.1-.3', 2, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.3-.75', 3, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.75-2', 4, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='>2', 5, lt$numdbh)

lt$numht <- ifelse(lt$HEIGHT=='<5', 1, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='5-15', 2, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='15-30', 3, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='>30', 4, lt$numht)

lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'S', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & lt$HEIGHT=='<5', 'S', lt$size)
lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='5-15', 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='>30'), 'X', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'X', lt$size)

lt$sizenum <- ifelse(lt$size=='S', 1, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='M', 2, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='L', 3, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='X', 4, lt$sizenum)
 
lt$sizen <- ifelse(lt$DBH=='0-0.1' & lt$HEIGHT=='<5', 1, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='<5'), 2, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='15-30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='<5'), 3, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='<5'), 4, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.1-.3' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='<5'), 5, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.3-.75' & lt$HEIGHT=='>30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='5-15'), 6, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.75-2' & lt$HEIGHT=='>30')
                 | (lt$DBH=='>2' & lt$HEIGHT=='15-30'), 7, lt$sizen)
lt$sizen <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='>30', 8, lt$sizen)

###  Assigning taxonomic category based on crew entry
lt <- assignTaxCat(lt)

###  Determining largest tree
#  Getting max of sizen
aa <- subset(lt, !(is.na(sizen)), select=c('UID','sizen'))
##  This is where the warnings occur if there are no non-missing arguments to max 
bb <- aggregate(list(maxSizen=aa$sizen), list(UID=aa$UID), max)
cc <- subset(merge(bb, lt), maxSizen==sizen)
dd <- first(cc, 'UID', 'first.UID')
ee <- subset(dd, first.UID=='TRUE')

## NOTE:  Metrics for ltmxcnt procuced by SAS appear to count all trees
##        at a site, not just those with max sizen
# Getting counts of max sizen
ff <- aggregate(list(ltmxcnt=cc$sizen),list(UID=cc$UID),count)
gg <- merge(ee,ff)
ltMxMets <- rename(gg, c('DBH','HEIGHT','taxCat','size','DISTANCE'),
             c('ltmxdbh','ltmxht','ltmxspp','ltmxsize','ltmxdist'))
             
ltmxdbh  <- subset(ltMxMets, select=c('UID','METRIC','ltmxdbh'))
  ltmxdbh$METRIC <- 'ltmxdbh'
  ltmxdbh <- rename(ltmxdbh,'ltmxdbh','RESULT')
ltmxht  <- subset(ltMxMets, select=c('UID','METRIC','ltmxht'))             
  ltmxht$METRIC <- 'ltmxht'
  ltmxht <- rename(ltmxht,'ltmxht','RESULT')
ltmxspp  <- subset(ltMxMets, select=c('UID','METRIC','ltmxspp'))
  ltmxspp$METRIC <- 'ltmxspp'
  ltmxspp <- rename(ltmxspp,'ltmxspp','RESULT')
ltmxsize  <- subset(ltMxMets, select=c('UID','METRIC','ltmxsize'))
  ltmxsize$METRIC <- 'ltmxsize'
  ltmxsize <- rename(ltmxsize,'ltmxsize','RESULT')
ltmxdist  <- subset(ltMxMets, select=c('UID','METRIC','ltmxdist'))
  ltmxdist$METRIC <- 'ltmxdist'
  ltmxdist <- rename(ltmxdist,'ltmxdist','RESULT')
ltmxcnt  <- subset(ltMxMets, select=c('UID','METRIC','ltmxcnt'))
  ltmxcnt$METRIC <- 'ltmxcnt'
  ltmxcnt <- rename(ltmxcnt,'ltmxcnt','RESULT')

#### Determining median tree size and related metrics
# ltfracl - Legacy fraction of trees >= large
# ltfracm - Legacy fraction of trees >= medium
# ltfracs - Legacy fraction of trees >= small
# ltfracx - Legacy fraction of trees >= xlarge
# ltmddist - Legacy mean dist of trees >= median size

## NOTE:  SAS code used median DBH as median tree size.
## NOTE:  SAS code created counts for ltfracs, ltfracm, ltfracl, ltfracx.

## ltmddist
aa <- subset(aggregate(list(medianSize=lt$numdbh), list(UID=lt$UID),
             na.rm = T, median), !(is.na(medianSize)))
lt <- merge(lt, aa, all.x=T)
lt$medianDistance <- ifelse(lt$numdbh>=lt$medianSize, lt$DISTANCE, NA)
ltmddist <- aggregate(list(ltmddist=lt$medianDistance),
               list(UID=lt$UID,METRIC=lt$METRIC), mean, na.rm = T)
  ltmddist$METRIC <- 'ltmddist'
  ltmddist <- rename(ltmddist, 'ltmddist', 'RESULT')

## fractions of trees by size
# cntS - count of trees >= small -> s, m, l, x
# cntM - count of trees >= medium -> m, l, x
# cntL - count of trees >= large -> l, x
# cntX - count of trees >= xlarge  -> x

## Creating count of transects by UID
tranCnt <- subset(aggregate(list(tranCnt=lt$TRANSECT), by=list(UID=lt$UID), count), select=c('UID', 'tranCnt'))

sm <- aggregate(list(smCnt=subset(lt$size, lt$size %in% c('S','M','L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('S','M','L','X'))), count)
med <- aggregate(list(medCnt=subset(lt$size, lt$size %in% c('M','L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('M','L','X'))), count)
lg <- aggregate(list(lgCnt=subset(lt$size, lt$size %in% c('L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('L','X'))), count)
xl <- aggregate(list(xlCnt=subset(lt$size, lt$size %in% c('X'))),
        list(UID=subset(lt$UID,lt$size %in% c('X'))), count)

aa <- merge(sm, med, all=T)
bb <- merge(aa, lg, all=T)
cc <- merge(bb, xl, all=T)
treeCounts <- merge(tranCnt, cc, all=T)
treeCounts$ltfracs <- treeCounts$smCnt/treeCounts$tranCnt
treeCounts$ltfracm <- treeCounts$medCnt/treeCounts$tranCnt
treeCounts$ltfracl <- treeCounts$lgCnt/treeCounts$tranCnt
treeCounts$ltfracx <- treeCounts$xlCnt/treeCounts$tranCnt

treeCounts$ltfracs <- ifelse(is.na(treeCounts$ltfracs), 0, treeCounts$ltfracs)
treeCounts$ltfracm <- ifelse(is.na(treeCounts$ltfracm), 0, treeCounts$ltfracm)
treeCounts$ltfracl <- ifelse(is.na(treeCounts$ltfracl), 0, treeCounts$ltfracl)
treeCounts$ltfracx <- ifelse(is.na(treeCounts$ltfracx), 0, treeCounts$ltfracx)

treeCounts$METRIC <- ''

ltfracs <- subset(treeCounts, select=c('UID','METRIC','ltfracs'))
  ltfracs$METRIC <- 'ltfracs'
  ltfracs <- rename(ltfracs,'ltfracs','RESULT')
ltfracm <- subset(treeCounts, select=c('UID','METRIC','ltfracm'))
  ltfracm$METRIC <- 'ltfracm'
  ltfracm <- rename(ltfracm,'ltfracm','RESULT')
ltfracl <- subset(treeCounts, select=c('UID','METRIC','ltfracl'))
  ltfracl$METRIC <- 'ltfracl'
  ltfracl <- rename(ltfracl,'ltfracl','RESULT')
ltfracx <- subset(treeCounts, select=c('UID','METRIC','ltfracx'))
  ltfracx$METRIC <- 'ltfracx'
  ltfracx <- rename(ltfracx,'ltfracx','RESULT')

### And now for the piece de resistance - the list of dominant and subdominant species
# ltsplist - List of all speicies present
# ltmddom - Species occurring most commonly
# ltmddomn - Number of times dominant speices occurs
# ltmdsub - Second most commonly occurring species
# ltmdsubn - Number of tims subdominant species occurs

## NOTE:  SAS code miscounts ltmddomn and ltmdsubn by one tree in each metric.
                     
##  Creating ltmddom and ltmdsub
counts <- aggregate(list(count=lt$taxCat), list('UID'=lt$UID,'METRIC'=lt$METRIC, 'maxTaxCat'=lt$taxCat), count)
ltmddomn <- aggregate(list('ltmddomn'=counts$count), list('UID'=counts$UID, METRIC=counts$METRIC), max)
  ltmddomn$METRIC <- 'ltmddomn'
  ltmddomn <- rename(ltmddomn,'ltmddomn','RESULT')
  
aa <- subset(merge(counts, subset(ltmddomn,select=c('UID','RESULT')), by='UID'), count==RESULT)
ltmddom <- aggregate(list('ltmddom'=aa$maxTaxCat)
                        ,list('UID'=aa$UID,'METRIC'=aa$METRIC)
                        ,function(x) { paste(x, collapse=',') } )
  ltmddom$METRIC <- 'ltmddom'
  ltmddom <- rename(ltmddom,'ltmddom','RESULT')
                     
bb <- subset(merge(counts, subset(ltmddomn, select=c('UID','RESULT')),
        by='UID'), count!=RESULT, select=c('UID','METRIC','maxTaxCat','count'))                      
ltmdsubn <- aggregate(list('ltmdsubn'=bb$count), list('UID'=bb$UID, METRIC=bb$METRIC), max)
  ltmdsubn$METRIC <- 'ltmdsubn'
  ltmdsubn <- rename(ltmdsubn,'ltmdsubn','RESULT')
cc <- subset(merge(counts, subset(ltmdsubn,select=c('UID','RESULT')), by='UID'), count==RESULT)
ltmdsub <- aggregate(list('ltmdsub'=cc$maxTaxCat)
                        ,list('UID'=cc$UID,'METRIC'=cc$METRIC)
                        ,function(x) { paste(x, collapse=',')})
  ltmdsub$METRIC <- 'ltmdsub'
  ltmdsub <- rename(ltmdsub,'ltmdsub','RESULT')

##  List of all trees
hasTrees <- subset(lt, !(is.na(SPECIES)))                   
ltsplist <- aggregate(list('ltsplist'=hasTrees$SPECIES)
                        ,list('UID'=hasTrees$UID,'METRIC'=hasTrees$METRIC) 
                        ,function(x) { paste(x, collapse=',')})
  ltsplist$METRIC <- 'ltsplist'
  ltsplist <- rename(ltsplist,'ltsplist','RESULT')


####  Woo hoo! Let's rbind these puppies and put 'em to bed
mhtrees <- rbind(ltmxdbh,ltmxht,ltmxspp,ltmxsize,ltmxdist,ltmxcnt,ltfracs,ltfracm,
                 ltfracl,ltfracx,ltmddist,ltsplist,ltmddom,ltmddomn,ltmdsub,
                 ltmdsubn)

}


