## metsInvasiveSpecies.r
##
## Created SSR 01/29/2010
## 03/23/10 cws moved creation of test dataframes into separate functions
#   3/25/10 cws Changed diff() calls to dfCompare().
#   6/24/10 cws Modified to handle odd case when transect has NO_INVASIVES
#           marked as well as an invasive species marked.  This shouldn't,
#           in the hopeful sense, make it through QA but we can assume a
#           species marked as present actually is present and safely ignore
#           the NO_INVASIVES check in these cases.

metsInvasiveSpecies <- function()
## Creating mhinplnt.csv which contains invasive species metrics:
## f_myrspi = count of E_WTRMILF/count of transects
## f_hydver = count of HYDRILLA/count of transects
## f_eiccra = count of W_HYACINTH/count of transects
## f_nympel = count of YLW_FLTHEAR/count of transects
## f_lytsal = count of P_LSTRIFE/count of transects
## f_arudon = count of G_REED/count of transects
## f_butumb = count of FLWR_RUSH/count of transects
## f_tamspp = count of SALT_CED/count of transects
## f_rosmul = count of MF_ROSE/count of transects
## f_eupesu = count of SPURGE/count of transects
## f_none = count of NO_INVASIVES/count of transects
## ip_count = sum(f_myrspi, f_hydver, f_eiccra, f_nympel, f_lytsal, f_arudon,
##                f_butumb, f_tamspp, f_rosmul, f_eupesu)
{
                 
intermediateMessage('Invasive species metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')
on.exit(odbcClose(chan))
##  Reading in data from tblINVASIVELEGACY2
chan <- odbcConnect('NRSA2')
tblINVASIVELEGACY2 <- fetchNRSATable(chan,'tblINVASIVELEGACY2')

## Retataining invasive legacy data
df <- subset(tblINVASIVELEGACY2, PARAMETER %in% c('E_WTRMILF','HYDRILLA','W_HYACINTH',
             'YLW_FLTHEAR','P_LSTRIFE','G_REED','FLWR_RUSH','SALT_CED','MF_ROSE',
             'SPURGE','NO_INVASIVES'), select=c('UID','TRANSECT','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsInvasiveSpecies.1', loc='end')
mets <- metsInvasiveSpecies.1(df)

## Write the results to 
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsInvasiveSpecies.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsInvasiveSpecies.1 <- function(df)
{
df$RESULT <- ifelse(df$RESULT=='X' | df$RESULT == 'Y', 1, df$RESULT)              
## Creating counts of individual invasive species
aa <- aggregate(list(RESULT=df$RESULT), by=list(UID=df$UID, METRIC=df$PARAMETER), count)

##Creating count of transects observed
bb <- subset(df, select=c('UID', 'TRANSECT'))
cc <- bb[!duplicated(bb),]
dd <- aggregate(list(tranCount=cc$TRANSECT), list(UID=cc$UID), count)
ee <- merge(aa,dd)

##  Creating metrics for individual species
## Because individual counts are not kept with the calculated metrics,
## we are changing the species name to the species metric name
ee$METRIC <- ifelse(ee$METRIC=='E_WTRMILF', 'f_myrspi', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='HYDRILLA', 'f_hydver', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='W_HYACINTH', 'f_eiccra', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='YLW_FLTHEAR', 'f_nympel', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='P_LSTRIFE', 'f_lytsal', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='G_REED', 'f_arudon', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='FLWR_RUSH', 'f_butumb', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='SALT_CED', 'f_tamspp', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='MF_ROSE', 'f_rosmul', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='SPURGE', 'f_eupesu', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='NO_INVASIVES', 'f_none', ee$METRIC)
## All invasive species metrics are number of observations within a site
## divided by the number of transects observed during site visit
ee$RESULT <- ee$RESULT/ee$tranCount
## Done with count of transects, deleting it
ee$tranCount <- NULL

## Calculating ip_score, which is the sum of all metrics of invasive species
## present.
ff <- aggregate(list(RESULT=ee[ee$METRIC != 'f_none',]$RESULT),
                     list(UID=ee[ee$METRIC != 'f_none',]$UID), sum)
ff$METRIC <- 'ip_score'
ff <- ff[,c('UID','METRIC','RESULT')]

## If there are no invasive species present, it will not have been calculated
## in the previous step, none must equal 1, and therefore ip_score must equal 0.
## This does not take into account transects in which NO_INVASIVES were checked
## along with an invasive.  This *should* be caught during QA, but if it isn't
## we'll assume the invasive actually exists and ignore the NO_INVASIVES flag.
gg <- subset(ee, METRIC=='f_none' & RESULT == 1)
gg$METRIC <- 'ip_score'
gg$RESULT <- 0
gg <- subset(gg, !(UID %in% ff$UID))


##  Creating final file -- indivual species metrics, ip_score when invasive
## species are present, and ip_score when no invasive species are present.
mhinplnt <- rbind(ee,ff,gg)
}

