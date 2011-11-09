## metsHumanInfluence.r
##
## Created SSR 02/10/2010
## 02/19/2010 removed source() call
## 03/23/1010 cws moved creation of test dataframes to separate functions.
#  3/25/10 cws Changed diff() calls to dfCompare().
## 03/25/10 ssr changed metrics names to lowercase.

metsHumanInfluence <- function()
## Creating human influence metrics:
## Creating human influence metrics:
## sdb_hall	 Std dev human disturbance on bank
## sdcb_hall	 Std dev human dist. on bank or channel
## sdc_hall	 Std dev human disturbance in channel
## sdwcb_hall	 Std dev wted human dist on bank or chan
## w1h_bldg	 Rip Dist--Buildings (ProxWt Pres)
## w1h_crop	 Rip Dist--Row Crop (ProxWt Pres)
## w1h_ldfl	 Rip Dist--Trash/Landfill (ProxWt Pres)
## w1h_log	 Rip Dist--Logging Activity (ProxWt Pres)
## w1h_mine	 Rip Dist--Mining Activity (ProxWt Pres)
## w1h_park	 Rip Dist--Lawn/Park (ProxWt Pres)
## w1h_pipe	 Rip Dist--Pipes infl/effl (ProxWt Pres)
## w1h_pstr	 Rip Dist--Pasture/Hayfield (ProxWt Pres)
## w1h_pvmt	 Rip Dist--Pavement (ProxWt Pres)
## w1h_road	 Rip Dist--Road/Railroad (ProxWt Pres)
## w1h_wall	 Rip Dist--Wall/Bank Revet. (ProxWt Pres)
## w1_hag	 Rip Dist--Sum Agric Types (ProxWt Pres)
## w1_hall	 Rip Dist--Sum All Types (ProxWt Pres)
## w1_hnoag	 Rip Dist--Sum NonAg Types (ProxWt Pres)
## xb_hag	 Rip Dist-Sum Ag Types instrm & in plot
## xb_hall	 Rip Dist--Sum All Types instrm & on bank
## xb_hnoag	 Rip Dist Sum-Non ag Types instrm & Plot
## xcb_hag	 Rip Dist Sum-Ag Types instrm & on Bank
## xcb_hall	 Rip Dist--Sum All Types instrm & in plot
## xcb_hnag	 Rip Dist Sum-Non Ag Types instrm & Bank
## xc_hag	 Rip Dist-Sum of Ag Types in Ripar Plot
## xc_hall	 Rip Dist--Sum All Types in Ripar Plots
## xc_hnoag	 Rip Dist Sum-Non Ag Types in Ripar Plot
## xf_hag	 Rip Dist Sum-Ag Types Beyond Ripar Plot
## xf_hall	 Rip Dist--Sum All Types beyond Rip Plots
## xf_hnoag	 Rip Dist Sum-Non Ag Types Beyond Rip Plt
## x_hag	 Rip Dist Sum-Ag Types rip Plt & Beyond
## x_hall	 Rip Dist--Sum All Types str plt & beyond
## x_hnoag	 Rip Dist Sum-Non Ag rip Plt & Beyond

{
intermediateMessage('Human influence metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')
on.exit(odbcClose(chan))
##  Reading in data from INVASIVES2
chan <- odbcConnect('NRSA2')
tblVR <- fetchNRSATable(chan,'tblVISRIP2')
df <- subset (tblVR, PARAMETER %in% c('BUILD', 'LANDFL', 'LOG', 'MINE', 'PARK',
              'PAST', 'PAVE', 'PIPES', 'ROAD', 'ROW', 'WALL'),
              select=c('UID','TRANSECT','TRANSDIR','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsHumanInfluence.1', loc='end')
mets <- metsHumanInfluence.1(df)

## Write the results to metsHumanInfluence.csv
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsHumanInfluence.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsHumanInfluence.1 <- function(df)
{

df2 <- df
df2$var_0 <- ifelse(df$RESULT=='0', 1, 0)
df2$var_P <- ifelse(df$RESULT=='P', 1, 0)
df2$var_C <- ifelse(df$RESULT=='C', 1, 0)
df2$var_B <- ifelse(df$RESULT=='B', 1, 0)
df2$var_CB <- df2$var_C+ df2$var_B
           
##  Calculating meanAtTransect
meanb_hall <- aggregate(list(xb_hall=df2$var_B), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T)
meanc_hall <- aggregate(list(xc_hall=df2$var_C), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T) 
meancb_hall <- aggregate(list(xcb_hall=df2$var_CB), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T)
meanf_hall <- aggregate(list(xf_hall=df2$var_P), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect)
xb_hall <- aggregate(list(xb_hall=meanb_hall$xb_hall)
                   , list(UID=meanb_hall$UID), sum, na.rm=T)
xc_hall <- aggregate(list(xc_hall=meanc_hall$xc_hall)
                   , list(UID=meanc_hall$UID), sum, na.rm=T) 
xcb_hall <- aggregate(list(xcb_hall=meancb_hall$xcb_hall)
                   , list(UID=meancb_hall$UID), sum, na.rm=T)
xf_hall <- aggregate(list(xf_hall=meanf_hall$xf_hall)
                   , list(UID=meanf_hall$UID), sum, na.rm=T) 

##  Creating ag (agriculture parameters) and noag (non-agriculture parameters)
ag <- subset(df2, (PARAMETER %in% c('ROW', 'PAST')))
noag <- subset(df2, !(PARAMETER %in% c('ROW', 'PAST')))

##  Calculating meanAtTransect for agriculture parameters
meanb_hag <- aggregate(list(xb_hag=ag$var_B), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T)
meanc_hag <- aggregate(list(xc_hag=ag$var_C), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T) 
meancb_hag <- aggregate(list(xcb_hag=ag$var_CB), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T)
meanf_hag <- aggregate(list(xf_hag=ag$var_P), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect) for agriculture parameters
xb_hag <- aggregate(list(xb_hag=meanb_hag$xb_hag), list(UID=meanb_hag$UID)
                   , sum, na.rm=T)
xc_hag <- aggregate(list(xc_hag=meanc_hag$xc_hag), list(UID=meanc_hag$UID)
                   , sum, na.rm=T) 
xcb_hag <- aggregate(list(xcb_hag=meancb_hag$xcb_hag), list(UID=meancb_hag$UID)
                   , sum, na.rm=T)
xf_hag <- aggregate(list(xf_hag=meanf_hag$xf_hag), list(UID=meanf_hag$UID)
                   , sum, na.rm=T) 

##  Calculating meanAtTransect for non-agriculture parameters
meanb_hnoag <- aggregate(list(xb_hnoag=noag$var_B), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T)
meanc_hnoag <- aggregate(list(xc_hnoag=noag$var_C), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T) 
meancb_hnoag <- aggregate(list(xcb_hnag=noag$var_CB), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T)
meanf_hnoag <- aggregate(list(xf_hnoag=noag$var_P), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect) for non-agriculture parameters
xb_hnoag <- aggregate(list(xb_hnoag=meanb_hnoag$xb_hnoag)
                    , list(UID=meanb_hnoag$UID), sum, na.rm=T)
xc_hnoag <- aggregate(list(xc_hnoag=meanc_hnoag$xc_hnoag)
                    , list(UID=meanc_hnoag$UID), sum, na.rm=T) 
xcb_hnoag <- aggregate(list(xcb_hnag=meancb_hnoag$xcb_hnag)
                     , list(UID=meancb_hnoag$UID), sum, na.rm=T)
xf_hnoag <- aggregate(list(xf_hnoag=meanf_hnoag$xf_hnoag)
                    , list(UID=meanf_hnoag$UID), sum, na.rm=T) 

df3 <- merge(xb_hall, xc_hall, all=T)
df3 <- merge(df3, xcb_hall, all=T)
df3 <- merge(df3, xf_hall, all=T)
df3 <- merge(df3, xb_hag, all=T)
df3 <- merge(df3, xc_hag, all=T)
df3 <- merge(df3, xcb_hag, all=T)
df3 <- merge(df3, xf_hag, all=T)
df3 <- merge(df3, xb_hnoag, all=T)
df3 <- merge(df3, xc_hnoag, all=T)
df3 <- merge(df3, xcb_hnoag, all=T)
df3 <- merge(df3, xf_hnoag, all=T)

df3$METRIC <- ''

df3$x_hall <- df3$xb_hall + df3$xc_hall + df3$xf_hall
df3$x_hag <-  df3$xb_hag + df3$xc_hag + df3$xf_hag
df3$x_hnoag <- df3$xb_hnoag + df3$xc_hnoag + df3$xf_hnoag

## Creating dataframes for export to csv file
x_hall <- subset(df3, select=c(UID,METRIC,x_hall))
 x_hall$METRIC <- 'x_hall'
 x_hall <- rename(x_hall, 'x_hall', 'RESULT')
x_hag <- subset(df3, select=c(UID,METRIC,x_hag))
 x_hag$METRIC <- 'x_hag'
 x_hag <- rename(x_hag, 'x_hag', 'RESULT')
x_hnoag <- subset(df3, select=c(UID,METRIC,x_hnoag))
 x_hnoag$METRIC <- 'x_hnoag'
 x_hnoag <- rename(x_hnoag, 'x_hnoag', 'RESULT')
 
 xb_hall <- rename(xb_hall, 'xb_hall', 'RESULT')
  xb_hall$METRIC <- 'xb_hall' 
 xc_hall <- rename(xc_hall, 'xc_hall', 'RESULT')
  xc_hall$METRIC <- 'xc_hall' 
 xcb_hall <- rename(xcb_hall, 'xcb_hall', 'RESULT')
  xcb_hall$METRIC <- 'xcb_hall' 
 xf_hall <- rename(xf_hall, 'xf_hall', 'RESULT')
  xf_hall$METRIC <- 'xf_hall' 
 xb_hag <- rename(xb_hag, 'xb_hag', 'RESULT')
  xb_hag$METRIC <- 'xb_hag' 
 xc_hag <- rename(xc_hag, 'xc_hag', 'RESULT')
  xc_hag$METRIC <- 'xc_hag' 
 xcb_hag <- rename(xcb_hag, 'xcb_hag', 'RESULT')
  xcb_hag$METRIC <- 'xcb_hag' 
 xf_hag <- rename(xf_hag, 'xf_hag', 'RESULT')
  xf_hag$METRIC <- 'xf_hag' 
 xb_hnoag <- rename(xb_hnoag, 'xb_hnoag', 'RESULT')
  xb_hnoag$METRIC <- 'xb_hnoag' 
 xc_hnoag <- rename(xc_hnoag, 'xc_hnoag', 'RESULT')
  xc_hnoag$METRIC <- 'xc_hnoag' 
 xcb_hnoag <- rename(xcb_hnoag, 'xcb_hnag', 'RESULT')
  xcb_hnoag$METRIC <- 'xcb_hnag' 
 xf_hnoag <- rename(xf_hnoag, 'xf_hnoag', 'RESULT')
  xf_hnoag$METRIC <- 'xf_hnoag' 

 ## Standard deviations of sumAtTransect
sumb_hall <- aggregate(list(sum_b=df2$var_B), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
sumc_hall <- aggregate(list(sum_c=df2$var_C), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
sumcb_hall <- aggregate(list(sum_cb=df2$var_CB), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
df2$wcb = (1.5 * df2$var_B) + df2$var_C
sumwcb_hall <- aggregate(list(sum_wcb=df2$wcb), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)


sdb_hall <- aggregate(list(RESULT=sumb_hall$sum_b), list(UID=sumb_hall$UID), sd)
 sdb_hall$METRIC <- 'sdb_hall'
sdc_hall <- aggregate(list(RESULT=sumc_hall$sum_c), list(UID=sumc_hall$UID), sd)
 sdc_hall$METRIC <- 'sdc_hall'
sdcb_hall <- aggregate(list(RESULT=sumcb_hall$sum_cb), list(UID=sumcb_hall$UID), sd)
 sdcb_hall$METRIC <- 'sdcb_hall'
sdwcb_hall <- aggregate(list(RESULT=sumwcb_hall$sum_wcb), list(UID=sumwcb_hall$UID), sd)
 sdwcb_hall$METRIC <- 'sdwcb_hall'

## Weighted sums 
df3$w1_hall <- (1.5 * df3$xb_hall) + df3$xc_hall + (0.6667 * df3$xf_hall)
df3$w1_hnoag <- (1.5 * df3$xb_hnoag) + df3$xc_hnoag + (0.6667 * df3$xf_hnoag)
df3$w1_hag <- (1.5 * df3$xb_hag) + df3$xc_hag + (0.6667 * df3$xf_hag)

w1_hall <- subset(df3, select=c('UID', 'METRIC', 'w1_hall'))
w1_hnoag <- subset(df3, select=c('UID', 'METRIC', 'w1_hnoag'))
w1_hag <- subset(df3, select=c('UID', 'METRIC', 'w1_hag'))
                                  
# Create table for weighing influence proximity values
weights0PCB <- data.frame(proximity=c('0', 'P', 'C', 'B')
                           ,calc=     c(0.0, 0.6667, 1.0, 1.5)
                           , stringsAsFactors=F
                          )

# Convert proximity classes to numeric values and characterize influence types
df5 <- merge(df, weights0PCB
                ,by.x='RESULT'
                ,by.y='proximity'
                ,all.x=TRUE
                ,sort=FALSE
                )

##  Creating W1H_ variables
w1h_sums <- aggregate(list(RESULT=df5$calc),
               list(UID=df5$UID, METRIC=df5$PARAMETER),mean, na.rm=T)

w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='BUILD', 'w1h_bldg', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LANDFL', 'w1h_ldfl', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LOG', 'w1h_log', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='MINE', 'w1h_mine', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PARK', 'w1h_park', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAST', 'w1h_pstr', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAVE', 'w1h_pvmt', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PIPES', 'w1h_pipe', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROAD', 'w1h_road', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROW', 'w1h_crop', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='WALL', 'w1h_wall', w1h_sums$METRIC)


w1_ag <- subset(w1h_sums,(METRIC %in% c('w1h_crop', 'w1h_pstr')))
w1_hag <- aggregate(list(RESULT=w1_ag$RESULT), list(UID=w1_ag$UID), sum, na.rm=T)
w1_hag$METRIC <- 'w1_hag'

w1_noag <- subset(w1h_sums, !(METRIC %in% c('w1h_crop', 'w1h_pstr')))
w1_hnoag <- aggregate(list(RESULT=w1_noag$RESULT), list(UID=w1_noag$UID), sum, na.rm=T)
w1_hnoag$METRIC <- 'w1_hnoag'

w1_hall <- aggregate(list(RESULT=w1h_sums$RESULT),list(UID=w1h_sums$UID),sum, na.rm=T)
w1_hall$METRIC <- 'w1_hall'




aa <- rbind(xb_hag,xc_hag,xcb_hag,xf_hag,xb_hnoag,xcb_hnoag,xc_hnoag,xf_hnoag
           ,xb_hall,xcb_hall,xc_hall,xf_hall,sdb_hall,sdcb_hall,sdc_hall
           ,sdwcb_hall,w1_hag,w1_hnoag,w1_hall)
bb <- aa[,c('UID', 'METRIC', 'RESULT')] 
hiMets <- rbind(bb,w1h_sums,x_hag,x_hnoag,x_hall)
 
}

