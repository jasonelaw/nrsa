summaryby<- function(xxx,yyy,zzz)
  {
   # This function uses aggregate to determine a statistic (mean, count, sd,
   # range, max, min) of RESULT for each UID in a data table.  Inputs are  the table
   # name (xxx), the desired statistic (yyy), and the new name for the statistic (zzz).
   # The output is a new dataset with variables UID, Metric and Result.
 
 www <- if (yyy == 'count' ) {
               aggregate( list('RESULT'=xxx$RESULT),list(UID=xxx$UID),get(yyy))
              
             } else if(yyy=='max' | yyy=='min' | yyy=='sum')  {
                aggregate( list('RESULT'=xxx$RESULT),list(UID=xxx$UID),function(x){ifelse(all(is.na(x)), NA, get(yyy)(x, na.rm=TRUE))})

             } else {
               aggregate( list('RESULT'=xxx$RESULT),list(UID=xxx$UID),yyy, na.rm=TRUE)
            }

 www$METRIC<- zzz
 www <- www[c('UID','METRIC','RESULT')]
 
 return(www)
 }

