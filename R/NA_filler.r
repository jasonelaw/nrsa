   NA_filler <-function(alldat,metdat,metlist)
     #finds UIDs that are not in the metdat  file and creates
     # observations with the metric name for PARAMETER
     # and NA for the RESULT variable.
     #
     # alldat -- input data set with either all possible observations,
     # all boatable sites, or all wadeable sites.  If the metric lists
     # for the boatable and wadeable sites are the same, then a list
     # of all possible observations can be used. If the metric lists
     # differ between boatable and wadeable sites, then alldat can
     # contain only boatable or wadeable obervations.

     # metdat -- refined dataset used to calculate the metrics.  Illegal
     # RESULT values have been eliminated, and unneeded PARAMETER
     # lines have been removed.  metdat may have both boatable and
     # wadeable  observations.

     # metlist--  a list of the metrics.  Use list('metric1','metric2',...).
     # This list should be matched up
     # with alldat, that is, either all possible metrics, or only boatable
     # metrics, or only wadeable metrics, depending on the configuration
     # of alldat. 


     {
      #get all appropriate  UIDs from unrefined dataset.
       uidlist<-unique(alldat$UID)
    
     #get UIDs that are not found in the refined dataset 
       missuids<- subset(uidlist,!(uidlist %in% metdat$UID))

     #create output file to be appended to the final dataset
       outdat<-rbind(expand.grid(UID=missuids,
                                                  PARAMETER=metlist ,
                                                  RESULT=NA
                                                )
                            )

        return (outdat)
     }
 
