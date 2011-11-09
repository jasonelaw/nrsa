# valLogicVisits.r
#
# Checks for consistency between tblVISITS2 and the data tables.
# Currently just checks VALXSITE to make sure siteProtocol() is accurate.
#
# 05/26/10 cws created
#
require(RODBC)

valLogicVisits <- function()
#
{
  chan <- odbcConnect('NRSA2')
  visits <- fetchNRSATable(chan, 'tblVISITS2')
  
  # VISITS::VALXSITE should be consistent with SAMPLE_TYPE in tables.  Since
  # only UID and SAMPLE_TYPE are needed, we'll use sqlFetch() instead of
  # fetchNRSATable() to save time and let the server do the summarization.
  sample_types <- NULL
  for(tName in c('tblBANKGEOMETRY2','tblCHANCOV2','tblCHANNELGEOMETRY2'
                ,'tblFISHCOVER2','tblTHALWEG2','tblWOOD2'
                )
     ) {
      intermediateMessage(sprintf(' .reading %s', tName))
      qq <- sprintf('select BATCHNO, SAMPLE_TYPE, count(*) AS N
                       from %s
                       group by BATCHNO, SAMPLE_TYPE
                    '
                   ,tName
                   )
      stCounts <- sqlQuery(chan, qq, stringsAsFactors=FALSE)
      if(!is.data.frame(stCounts)) {
          print(sprintf("Could not read table %s", tName))
          next;
      }
      stCounts$BATCHNO <- trimws(as.character(stCounts$BATCHNO))
      stCounts$SAMPLE_TYPE <- trimws(stCounts$SAMPLE_TYPE)
      stCounts$stProtocol <- ifelse(stCounts$SAMPLE_TYPE %in% c('PHAB_CHANBFRONT','PHAB_CHANB','PHAB_THAL','PHAB_CHANBFRONT')
                                   ,'BOATABLE'
                                   ,ifelse(stCounts$SAMPLE_TYPE %in% c('PHAB_CHANW','PHAB_THALW','PHAB_SLOPE')
                                          ,'WADEABLE'
                                          ,'XXXXXXXX'
                                          )
                                   )
      if(any(stCounts$stProtocol=='XXXXXXXX')) {
          cat(sprintf("unknown SAMPLE_TYPE %s", subset(stCounts, stProtocol=='XXXXXXXX')$SAMPLE_TYPE))
      }
      
      tt <- aggregate(list(stConsistent=stCounts$stProtocol)
                     ,list(BATCHNO=stCounts$BATCHNO, stProtocol=stCounts$stProtocol)
                     ,function(x) { length(unique(x))==1 }
                     )
     sample_types <- rbind(sample_types, tt)
  }
  sample_types <- rename(sample_types, 'BATCHNO', 'UID')

  # Report occurences of SAMPLE_TYPE protocols inconsistent within a table
  if(any(!sample_types$stConsistent)) {
      tt <- with(subset(sample_types, !stConsistent)
                ,aggregate(list(UID=UID)
                          ,list(stProtocol=stProtocol)
                          ,function(x) { paste(x, collapse=', ') }
                          )
                )
      print("WARNING: There are sites with SAMPLE_TYPE inconsistencies:")
      print(tt)
  }

  if(any(sample_types$stConsistent)) {
      # Determine protocol at each site based on summary of all tables.
      # Report occurences of SAMPLE_TYPE protocols inconsistent across tables.
      tt <- with(subset(sample_types, stConsistent)
                ,aggregate(list(tablesConsistent=stProtocol)
                          ,list(UID=UID)
                          ,function(x) { count(unique(x)) == 1 }
                          )
                )
      actual <- merge(unique(subset(sample_types, stConsistent))
                     ,tt
                     ,by='UID'
                     )

      if(any(!actual$tablesConsistent)) {
          tt <- with(subset(actual, !tablesConsistent)
                    ,aggregate(list(UID=UID)
                              ,list(stProtocol=stProtocol)
                              ,function(x) { paste(x, collapse=', ') }
                              )
                    )
          print("Warning: There are sites with protocol inconsistencies across tables:")
          print(tt)
      }
      
      if(any(actual$tablesConsistent)) {
          # Compare reported sampling protocol with the protocol used based on the
          # SAMPLE_TYPE values, where these are consistent.
          reported <- siteProtocol(unique(actual$UID))
          tt <- merge(reported, actual[c('UID','stProtocol')], by='UID')
          
          if(any(tt$PROTOCOL != tt$stProtocol))
              print("WARNING: There are sites for which reported sampling protocol does not agree with SAMPLE_TYPE in tables:")
              print(subset(tt, tt$PROTOCOL != tt$stProtocol))
      }

  }

  intermediateMessage(' .Done.')
}
# end of file