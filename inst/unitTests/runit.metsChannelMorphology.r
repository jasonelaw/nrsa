metsChannelMorphologyTest <- function ()
# test of metsChannelMorphology function
# ARGUMENTS:
#  none
{
  intermediateMessage ('Channel Morphology calculations', loc='start')

  # Create test data and expected calculation results
  intermediateMessage (' both protocols')
  df1 <- metsChannelMorphology.createBankGeometryData()
  df2 <- metsChannelMorphology.createThalwegData()
  protocols <- metsChannelMorphology.createProtocolData()
  expected <- metsChannelMorphology.createExpectedResults()

  metsChannelMorphologyTest.process(df1, df2, protocols, expected)

  intermediateMessage (' wadeable protocol')
  df1.w <- subset(df1, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  df2.w <- subset(df2, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  prot.w <- subset(protocols, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  exp.w <- subset(expected, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  metsChannelMorphologyTest.process(df1.w, df2.w, prot.w, exp.w)

  intermediateMessage (' boatable protocol')
  df1.b <- subset(df1, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  df2.b <- subset(df2, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  prot.b <- subset(protocols, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  exp.b <- subset(expected, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  metsChannelMorphologyTest.process(df1.b, df2.b, prot.b, exp.b)
}


metsChannelMorphologyTest.process <- function(df1, df2, protocols, expected)
# The bulk of the unit testing is done here
# df1<-df1.b; df2<-df2.b; protocols<-prot.b; expected<-exp.b
{
  #calculate the metrics
  rr <- metsChannelMorphology.1 (df1, df2,  protocols)
  if(is.character(rr)) return (rr)
   

  tt <- merge(expected, rr, by=c('UID','METRIC')
             ,suffixes=c('.expected','.actual')
             )
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  errs <- subset(tt, abs(diff) > 10^-5 | is.na(RESULT.expected) != is.na(RESULT.actual))
  intermediateMessage ( ' Done.', loc='end')
  checkEquals(0, nrow(errs)
            ,"Error: bankMorphology metrics are broken; "
            )
  
}

metsChannelMorphology.cleanup <- function(chan){
# Clean up when function terminates
  odbcClose(chan)
}

metsChannelMorphology.createBankGeometryData <- function(){
  #stream : angle, undercut
  #rivers : angle, wetted width
  #read in the test data from
  testData('ChannelMorphology.BankGeometry.Rdata')
}


metsChannelMorphology.createThalwegData <- function(){
  testData('ChannelMorphology.Thalweg.Rdata')
}


metsChannelMorphology.createProtocolData <- function(){
  testData('ChannelMorphology.Protocol.Rdata')
}

metsChannelMorphology.createExpectedResults <- function(){
  testData('ChannelMorphology.ExpectedResults.Rdata')
}


# end of file
