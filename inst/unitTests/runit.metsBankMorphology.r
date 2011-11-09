metsBankMorphologyTest <- function ()

# test of metsBankMorphology function
# ARGUMENTS:
#  none
{
  intermediateMessage ('Bank Morphology calculations', loc='start')

  # Create test data and expected calculation results
  intermediateMessage (' both protocols')
  testData <- metsBankMorphology.inputData()
  expected <- metsBankMorphology.testResults()

  #create protocols dataset
  bob3 <- textConnection("UID  PROTOCOL
           1  BOATABLE
           2  BOATABLE
           3  BOATABLE
           4  BOATABLE
           5  WADEABLE
           6  WADEABLE
           7  WADEABLE
           8  WADEABLE
           9  WADEABLE
           10 WADEABLE"
           )
  protocols <- read.table(bob3, header=TRUE,stringsAsFactors=FALSE)
  close (bob3)

  #calculate the metrics
    

  metsBankMorphologyTest.process(testData, protocols, expected)

  intermediateMessage (' wadeable protocol')
  df1.w <- subset(testData, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  prot.w <- subset(protocols, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  exp.w <- subset(expected, UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID)
  metsBankMorphologyTest.process(df1.w, prot.w, exp.w)


  intermediateMessage (' boatable protocol')
  df1.b <- subset(testData, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  prot.b <- subset(protocols, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  exp.b <- subset(expected, UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID)
  metsBankMorphologyTest.process(df1.b, prot.b, exp.b)

}


metsBankMorphologyTest.process <- function(df1, protocols, expected)
# The bulk of the unit testing is done here
# df1<-df1.b; df2<-df2.b; protocols<-prot.b; expected<-exp.b
{
  #calculate the metrics
  rr <- metsBankMorphology.1 (df1,  protocols)
  if(is.character(rr)) return (rr)


  tt <- merge(expected, rr, by=c('UID','METRIC')
             ,suffixes=c('.expected','.actual')
             )
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  #print(tt)
  errs <- subset(tt, abs(diff) > 10^-5 | is.na(RESULT.expected) != is.na(RESULT.actual))
  intermediateMessage ( ' Done.', loc='end')
  checkEquals(0, nrow(errs)
            ,"Error: bankMorphology metrics are broken; "
            )

}

  
metsBankMorphology.inputData <- function()
# creates dataframe of bank morphology data for unit test
{
     bob <- textConnection("UID  TRANSECT  PARAMETER  RESULT
                              5  A  ANGLE  4
                              5  A  ANGLE  18
                              5  B  ANGLE  16
                              5  B  ANGLE  16
                              5  C  ANGLE  21
                              5  C  ANGLE  25
                              5  D  ANGLE  5
                              5  D  ANGLE  15
                              5  E  ANGLE  10
                              5  E  ANGLE  5
                              5  F  ANGLE  10
                              5  F  ANGLE  9
                              5  G  ANGLE  11
                              5  G  ANGLE  5
                              5  H  ANGLE  82
                              5  H  ANGLE  3
                              5  I  ANGLE  18
                              5  I  ANGLE  24
                              5  J  ANGLE  7
                              5  J  ANGLE  20
                              5  K  ANGLE  1
                              5  K  ANGLE  44
                              6  A  ANGLE  22
                              6  A  ANGLE  25
                              6  B  ANGLE  15
                              6  B  ANGLE  24
                              6  C  ANGLE  8
                              6  C  ANGLE  90
                              6  D  ANGLE  110
                              6  D  ANGLE  151
                              6  E  ANGLE  5
                              6  E  ANGLE  45
                              6  F  ANGLE  15
                              6  F  ANGLE  10
                              6  G  ANGLE  20
                              6  G  ANGLE  5
                              6  H  ANGLE  95
                              6  H  ANGLE  9
                              6  I  ANGLE  65
                              6  I  ANGLE  70
                              6  J  ANGLE  105
                              6  J  ANGLE  16
                              6  K  ANGLE  5
                              6  K  ANGLE  60
                              8  A  ANGLE  24
                              8  A  ANGLE  8
                              8  B  ANGLE  13
                              8  B  ANGLE  12
                              8  C  ANGLE  23
                              8  D  ANGLE  10
                              8  E  ANGLE  74
                              8  E  ANGLE  22
                              8  F  ANGLE  42
                              8  F  ANGLE  24
                              8  G  ANGLE  18
                              8  G  ANGLE  12
                              8  H  ANGLE  25
                              8  I  ANGLE  52
                              8  I  ANGLE  25
                              8  J  ANGLE  15
                              8  J  ANGLE  40
                              8  K  ANGLE  25
                              8  K  ANGLE  90
                              9  A  ANGLE  120
                              9  A  ANGLE  136
                              9  B  ANGLE  100
                              9  B  ANGLE  140
                              9  C  ANGLE  64
                              9  C  ANGLE  50
                              9  D  ANGLE  103
                              9  D  ANGLE  61
                              9  E  ANGLE  148
                              9  E  ANGLE  110
                              9  F  ANGLE  118
                              9  F  ANGLE  131
                              9  G  ANGLE  127
                              9  G  ANGLE  129
                              9  H  ANGLE  102
                              9  H  ANGLE  95
                              9  I  ANGLE  132
                              9  I  ANGLE  28
                              9  J  ANGLE  138
                              9  J  ANGLE  119
                              9  K  ANGLE  108
                              9  K  ANGLE  114
                              10  A  ANGLE  32
                              10  A  ANGLE  30
                              10  B  ANGLE  44
                              10  B  ANGLE  48
                              10  C  ANGLE  50
                              10  C  ANGLE  48
                              10  D  ANGLE  62
                              10  D  ANGLE  38
                              10  E  ANGLE  46
                              10  E  ANGLE  44
                              10  F  ANGLE  50
                              10  F  ANGLE  28
                              10  G  ANGLE  102
                              10  G  ANGLE  18
                              10  H  ANGLE  20
                              10  H  ANGLE  82
                              10  I  ANGLE  10
                              10  I  ANGLE  42
                              10  J  ANGLE  121
                              10  J  ANGLE  52
                              10  K  ANGLE  43
                              10  K  ANGLE  117
                              5  A  UNDERCUT  0
                              5  A  UNDERCUT  0
                              5  B  UNDERCUT  0
                              5  B  UNDERCUT  0
                              5  C  UNDERCUT  0
                              5  C  UNDERCUT  0
                              5  D  UNDERCUT  0
                              5  D  UNDERCUT  0.2
                              5  E  UNDERCUT  0.25
                              5  E  UNDERCUT  0
                              5  F  UNDERCUT  0
                              5  F  UNDERCUT  0
                              5  G  UNDERCUT  0
                              5  G  UNDERCUT  0
                              5  H  UNDERCUT  0
                              5  H  UNDERCUT  0
                              5  I  UNDERCUT  0
                              5  I  UNDERCUT  0
                              5  J  UNDERCUT  0
                              5  J  UNDERCUT  0.18
                              5  K  UNDERCUT  0
                              5  K  UNDERCUT  0
                              6  A  UNDERCUT  0
                              6  A  UNDERCUT  0
                              6  B  UNDERCUT  0
                              6  B  UNDERCUT  0
                              6  C  UNDERCUT  0
                              6  C  UNDERCUT  0.15
                              6  D  UNDERCUT  0.15
                              6  D  UNDERCUT  0
                              6  E  UNDERCUT  0
                              6  E  UNDERCUT  0
                              6  F  UNDERCUT  0
                              6  F  UNDERCUT  0
                              6  G  UNDERCUT  0
                              6  G  UNDERCUT  0
                              6  H  UNDERCUT  0.05
                              6  H  UNDERCUT  0
                              6  I  UNDERCUT  0
                              6  I  UNDERCUT  0
                              6  J  UNDERCUT  0
                              6  J  UNDERCUT  0
                              6  K  UNDERCUT  0
                              6  K  UNDERCUT  0
                              8  A  UNDERCUT  0
                              8  A  UNDERCUT  0
                              8  B  UNDERCUT  0
                              8  B  UNDERCUT  0
                              8  C  UNDERCUT  0
                              8  D  UNDERCUT  0
                              8  E  UNDERCUT  0
                              8  E  UNDERCUT  0
                              8  F  UNDERCUT  0
                              8  F  UNDERCUT  0
                              8  G  UNDERCUT  0
                              8  G  UNDERCUT  0
                              8  H  UNDERCUT  0
                              8  I  UNDERCUT  0
                              8  I  UNDERCUT  0
                              8  J  UNDERCUT  0
                              8  J  UNDERCUT  0
                              8  K  UNDERCUT  0
                              8  K  UNDERCUT  0
                              9  A  UNDERCUT  0.2
                              9  A  UNDERCUT  0.3
                              9  B  UNDERCUT  0.1
                              9  B  UNDERCUT  0.2
                              9  D  UNDERCUT  0.3
                              9  E  UNDERCUT  0.7
                              9  E  UNDERCUT  0.2
                              9  F  UNDERCUT  0.3
                              9  F  UNDERCUT  0.4
                              9  G  UNDERCUT  0.2
                              9  G  UNDERCUT  0.4
                              9  H  UNDERCUT  0.3
                              9  H  UNDERCUT  0.2
                              9  I  UNDERCUT  0.4
                              9  J  UNDERCUT  0.3
                              9  J  UNDERCUT  0.2
                              9  K  UNDERCUT  0.4
                              9  K  UNDERCUT  0.2
                              10  A  UNDERCUT  0
                              10  A  UNDERCUT  0
                              10  B  UNDERCUT  0
                              10  B  UNDERCUT  0
                              10  C  UNDERCUT  0
                              10  C  UNDERCUT  0
                              10  D  UNDERCUT  0
                              10  D  UNDERCUT  0
                              10  E  UNDERCUT  0
                              10  E  UNDERCUT  0
                              10  F  UNDERCUT  0
                              10  F  UNDERCUT  0
                              10  G  UNDERCUT  0.3
                              10  G  UNDERCUT  0
                              10  H  UNDERCUT  0
                              10  H  UNDERCUT  0
                              10  I  UNDERCUT  0
                              10  I  UNDERCUT  0
                              10  J  UNDERCUT  0.4
                              10  J  UNDERCUT  0
                              10  K  UNDERCUT  0
                              10  K  UNDERCUT  0.2
                              1  A  ANGLE  30-75
                              1  B  ANGLE  5-30
                              1  C  ANGLE  5-30
                              1  D  ANGLE  5-30
                              1  E  ANGLE  5-30
                              1  F  ANGLE  5-30
                              1  G  ANGLE  5-30
                              1  H  ANGLE  5-30
                              1  I  ANGLE  5-30
                              1  J  ANGLE  5-30
                              1  K  ANGLE  30-75
                              2  A  ANGLE  5-30
                              2  B  ANGLE  5-30
                              2  C  ANGLE  5-30
                              2  D  ANGLE  5-30
                              2  E  ANGLE  5-30
                              2  F  ANGLE  5-30
                              2  G  ANGLE  5-30
                              2  H  ANGLE  5-30
                              2  I  ANGLE  30-75
                              2  J  ANGLE  30-75
                              2  K  ANGLE  0-5
                              3  A  ANGLE  5-30
                              3  B  ANGLE  5-30
                              3  C  ANGLE  5-30
                              3  D  ANGLE  5-30
                              3  E  ANGLE  5-30
                              3  F  ANGLE  5-30
                              3  G  ANGLE  5-30
                              3  H  ANGLE  5-30
                              3  I  ANGLE  5-30
                              3  J  ANGLE  5-30
                              3  K  ANGLE  5-30
                              4  A  ANGLE  30-75
                              4  B  ANGLE  5-30
                              4  C  ANGLE  5-30
                              4  D  ANGLE  30-75
                              4  E  ANGLE  30-75
                              4  F  ANGLE  30-75
                              4  G  ANGLE  5-30
                              4  H  ANGLE  5-30
                              4  I  ANGLE  5-30
                              4  J  ANGLE  5-30
                              4  K  ANGLE  0-5
                              1  A  WETWID  55
                              1  B  WETWID  48
                              1  C  WETWID  63
                              1  D  WETWID  48
                              1  E  WETWID  80
                              1  F  WETWID  77
                              1  G  WETWID  66
                              1  H  WETWID  95
                              1  I  WETWID  131
                              1  J  WETWID  37
                              1  K  WETWID  32
                              2  A  WETWID  126
                              2  B  WETWID  48
                              2  C  WETWID  49
                              2  D  WETWID  28
                              2  E  WETWID  58
                              2  F  WETWID  38
                              2  G  WETWID  65
                              2  H  WETWID  42
                              2  I  WETWID  36
                              2  J  WETWID  107
                              2  K  WETWID  69
                              3  A  WETWID  121
                              3  B  WETWID  52
                              3  C  WETWID  45
                              3  D  WETWID  76
                              3  E  WETWID  56
                              3  F  WETWID  36
                              3  G  WETWID  25
                              3  H  WETWID  60
                              3  I  WETWID  45
                              3  J  WETWID  49
                              3  K  WETWID  41
                              4  A  WETWID  26
                              4  B  WETWID  23
                              4  C  WETWID  32
                              4  D  WETWID  28
                              4  E  WETWID  26
                              4  F  WETWID  30
                              4  G  WETWID  33
                              4  H  WETWID  33
                              4  I  WETWID  28
                              4  J  WETWID  29
                              4  K  WETWID  26"
                              )

  
testData <- read.table(bob, header=TRUE,stringsAsFactors=FALSE)
 
  close (bob)
return(testData)
}


metsBankMorphology.testResults <- function()
# creates dataframe of bank morphology metrics calculation results for unit test

{
                   bob2 <- textConnection("UID  METRIC  RESULT
                              1  n_ba  11
                              2  n_ba  11
                              3  n_ba  11
                              4  n_ba  11
                              1  bap_stp  18.18181818
                              2  bap_stp  18.18181818
                              3  bap_stp  0
                              4  bap_stp  36.36363636
                              1  bap_med  81.81818182
                              2  bap_med  72.72727273
                              3  bap_med  100
                              4  bap_med  54.54545455
                              1  bap_vst  0
                              2  bap_vst  0
                              3  bap_vst  0
                              4  bap_vst  0
                              1  bap_low  0
                              2  bap_low  9.090909091
                              3  bap_low  0
                              4  bap_low  9.090909091
                              1  bangmode  med
                              2  bangmode  med
                              3  bangmode  med
                              4  bangmode  med
                              1  n_w  11
                              2  n_w  11
                              3  n_w  11
                              4  n_w  11
                              5  n_ba  22
                              6  n_ba  22
                              8  n_ba  19
                              9  n_ba  22
                              10  n_ba  22
                              5  xbka  16.77272727
                              6  xbka  44.09090909
                              8  xbka  29.15789474
                              9  xbka  107.8636364
                              10  xbka  51.22727273
                              5  sdbk_a  17.52530638
                              6  sdbk_a  42.61891944
                              8  sdbk_a  21.98550825
                              9  sdbk_a  31.48891786
                              10  sdbk_a  29.64702156
                              5  bka_q3  20
                              6  bka_q3  70
                              8  bka_q3  40
                              9  bka_q3  131
                              10  bka_q3  52
                              5  medbk_a  13
                              6  medbk_a  23
                              8  medbk_a  24
                              9  medbk_a  116
                              10  medbk_a  45
                              5  bka_q1  5
                              6  bka_q1  10
                              8  bka_q1  13
                              9  bka_q1  100
                              10  bka_q1  32
                              5  intqbka  15
                              6  intqbka  60
                              8  intqbka  27
                              9  intqbka  31
                              10  intqbka  20
                              5  n_un  22
                              6  n_un  22
                              8  n_un  19
                              9  n_un  18
                              10  n_un  22
                              5  xun  0.028636364
                              6  xun  0.015909091
                              8  xun  0
                              9  xun  0.294444444
                              10  xun  0.040909091
                              5  sdun  0.074597042
                              6  sdun  0.044685045
                              8  sdun  0
                              9  sdun  0.134917065
                              10  sdun  0.10980108
                              5  bkun_q3  0
                              6  bkun_q3  0
                              8  bkun_q3  0
                              9  bkun_q3  0.4
                              10  bkun_q3  0
                              5  medbkun  0
                              6  medbkun  0
                              8  medbkun  0
                              9  medbkun  0.3
                              10  medbkun  0
                              5  bkun_q1  0
                              6  bkun_q1  0
                              8  bkun_q1  0
                              9  bkun_q1  0.2
                              10  bkun_q1  0
                              5  intqbkun  0
                              6  intqbkun  0
                              8  intqbkun  0
                              9  intqbkun  0.2
                              10  intqbkun  0"
                              )

  

     testResults  <- read.table(bob2, header=TRUE,stringsAsFactors=FALSE)
  close (bob2)
return(testResults)
}
