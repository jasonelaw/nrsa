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
  
 
metsChannelMorphology.cleanup <- function(chan)
# Clean up when function terminates
{
  odbcClose(chan)
}


metsChannelMorphology.createBankGeometryData <- function()
#
{
  #stream : angle, undercut
  #rivers : angle, wetted width
  #read in the test data from

   bob <- textConnection("UID  TRANSECT  PARAMETER  RESULT

                              1  A	WETWID	6.4
                              1	B	WETWID	10.5
                              1	C	WETWID	8.6
                              1	D	WETWID	7.1
                              1	E	WETWID	5.1
                              1	F	WETWID	7.5
                              1	G	WETWID	4.6
                              1	H	WETWID	5.7
                              1	I	WETWID	3.7
                              1	J	WETWID	5.8
                              1	K	WETWID	7.4
                              2	A	WETWID	10.8
                              2	B	WETWID	9.9
                              2	C	WETWID	7.3
                              2	D	WETWID	7
                              2	E	WETWID	5.6
                              2	F	WETWID	6
                              2	G	WETWID	5.1
                              2	H	WETWID	5.5
                              2	I	WETWID	3.5
                              2	J	WETWID	5.8
                              2	K	WETWID	6.7
                              3	A	WETWID	14
                              3	B	WETWID	5.7
                              3	C	WETWID	4.1
                              3	D	WETWID	7.3
                              3	E	WETWID	4.7
                              3	F	WETWID	5.5
                              3	G	WETWID	13.2
                              3	H	WETWID	3.7
                              3	I	WETWID	4.3
                              3	J	WETWID	7.5
                              3	K	WETWID	4
                              3	XC	WETWID	4.8
                              3	XK	WETWID	3.6
                              4	A	WETWID	13.5
                              4	B	WETWID	4.5
                              4	C	WETWID	3.7
                              4	D	WETWID	5.3
                              4	E	WETWID	4.4
                              4	F	WETWID	5.6
                              4	G	WETWID	7.4
                              4	H	WETWID	3.1
                              4	I	WETWID	3.9
                              4	J	WETWID	6.8
                              4	K	WETWID	3.4
                              4	XC	WETWID	2.5
                              4	XK	WETWID	2.5
                              5	A	WETWID	1.5
                              5	B	WETWID	1.5
                              5	C	WETWID	2
                              5	D	WETWID	1.8
                              5	E	WETWID	2.3
                              5	F	WETWID	1.7
                              5	G	WETWID	1.8
                              5	H	WETWID	1.6
                              5	I	WETWID	1.9
                              5	J	WETWID	2.4
                              5	K	WETWID	1.7
                              6	A	WETWID	1
                              6	B	WETWID	2.1
                              6	C	WETWID	1.2
                              6	D	WETWID	1.3
                              6	E	WETWID	2
                              6	F	WETWID	1.8
                              6	G	WETWID	2
                              6	H	WETWID	1.6
                              6	I	WETWID	0.6
                              6	J	WETWID	4.6
                              6	K	WETWID	1.8
                              7	A	WETWID	55
                              7	B	WETWID	48
                              7	C	WETWID	63
                              7	D	WETWID	48
                              7	E	WETWID	80
                              7	F	WETWID	77
                              7	G	WETWID	66
                              7	H	WETWID	95
                              7	I	WETWID	131
                              7	J	WETWID	37
                              7	K	WETWID	32
                              8	A	WETWID	126
                              8	B	WETWID	48
                              8	C	WETWID	49
                              8	D	WETWID	28
                              8	E	WETWID	58
                              8	F	WETWID	38
                              8	G	WETWID	65
                              8	H	WETWID	42
                              8	I	WETWID	36
                              8	J	WETWID	107
                              8	K	WETWID	69
                              9	A	WETWID	121
                              9	B	WETWID	52
                              9	C	WETWID	45
                              9	D	WETWID	76
                              9	E	WETWID	56
                              9	F	WETWID	36
                              9	G	WETWID	25
                              9	H	WETWID	60
                              9	I	WETWID	45
                              9	J	WETWID	49
                              9	K	WETWID	41
                              1	A	BANKWID	15
                              1	B	BANKWID	0
                              1	C	BANKWID	14
                              1	D	BANKWID	11.3
                              1	E	BANKWID	10.1
                              1	F	BANKWID	12.4
                              1	G	BANKWID	19
                              1	H	BANKWID	11
                              1	I	BANKWID	9.1
                              1	J	BANKWID	8.5
                              1	K	BANKWID	21.8
                              2	A	BANKWID	12.4
                              2	B	BANKWID	11.6
                              2	C	BANKWID	10.5
                              2	D	BANKWID	9.1
                              2	E	BANKWID	10.2
                              2	F	BANKWID	11.4
                              2	G	BANKWID	10.1
                              2	H	BANKWID	7.7
                              2	I	BANKWID	4.1
                              2	J	BANKWID	7.7
                              2	K	BANKWID	9.7
                              3	A	BANKWID	15
                              3	B	BANKWID	6.5
                              3	C	BANKWID	4.8
                              3	D	BANKWID	8
                              3	E	BANKWID	5.7
                              3	F	BANKWID	7.1
                              3	G	BANKWID	14
                              3	H	BANKWID	6.4
                              3	I	BANKWID	5.5
                              3	J	BANKWID	10
                              3	K	BANKWID	4.5
                              3	XC	BANKWID	5.5
                              3	XK	BANKWID	4.3
                              4	A	BANKWID	15.3
                              4	B	BANKWID	6.8
                              4	C	BANKWID	4.8
                              4	D	BANKWID	7.8
                              4	E	BANKWID	5.5
                              4	F	BANKWID	7.5
                              4	G	BANKWID	13.7
                              4	H	BANKWID	6.9
                              4	I	BANKWID	5.2
                              4	J	BANKWID	10
                              4	K	BANKWID	3.9
                              4	XC	BANKWID	5.5
                              4	XK	BANKWID	4.7
                              5	A	BANKWID	1.5
                              5	B	BANKWID	1.5
                              5	C	BANKWID	2
                              5	D	BANKWID	1.7
                              5	E	BANKWID	2
                              5	F	BANKWID	1.5
                              5	G	BANKWID	1.6
                              5	H	BANKWID	1.6
                              5	I	BANKWID	1.7
                              5	J	BANKWID	2.3
                              5	K	BANKWID	1.7
                              6	A	BANKWID	1.3
                              6	B	BANKWID	2.3
                              6	C	BANKWID	1.4
                              6	D	BANKWID	1.6
                              6	E	BANKWID	2.1
                              6	F	BANKWID	1.9
                              6	G	BANKWID	2.3
                              6	H	BANKWID	1.7
                              6	I	BANKWID	3.9
                              6	J	BANKWID	4.6
                              6	K	BANKWID	1.9
                              7	A	BANKWID	60
                              7	B	BANKWID	50
                              7	C	BANKWID	68
                              7	E	BANKWID	88
                              7	F	BANKWID	85
                              7	G	BANKWID	75
                              7	H	BANKWID	100
                              7	I	BANKWID	140
                              7	J	BANKWID	40
                              7	K	BANKWID	75
                              8	A	BANKWID	138
                              8	B	BANKWID	244
                              8	C	BANKWID	227
                              8	D	BANKWID	86
                              8	E	BANKWID	96
                              8	F	BANKWID	115
                              8	G	BANKWID	200
                              8	H	BANKWID	120
                              8	I	BANKWID	175
                              8	J	BANKWID	115
                              8	K	BANKWID	139
                              9	A	BANKWID	125
                              9	B	BANKWID	123
                              9	C	BANKWID	171
                              9	D	BANKWID	130
                              9	E	BANKWID	160
                              9	F	BANKWID	114
                              9	G	BANKWID	83
                              9	H	BANKWID	218
                              9	I	BANKWID	300
                              9	J	BANKWID	89
                              9	K	BANKWID	275
                              1	A	BANKHGT	0.9
                              1	B	BANKHGT	0.6
                              1	C	BANKHGT	0.7
                              1	D	BANKHGT	0.5
                              1	E	BANKHGT	0.4
                              1	F	BANKHGT	0.4
                              1	G	BANKHGT	0.5
                              1	H	BANKHGT	0.6
                              1	I	BANKHGT	0.3
                              1	J	BANKHGT	0.6
                              1	K	BANKHGT	0.6
                              2	A	BANKHGT	0.3
                              2	B	BANKHGT	0.4
                              2	C	BANKHGT	0.5
                              2	D	BANKHGT	0.4
                              2	E	BANKHGT	0.3
                              2	F	BANKHGT	0.3
                              2	G	BANKHGT	0.5
                              2	H	BANKHGT	0.3
                              2	I	BANKHGT	0.6
                              2	J	BANKHGT	0.3
                              2	K	BANKHGT	0.4
                              3	A	BANKHGT	0.35
                              3	B	BANKHGT	0.25
                              3	C	BANKHGT	0.3
                              3	D	BANKHGT	0.28
                              3	E	BANKHGT	0.2
                              3	F	BANKHGT	0.25
                              3	G	BANKHGT	0.25
                              3	H	BANKHGT	0.25
                              3	I	BANKHGT	0.2
                              3	J	BANKHGT	0.2
                              3	K	BANKHGT	0.13
                              3	XC	BANKHGT	0.15
                              3	XK	BANKHGT	0.2
                              4	A	BANKHGT	0.45
                              4	B	BANKHGT	0.4
                              4	C	BANKHGT	0.4
                              4	D	BANKHGT	0.4
                              4	E	BANKHGT	0.4
                              4	F	BANKHGT	0.35
                              4	G	BANKHGT	0.35
                              4	H	BANKHGT	0.3
                              4	I	BANKHGT	0.35
                              4	J	BANKHGT	0.4
                              4	K	BANKHGT	0.2
                              4	XC	BANKHGT	0.3
                              4	XK	BANKHGT	0.25
                              5	A	BANKHGT	0
                              5	B	BANKHGT	0
                              5	C	BANKHGT	0
                              5	D	BANKHGT	0.1
                              5	E	BANKHGT	0.1
                              5	F	BANKHGT	0.1
                              5	G	BANKHGT	0.1
                              5	H	BANKHGT	0
                              5	I	BANKHGT	0.1
                              5	J	BANKHGT	0.1
                              5	K	BANKHGT	0.1
                              6	A	BANKHGT	0
                              6	B	BANKHGT	0.1
                              6	C	BANKHGT	0
                              6	D	BANKHGT	0.1
                              6	E	BANKHGT	0.1
                              6	F	BANKHGT	0.1
                              6	G	BANKHGT	0
                              6	H	BANKHGT	0.1
                              6	I	BANKHGT	0.1
                              6	J	BANKHGT	0
                              6	K	BANKHGT	0.1
                              7	A	BANKHGT	1.3
                              7	B	BANKHGT	1.3
                              7	C	BANKHGT	2
                              7	D	BANKHGT	2.7
                              7	E	BANKHGT	2
                              7	F	BANKHGT	2
                              7	G	BANKHGT	2
                              7	H	BANKHGT	2
                              7	I	BANKHGT	2
                              7	J	BANKHGT	2
                              7	K	BANKHGT	2
                              8	A	BANKHGT	3.5
                              8	B	BANKHGT	2.5
                              8	C	BANKHGT	3.2
                              8	D	BANKHGT	3
                              8	E	BANKHGT	4
                              8	F	BANKHGT	4
                              8	G	BANKHGT	3.6
                              8	H	BANKHGT	4
                              8	I	BANKHGT	3
                              8	J	BANKHGT	3
                              8	K	BANKHGT	3
                              9	A	BANKHGT	3.8
                              9	B	BANKHGT	3.7
                              9	C	BANKHGT	3.1
                              9	D	BANKHGT	3.1
                              9	E	BANKHGT	2.9
                              9	F	BANKHGT	3
                              9	G	BANKHGT	3.6
                              9	H	BANKHGT	4
                              9	I	BANKHGT	4
                              9	J	BANKHGT	4
                              9	K	BANKHGT	4
                              1	A	INCISHGT	0
                              1	B	INCISHGT	2.1
                              1	C	INCISHGT	2.4
                              1	D	INCISHGT	2.5
                              1	E	INCISHGT	1.8
                              1	F	INCISHGT	0
                              1	G	INCISHGT	1.9
                              1	H	INCISHGT	0.6
                              1	I	INCISHGT	2.2
                              1	J	INCISHGT	2.2
                              1	K	INCISHGT	2.1
                              2	A	INCISHGT	1.9
                              2	B	INCISHGT	1.5
                              2	C	INCISHGT	1.3
                              2	D	INCISHGT	1.3
                              2	E	INCISHGT	0.8
                              2	F	INCISHGT	1
                              2	G	INCISHGT	1.2
                              2	H	INCISHGT	1
                              2	I	INCISHGT	1.8
                              2	J	INCISHGT	1.8
                              2	K	INCISHGT	2.2
                              3	A	INCISHGT	1.1
                              3	B	INCISHGT	1.2
                              3	C	INCISHGT	0.7
                              3	D	INCISHGT	0.4
                              3	E	INCISHGT	0.6
                              3	F	INCISHGT	0.55
                              3	G	INCISHGT	0.7
                              3	H	INCISHGT	0.9
                              3	I	INCISHGT	0.7
                              3	J	INCISHGT	0.4
                              3	K	INCISHGT	0.85
                              3	XC	INCISHGT	1.2
                              3	XK	INCISHGT	0.85
                              4	A	INCISHGT	1.2
                              4	B	INCISHGT	1.3
                              4	C	INCISHGT	0.65
                              4	D	INCISHGT	0.5
                              4	E	INCISHGT	0.7
                              4	F	INCISHGT	0.6
                              4	G	INCISHGT	1.05
                              4	H	INCISHGT	1
                              4	I	INCISHGT	0.8
                              4	J	INCISHGT	0.7
                              4	K	INCISHGT	0.9
                              4	XC	INCISHGT	1
                              4	XK	INCISHGT	0.9
                              5	A	INCISHGT	0.6
                              5	B	INCISHGT	1.6
                              5	C	INCISHGT	0.8
                              5	D	INCISHGT	0.8
                              5	E	INCISHGT	0.7
                              5	F	INCISHGT	0.7
                              5	G	INCISHGT	0.8
                              5	H	INCISHGT	0.8
                              5	I	INCISHGT	0.8
                              5	J	INCISHGT	0.6
                              5	K	INCISHGT	0.6
                              6	A	INCISHGT	0.8
                              6	B	INCISHGT	0.6
                              6	C	INCISHGT	0.3
                              6	D	INCISHGT	0.4
                              6	E	INCISHGT	0.3
                              6	F	INCISHGT	0.3
                              6	G	INCISHGT	0.3
                              6	H	INCISHGT	0.2
                              6	I	INCISHGT	0.3
                              6	J	INCISHGT	0.8
                              6	K	INCISHGT	0.3
                              7	A	INCISHGT	5.8
                              7	B	INCISHGT	5
                              7	C	INCISHGT	6
                              7	D	INCISHGT	3.5
                              7	E	INCISHGT	5
                              7	F	INCISHGT	4
                              7	G	INCISHGT	8
                              7	H	INCISHGT	6
                              7	I	INCISHGT	7
                              7	J	INCISHGT	7
                              7	K	INCISHGT	6
                              8	A	INCISHGT	10
                              8	B	INCISHGT	10
                              8	C	INCISHGT	15
                              8	D	INCISHGT	6
                              8	E	INCISHGT	15
                              8	F	INCISHGT	6
                              8	G	INCISHGT	10
                              8	H	INCISHGT	10
                              8	I	INCISHGT	15
                              8	J	INCISHGT	15
                              8	K	INCISHGT	10
                              9	A	INCISHGT	20
                              9	B	INCISHGT	6
                              9	C	INCISHGT	10
                              9	D	INCISHGT	4
                              9	E	INCISHGT	8
                              9	F	INCISHGT	7
                              9	G	INCISHGT	8
                              9	H	INCISHGT	4
                              9	I	INCISHGT	10
                              9	J	INCISHGT	10
                              9	K	INCISHGT	15"
                            )
  df1 <- read.table(bob, header=TRUE,stringsAsFactors=FALSE)
  close (bob)

  df1$TRANSECT <- as.character(df1$TRANSECT)
  df1$TRANSDIR <- 'NONE'
  df1$PARAMETER <- as.character(df1$PARAMETER)
  df1$RESULT <- as.character(df1$RESULT)
  df1$SAMPLE_TYPE <- ifelse(df1$UID %in% 1:6, 'PHAB_CHANW', 'PHAB_CHANBFRONT')
  df1$FLAG <- NA
  df1$UNITS <- "NONE"

  return(df1)
}


metsChannelMorphology.createThalwegData <- function()
#
{
   bob2 <- textConnection("UID	TRANSECT	STATION	PARAMETER	RESULT	UNITS
                            1	A	0	DEPTH	94	CM
                            1	A	1	DEPTH	95	CM
                            1	A	2	DEPTH	76	CM
                            1	A	3	DEPTH	76	CM
                            1	A	4	DEPTH	36	CM
                            1	A	5	DEPTH	39	CM
                            1	A	6	DEPTH	48	CM
                            1	A	7	DEPTH	44	CM
                            1	A	8	DEPTH	40	CM
                            1	A	9	DEPTH	29	CM
                            1	B	0	DEPTH	21	CM
                            1	B	1	DEPTH	35	CM
                            1	B	2	DEPTH	38	CM
                            1	B	3	DEPTH	29	CM
                            1	B	4	DEPTH	28	CM
                            1	B	5	DEPTH	26	CM
                            1	B	6	DEPTH	20	CM
                            1	B	7	DEPTH	34	CM
                            1	B	8	DEPTH	32	CM
                            1	B	9	DEPTH	36	CM
                            1	C	0	DEPTH	52	CM
                            1	C	1	DEPTH	38	CM
                            1	C	2	DEPTH	35	CM
                            1	C	3	DEPTH	36	CM
                            1	C	4	DEPTH	47	CM
                            1	C	5	DEPTH	59	CM
                            1	C	6	DEPTH	60	CM
                            1	C	7	DEPTH	64	CM
                            1	C	8	DEPTH	59	CM
                            1	C	9	DEPTH	65	CM
                            1	D	0	DEPTH	54	CM
                            1	D	1	DEPTH	30	CM
                            1	D	2	DEPTH	41	CM
                            1	D	3	DEPTH	21	CM
                            1	D	4	DEPTH	22	CM
                            1	D	5	DEPTH	21	CM
                            1	D	6	DEPTH	30	CM
                            1	D	7	DEPTH	26	CM
                            1	D	8	DEPTH	24	CM
                            1	D	9	DEPTH	25	CM
                            1	E	0	DEPTH	20	CM
                            1	E	1	DEPTH	25	CM
                            1	E	2	DEPTH	23	CM
                            1	E	3	DEPTH	15	CM
                            1	E	4	DEPTH	19	CM
                            1	E	5	DEPTH	28	CM
                            1	E	6	DEPTH	25	CM
                            1	E	7	DEPTH	19	CM
                            1	E	8	DEPTH	16	CM
                            1	E	9	DEPTH	14	CM
                            1	F	0	DEPTH	13	CM
                            1	F	1	DEPTH	15	CM
                            1	F	2	DEPTH	22	CM
                            1	F	3	DEPTH	17	CM
                            1	F	4	DEPTH	14	CM
                            1	F	5	DEPTH	21	CM
                            1	F	6	DEPTH	15	CM
                            1	F	7	DEPTH	15	CM
                            1	F	8	DEPTH	14	CM
                            1	F	9	DEPTH	21	CM
                            1	G	0	DEPTH	13	CM
                            1	G	1	DEPTH	14	CM
                            1	G	2	DEPTH	15	CM
                            1	G	3	DEPTH	12	CM
                            1	G	4	DEPTH	19	CM
                            1	G	5	DEPTH	49	CM
                            1	G	6	DEPTH	85	CM
                            1	G	7	DEPTH	92	CM
                            1	G	8	DEPTH	104	CM
                            1	G	9	DEPTH	79	CM
                            1	H	0	DEPTH	72	CM
                            1	H	1	DEPTH	115	CM
                            1	H	2	DEPTH	125	CM
                            1	H	3	DEPTH	114	CM
                            1	H	4	DEPTH	93	CM
                            1	H	5	DEPTH	90	CM
                            1	H	6	DEPTH	29	CM
                            1	H	7	DEPTH	22	CM
                            1	H	8	DEPTH	37	CM
                            1	H	9	DEPTH	17	CM
                            1	I	0	DEPTH	20	CM
                            1	I	1	DEPTH	12	CM
                            1	I	2	DEPTH	17	CM
                            1	I	3	DEPTH	22	CM
                            1	I	4	DEPTH	14	CM
                            1	I	5	DEPTH	18	CM
                            1	I	6	DEPTH	17	CM
                            1	I	7	DEPTH	18	CM
                            1	I	8	DEPTH	16	CM
                            1	I	9	DEPTH	16	CM
                            1	J	0	DEPTH	17	CM
                            1	J	1	DEPTH	18	CM
                            1	J	2	DEPTH	23	CM
                            1	J	3	DEPTH	21	CM
                            1	J	4	DEPTH	24	CM
                            1	J	5	DEPTH	20	CM
                            1	J	6	DEPTH	14	CM
                            1	J	7	DEPTH	13	CM
                            1	J	8	DEPTH	16	CM
                            1	J	9	DEPTH	18	CM
                            2	A	0	DEPTH	21	CM
                            2	A	1	DEPTH	46	CM
                            2	A	2	DEPTH	30	CM
                            2	A	3	DEPTH	29	CM
                            2	A	4	DEPTH	28	CM
                            2	A	5	DEPTH	25	CM
                            2	A	6	DEPTH	21	CM
                            2	A	7	DEPTH	25	CM
                            2	A	8	DEPTH	25	CM
                            2	A	9	DEPTH	35	CM
                            2	B	0	DEPTH	37	CM
                            2	B	1	DEPTH	42	CM
                            2	B	2	DEPTH	57	CM
                            2	B	3	DEPTH	47	CM
                            2	B	4	DEPTH	34	CM
                            2	B	5	DEPTH	35	CM
                            2	B	6	DEPTH	51	CM
                            2	B	7	DEPTH	60	CM
                            2	B	8	DEPTH	50	CM
                            2	B	9	DEPTH	60	CM
                            2	C	0	DEPTH	67	CM
                            2	C	1	DEPTH	62	CM
                            2	C	2	DEPTH	70	CM
                            2	C	3	DEPTH	60	CM
                            2	C	4	DEPTH	50	CM
                            2	C	5	DEPTH	37	CM
                            2	C	6	DEPTH	40	CM
                            2	C	7	DEPTH	20	CM
                            2	C	8	DEPTH	21	CM
                            2	C	9	DEPTH	23	CM
                            2	D	0	DEPTH	25	CM
                            2	D	1	DEPTH	40	CM
                            2	D	2	DEPTH	25	CM
                            2	D	3	DEPTH	27	CM
                            2	D	4	DEPTH	25	CM
                            2	D	5	DEPTH	21	CM
                            2	D	6	DEPTH	25	CM
                            2	D	7	DEPTH	30	CM
                            2	D	8	DEPTH	26	CM
                            2	D	9	DEPTH	17	CM
                            2	E	0	DEPTH	20	CM
                            2	E	1	DEPTH	27	CM
                            2	E	2	DEPTH	30	CM
                            2	E	3	DEPTH	25	CM
                            2	E	4	DEPTH	15	CM
                            2	E	5	DEPTH	18	CM
                            2	E	6	DEPTH	15	CM
                            2	E	7	DEPTH	15	CM
                            2	E	8	DEPTH	20	CM
                            2	E	9	DEPTH	23	CM
                            2	F	0	DEPTH	20	CM
                            2	F	1	DEPTH	20	CM
                            2	F	2	DEPTH	18	CM
                            2	F	3	DEPTH	25	CM
                            2	F	4	DEPTH	15	CM
                            2	F	5	DEPTH	15	CM
                            2	F	6	DEPTH	25	CM
                            2	F	7	DEPTH	20	CM
                            2	F	8	DEPTH	20	CM
                            2	F	9	DEPTH	13	CM
                            2	G	0	DEPTH	15	CM
                            2	G	1	DEPTH	14	CM
                            2	G	2	DEPTH	16	CM
                            2	G	3	DEPTH	17	CM
                            2	G	4	DEPTH	22	CM
                            2	G	5	DEPTH	95	CM
                            2	G	6	DEPTH	100	CM
                            2	G	7	DEPTH	90	CM
                            2	G	8	DEPTH	80	CM
                            2	G	9	DEPTH	65	CM
                            2	H	0	DEPTH	70	CM
                            2	H	1	DEPTH	120	CM
                            2	H	2	DEPTH	120	CM
                            2	H	3	DEPTH	110	CM
                            2	H	4	DEPTH	110	CM
                            2	H	5	DEPTH	100	CM
                            2	H	6	DEPTH	90	CM
                            2	H	7	DEPTH	17	CM
                            2	H	8	DEPTH	20	CM
                            2	H	9	DEPTH	25	CM
                            2	I	0	DEPTH	30	CM
                            2	I	1	DEPTH	23	CM
                            2	I	2	DEPTH	20	CM
                            2	I	3	DEPTH	20	CM
                            2	I	4	DEPTH	15	CM
                            2	I	5	DEPTH	20	CM
                            2	I	6	DEPTH	25	CM
                            2	I	7	DEPTH	15	CM
                            2	I	8	DEPTH	20	CM
                            2	I	9	DEPTH	20	CM
                            2	J	0	DEPTH	22	CM
                            2	J	1	DEPTH	22	CM
                            2	J	2	DEPTH	19	CM
                            2	J	3	DEPTH	20	CM
                            2	J	4	DEPTH	23	CM
                            2	J	5	DEPTH	19	CM
                            2	J	6	DEPTH	23	CM
                            2	J	7	DEPTH	21	CM
                            2	J	8	DEPTH	23	CM
                            2	J	9	DEPTH	27	CM
                            3	A	0	DEPTH	31	CM
                            3	A	1	DEPTH	36	CM
                            3	A	2	DEPTH	41	CM
                            3	A	3	DEPTH	45	CM
                            3	A	4	DEPTH	51	CM
                            3	A	5	DEPTH	65	CM
                            3	A	6	DEPTH	72	CM
                            3	A	7	DEPTH	75	CM
                            3	B	0	DEPTH	47	CM
                            3	B	1	DEPTH	40	CM
                            3	B	2	DEPTH	40	CM
                            3	B	3	DEPTH	37	CM
                            3	B	4	DEPTH	40	CM
                            3	B	5	DEPTH	40	CM
                            3	B	6	DEPTH	47	CM
                            3	B	7	DEPTH	52	CM
                            3	B	8	DEPTH	56	CM
                            3	B	9	DEPTH	63	CM
                            3	B	10	DEPTH	75	CM
                            3	C	0	DEPTH	80	CM
                            3	C	1	DEPTH	63	CM
                            3	C	2	DEPTH	42	CM
                            3	C	3	DEPTH	40	CM
                            3	C	4	DEPTH	35	CM
                            3	C	5	DEPTH	38	CM
                            3	C	6	DEPTH	45	CM
                            3	C	7	DEPTH	48	CM
                            3	C	8	DEPTH	56	CM
                            3	C	9	DEPTH	52	CM
                            3	C	10	DEPTH	60	CM
                            3	D	0	DEPTH	55	CM
                            3	D	1	DEPTH	58	CM
                            3	D	2	DEPTH	60	CM
                            3	D	3	DEPTH	68	CM
                            3	D	4	DEPTH	80	CM
                            3	D	5	DEPTH	82	CM
                            3	D	6	DEPTH	110	CM
                            3	D	7	DEPTH	85	CM
                            3	D	8	DEPTH	62	CM
                            3	D	9	DEPTH	52	CM
                            3	D	10	DEPTH	40	CM
                            3	E	0	DEPTH	44	CM
                            3	E	1	DEPTH	52	CM
                            3	E	2	DEPTH	70	CM
                            3	E	3	DEPTH	62	CM
                            3	E	4	DEPTH	60	CM
                            3	E	5	DEPTH	62	CM
                            3	E	6	DEPTH	40	CM
                            3	E	7	DEPTH	44	CM
                            3	E	8	DEPTH	54	CM
                            3	E	9	DEPTH	45	CM
                            3	E	10	DEPTH	55	CM
                            3	F	0	DEPTH	38	CM
                            3	F	1	DEPTH	36	CM
                            3	F	2	DEPTH	46	CM
                            3	F	3	DEPTH	40	CM
                            3	F	4	DEPTH	46	CM
                            3	F	5	DEPTH	44	CM
                            3	F	6	DEPTH	56	CM
                            3	F	7	DEPTH	76	CM
                            3	F	8	DEPTH	100	CM
                            3	F	9	DEPTH	32	CM
                            3	F	10	DEPTH	45	CM
                            3	G	0	DEPTH	44	CM
                            3	G	1	DEPTH	40	CM
                            3	G	2	DEPTH	38	CM
                            3	G	3	DEPTH	46	CM
                            3	G	4	DEPTH	60	CM
                            3	G	5	DEPTH	64	CM
                            3	G	6	DEPTH	65	CM
                            3	G	7	DEPTH	48	CM
                            3	G	8	DEPTH	56	CM
                            3	G	9	DEPTH	66	CM
                            3	G	10	DEPTH	78	CM
                            3	H	0	DEPTH	80	CM
                            3	H	1	DEPTH	78	CM
                            3	H	2	DEPTH	72	CM
                            3	H	3	DEPTH	84	CM
                            3	H	4	DEPTH	160	CM
                            3	H	5	DEPTH	96	CM
                            3	H	6	DEPTH	170	CM
                            3	H	7	DEPTH	170	CM
                            3	H	8	DEPTH	40	CM
                            3	H	9	DEPTH	46	CM
                            3	H	10	DEPTH	42	CM
                            3	I	0	DEPTH	45	CM
                            3	I	1	DEPTH	40	CM
                            3	I	2	DEPTH	42	CM
                            3	I	3	DEPTH	44	CM
                            3	I	4	DEPTH	44	CM
                            3	I	5	DEPTH	45	CM
                            3	I	6	DEPTH	60	CM
                            3	I	7	DEPTH	58	CM
                            3	I	8	DEPTH	70	CM
                            3	I	9	DEPTH	68	CM
                            3	I	10	DEPTH	50	CM
                            3	J	0	DEPTH	50	CM
                            3	J	1	DEPTH	54	CM
                            3	J	2	DEPTH	58	CM
                            3	J	3	DEPTH	35	CM
                            3	J	4	DEPTH	46	CM
                            3	J	5	DEPTH	40	CM
                            3	J	6	DEPTH	40	CM
                            3	J	7	DEPTH	42	CM
                            3	J	8	DEPTH	40	CM
                            3	J	9	DEPTH	45	CM
                            3	J	10	DEPTH	44	CM
                            4	A	0	DEPTH	19	CM
                            4	A	1	DEPTH	24	CM
                            4	A	2	DEPTH	32	CM
                            4	A	3	DEPTH	41	CM
                            4	A	4	DEPTH	62	CM
                            4	A	5	DEPTH	64	CM
                            4	A	6	DEPTH	60	CM
                            4	A	7	DEPTH	43	CM
                            4	A	8	DEPTH	50	CM
                            4	A	9	DEPTH	45	CM
                            4	A	10	DEPTH	41	CM
                            4	B	0	DEPTH	25	CM
                            4	B	1	DEPTH	18	CM
                            4	B	2	DEPTH	23	CM
                            4	B	3	DEPTH	24	CM
                            4	B	4	DEPTH	31	CM
                            4	B	5	DEPTH	33	CM
                            4	B	6	DEPTH	36	CM
                            4	B	7	DEPTH	26	CM
                            4	B	8	DEPTH	35	CM
                            4	B	9	DEPTH	37	CM
                            4	B	10	DEPTH	57	CM
                            4	C	0	DEPTH	60	CM
                            4	C	1	DEPTH	48	CM
                            4	C	2	DEPTH	23	CM
                            4	C	3	DEPTH	24	CM
                            4	C	4	DEPTH	22	CM
                            4	C	5	DEPTH	29	CM
                            4	C	6	DEPTH	27	CM
                            4	C	7	DEPTH	32	CM
                            4	C	8	DEPTH	40	CM
                            4	C	9	DEPTH	29	CM
                            4	C	10	DEPTH	30	CM
                            4	D	0	DEPTH	43	CM
                            4	D	1	DEPTH	41	CM
                            4	D	2	DEPTH	42	CM
                            4	D	3	DEPTH	53	CM
                            4	D	4	DEPTH	70	CM
                            4	D	5	DEPTH	85	CM
                            4	D	6	DEPTH	90	CM
                            4	D	7	DEPTH	76	CM
                            4	D	8	DEPTH	40	CM
                            4	D	9	DEPTH	27	CM
                            4	D	10	DEPTH	35	CM
                            4	E	0	DEPTH	28	CM
                            4	E	1	DEPTH	33	CM
                            4	E	2	DEPTH	37	CM
                            4	E	3	DEPTH	47	CM
                            4	E	4	DEPTH	39	CM
                            4	E	5	DEPTH	43	CM
                            4	E	6	DEPTH	27	CM
                            4	E	7	DEPTH	32	CM
                            4	E	8	DEPTH	31	CM
                            4	E	9	DEPTH	39	CM
                            4	E	10	DEPTH	31	CM
                            4	F	0	DEPTH	28	CM
                            4	F	1	DEPTH	35	CM
                            4	F	2	DEPTH	26	CM
                            4	F	3	DEPTH	36	CM
                            4	F	4	DEPTH	27	CM
                            4	F	5	DEPTH	78	CM
                            4	F	6	DEPTH	33	CM
                            4	F	7	DEPTH	67	CM
                            4	F	8	DEPTH	78	CM
                            4	F	9	DEPTH	23	CM
                            4	F	10	DEPTH	27	CM
                            4	G	0	DEPTH	24	CM
                            4	G	1	DEPTH	32	CM
                            4	G	2	DEPTH	24	CM
                            4	G	3	DEPTH	31	CM
                            4	G	4	DEPTH	45	CM
                            4	G	5	DEPTH	46	CM
                            4	G	6	DEPTH	43	CM
                            4	G	7	DEPTH	35	CM
                            4	G	8	DEPTH	46	CM
                            4	G	9	DEPTH	51	CM
                            4	G	10	DEPTH	64	CM
                            4	H	0	DEPTH	67	CM
                            4	H	1	DEPTH	53	CM
                            4	H	2	DEPTH	49	CM
                            4	H	3	DEPTH	68	CM
                            4	H	4	DEPTH	81	CM
                            4	H	5	DEPTH	68	CM
                            4	H	6	DEPTH	82	CM
                            4	H	7	DEPTH	83	CM
                            4	H	8	DEPTH	28	CM
                            4	H	9	DEPTH	38	CM
                            4	H	10	DEPTH	35	CM
                            4	I	0	DEPTH	26	CM
                            4	I	1	DEPTH	35	CM
                            4	I	2	DEPTH	26	CM
                            4	I	3	DEPTH	24	CM
                            4	I	4	DEPTH	34	CM
                            4	I	5	DEPTH	32	CM
                            4	I	6	DEPTH	28	CM
                            4	I	7	DEPTH	37	CM
                            4	I	8	DEPTH	50	CM
                            4	I	9	DEPTH	45	CM
                            4	I	10	DEPTH	32	CM
                            4	J	0	DEPTH	29	CM
                            4	J	1	DEPTH	36	CM
                            4	J	2	DEPTH	27	CM
                            4	J	3	DEPTH	29	CM
                            4	J	4	DEPTH	21	CM
                            4	J	5	DEPTH	30	CM
                            4	J	6	DEPTH	30	CM
                            4	J	7	DEPTH	28	CM
                            4	J	8	DEPTH	31	CM
                            4	J	9	DEPTH	37	CM
                            4	J	10	DEPTH	32	CM
                            5	A	0	DEPTH	35	CM
                            5	A	1	DEPTH	38	CM
                            5	A	2	DEPTH	77	CM
                            5	A	3	DEPTH	39	CM
                            5	A	4	DEPTH	35	CM
                            5	A	5	DEPTH	39	CM
                            5	A	6	DEPTH	33	CM
                            5	A	7	DEPTH	37	CM
                            5	A	8	DEPTH	36	CM
                            5	A	9	DEPTH	31	CM
                            5	A	10	DEPTH	33	CM
                            5	A	11	DEPTH	35	CM
                            5	A	12	DEPTH	35	CM
                            5	A	13	DEPTH	36	CM
                            5	A	14	DEPTH	34	CM
                            5	B	0	DEPTH	26	CM
                            5	B	1	DEPTH	32	CM
                            5	B	2	DEPTH	30	CM
                            5	B	3	DEPTH	31	CM
                            5	B	4	DEPTH	34	CM
                            5	B	5	DEPTH	35	CM
                            5	B	6	DEPTH	36	CM
                            5	B	7	DEPTH	35	CM
                            5	B	8	DEPTH	28	CM
                            5	B	9	DEPTH	27	CM
                            5	B	10	DEPTH	33	CM
                            5	B	11	DEPTH	34	CM
                            5	B	12	DEPTH	44	CM
                            5	B	13	DEPTH	55	CM
                            5	B	14	DEPTH	59	CM
                            5	C	0	DEPTH	26	CM
                            5	C	1	DEPTH	28	CM
                            5	C	2	DEPTH	30	CM
                            5	C	3	DEPTH	25	CM
                            5	C	4	DEPTH	17	CM
                            5	C	5	DEPTH	24	CM
                            5	C	6	DEPTH	21	CM
                            5	C	7	DEPTH	21	CM
                            5	C	8	DEPTH	25	CM
                            5	C	9	DEPTH	31	CM
                            5	C	10	DEPTH	40	CM
                            5	C	11	DEPTH	45	CM
                            5	C	12	DEPTH	42	CM
                            5	C	13	DEPTH	40	CM
                            5	C	14	DEPTH	36	CM
                            5	D	0	DEPTH	36	CM
                            5	D	1	DEPTH	28	CM
                            5	D	2	DEPTH	32	CM
                            5	D	3	DEPTH	23	CM
                            5	D	4	DEPTH	30	CM
                            5	D	5	DEPTH	27	CM
                            5	D	6	DEPTH	28	CM
                            5	D	7	DEPTH	28	CM
                            5	D	8	DEPTH	26	CM
                            5	D	9	DEPTH	27	CM
                            5	D	10	DEPTH	25	CM
                            5	D	11	DEPTH	25	CM
                            5	D	12	DEPTH	25	CM
                            5	D	13	DEPTH	30	CM
                            5	D	14	DEPTH	46	CM
                            5	E	0	DEPTH	43	CM
                            5	E	1	DEPTH	41	CM
                            5	E	2	DEPTH	75	CM
                            5	E	3	DEPTH	33	CM
                            5	E	4	DEPTH	34	CM
                            5	E	5	DEPTH	36	CM
                            5	E	6	DEPTH	34	CM
                            5	E	7	DEPTH	35	CM
                            5	E	8	DEPTH	38	CM
                            5	E	9	DEPTH	34	CM
                            5	E	10	DEPTH	31	CM
                            5	E	11	DEPTH	26	CM
                            5	E	12	DEPTH	31	CM
                            5	E	13	DEPTH	27	CM
                            5	E	14	DEPTH	34	CM
                            5	F	0	DEPTH	35	CM
                            5	F	1	DEPTH	40	CM
                            5	F	2	DEPTH	37	CM
                            5	F	3	DEPTH	39	CM
                            5	F	4	DEPTH	31	CM
                            5	F	5	DEPTH	35	CM
                            5	F	6	DEPTH	33	CM
                            5	F	7	DEPTH	40	CM
                            5	F	8	DEPTH	41	CM
                            5	F	9	DEPTH	34	CM
                            5	F	10	DEPTH	31	CM
                            5	F	11	DEPTH	30	CM
                            5	F	12	DEPTH	24	CM
                            5	F	13	DEPTH	30	CM
                            5	F	14	DEPTH	34	CM
                            5	G	0	DEPTH	36	CM
                            5	G	1	DEPTH	23	CM
                            5	G	2	DEPTH	25	CM
                            5	G	3	DEPTH	26	CM
                            5	G	4	DEPTH	36	CM
                            5	G	5	DEPTH	35	CM
                            5	G	6	DEPTH	34	CM
                            5	G	7	DEPTH	29	CM
                            5	G	8	DEPTH	27	CM
                            5	G	9	DEPTH	35	CM
                            5	G	10	DEPTH	28	CM
                            5	G	11	DEPTH	30	CM
                            5	G	12	DEPTH	25	CM
                            5	G	13	DEPTH	25	CM
                            5	G	14	DEPTH	31	CM
                            5	H	0	DEPTH	33	CM
                            5	H	1	DEPTH	30	CM
                            5	H	2	DEPTH	34	CM
                            5	H	3	DEPTH	35	CM
                            5	H	4	DEPTH	30	CM
                            5	H	5	DEPTH	27	CM
                            5	H	6	DEPTH	30	CM
                            5	H	7	DEPTH	45	CM
                            5	H	8	DEPTH	45	CM
                            5	H	9	DEPTH	30	CM
                            5	H	10	DEPTH	36	CM
                            5	H	11	DEPTH	33	CM
                            5	H	12	DEPTH	26	CM
                            5	H	13	DEPTH	26	CM
                            5	H	14	DEPTH	47	CM
                            5	I	0	DEPTH	52	CM
                            5	I	1	DEPTH	39	CM
                            5	I	2	DEPTH	34	CM
                            5	I	3	DEPTH	34	CM
                            5	I	4	DEPTH	29	CM
                            5	I	5	DEPTH	21	CM
                            5	I	6	DEPTH	24	CM
                            5	I	7	DEPTH	25	CM
                            5	I	8	DEPTH	27	CM
                            5	I	9	DEPTH	30	CM
                            5	I	10	DEPTH	35	CM
                            5	I	11	DEPTH	30	CM
                            5	I	12	DEPTH	36	CM
                            5	I	13	DEPTH	40	CM
                            5	I	14	DEPTH	40	CM
                            5	J	0	DEPTH	30	CM
                            5	J	1	DEPTH	21	CM
                            5	J	2	DEPTH	35	CM
                            5	J	3	DEPTH	40	CM
                            5	J	4	DEPTH	36	CM
                            5	J	5	DEPTH	31	CM
                            5	J	6	DEPTH	40	CM
                            5	J	7	DEPTH	45	CM
                            5	J	8	DEPTH	37	CM
                            5	J	9	DEPTH	35	CM
                            5	J	10	DEPTH	38	CM
                            5	J	11	DEPTH	55	CM
                            5	J	12	DEPTH	36	CM
                            5	J	13	DEPTH	51	CM
                            5	J	14	DEPTH	43	CM
                            6	A	0	DEPTH	8	CM
                            6	A	1	DEPTH	7	CM
                            6	A	2	DEPTH	23	CM
                            6	A	3	DEPTH	6	CM
                            6	A	4	DEPTH	19	CM
                            6	A	5	DEPTH	15	CM
                            6	A	6	DEPTH	9	CM
                            6	A	7	DEPTH	16	CM
                            6	A	8	DEPTH	16	CM
                            6	A	9	DEPTH	21	CM
                            6	A	10	DEPTH	7	CM
                            6	A	11	DEPTH	9	CM
                            6	A	12	DEPTH	11	CM
                            6	A	13	DEPTH	8	CM
                            6	A	14	DEPTH	10	CM
                            6	B	0	DEPTH	13	CM
                            6	B	1	DEPTH	11	CM
                            6	B	2	DEPTH	10	CM
                            6	B	3	DEPTH	13	CM
                            6	B	4	DEPTH	10	CM
                            6	B	5	DEPTH	12	CM
                            6	B	6	DEPTH	11	CM
                            6	B	7	DEPTH	10	CM
                            6	B	8	DEPTH	20	CM
                            6	B	9	DEPTH	13	CM
                            6	B	10	DEPTH	10	CM
                            6	B	11	DEPTH	12	CM
                            6	B	12	DEPTH	9	CM
                            6	B	13	DEPTH	14	CM
                            6	B	14	DEPTH	14	CM
                            6	C	0	DEPTH	10	CM
                            6	C	1	DEPTH	5	CM
                            6	C	2	DEPTH	14	CM
                            6	C	3	DEPTH	16	CM
                            6	C	4	DEPTH	19	CM
                            6	C	5	DEPTH	5	CM
                            6	C	6	DEPTH	11	CM
                            6	C	7	DEPTH	9	CM
                            6	C	8	DEPTH	15	CM
                            6	C	9	DEPTH	6	CM
                            6	C	10	DEPTH	16	CM
                            6	C	11	DEPTH	15	CM
                            6	C	12	DEPTH	6	CM
                            6	C	13	DEPTH	16	CM
                            6	C	14	DEPTH	11	CM
                            6	D	0	DEPTH	12	CM
                            6	D	1	DEPTH	13	CM
                            6	D	2	DEPTH	6	CM
                            6	D	3	DEPTH	8	CM
                            6	D	4	DEPTH	14	CM
                            6	D	5	DEPTH	23	CM
                            6	D	6	DEPTH	4	CM
                            6	D	7	DEPTH	9	CM
                            6	D	8	DEPTH	15	CM
                            6	D	9	DEPTH	15	CM
                            6	D	10	DEPTH	7	CM
                            6	D	11	DEPTH	19	CM
                            6	D	12	DEPTH	14	CM
                            6	D	13	DEPTH	10	CM
                            6	D	14	DEPTH	21	CM
                            6	E	0	DEPTH	16	CM
                            6	E	1	DEPTH	6	CM
                            6	E	2	DEPTH	4	CM
                            6	E	3	DEPTH	14	CM
                            6	E	4	DEPTH	11	CM
                            6	E	5	DEPTH	7	CM
                            6	E	6	DEPTH	7	CM
                            6	E	7	DEPTH	8	CM
                            6	E	8	DEPTH	16	CM
                            6	E	9	DEPTH	16	CM
                            6	E	10	DEPTH	6	CM
                            6	E	11	DEPTH	11	CM
                            6	E	12	DEPTH	12	CM
                            6	E	13	DEPTH	5	CM
                            6	E	14	DEPTH	25	CM
                            6	F	0	DEPTH	7	CM
                            6	F	1	DEPTH	15	CM
                            6	F	2	DEPTH	14	CM
                            6	F	3	DEPTH	9	CM
                            6	F	4	DEPTH	14	CM
                            6	F	5	DEPTH	8	CM
                            6	F	6	DEPTH	16	CM
                            6	F	7	DEPTH	8	CM
                            6	F	8	DEPTH	5	CM
                            6	F	9	DEPTH	7	CM
                            6	F	10	DEPTH	8	CM
                            6	F	11	DEPTH	7	CM
                            6	F	12	DEPTH	10	CM
                            6	F	13	DEPTH	11	CM
                            6	F	14	DEPTH	6	CM
                            6	G	0	DEPTH	12	CM
                            6	G	1	DEPTH	8	CM
                            6	G	2	DEPTH	7	CM
                            6	G	3	DEPTH	11	CM
                            6	G	4	DEPTH	7	CM
                            6	G	5	DEPTH	10	CM
                            6	G	6	DEPTH	18	CM
                            6	G	7	DEPTH	11	CM
                            6	G	8	DEPTH	15	CM
                            6	G	9	DEPTH	7	CM
                            6	G	10	DEPTH	33	CM
                            6	G	11	DEPTH	5	CM
                            6	G	12	DEPTH	15	CM
                            6	G	13	DEPTH	12	CM
                            6	G	14	DEPTH	15	CM
                            6	H	0	DEPTH	10	CM
                            6	H	1	DEPTH	5	CM
                            6	H	2	DEPTH	5	CM
                            6	H	3	DEPTH	10	CM
                            6	H	4	DEPTH	10	CM
                            6	H	5	DEPTH	20	CM
                            6	H	6	DEPTH	20	CM
                            6	H	7	DEPTH	9	CM
                            6	H	8	DEPTH	12	CM
                            6	H	9	DEPTH	10	CM
                            6	H	10	DEPTH	9	CM
                            6	H	11	DEPTH	12	CM
                            6	H	12	DEPTH	8	CM
                            6	H	13	DEPTH	6	CM
                            6	H	14	DEPTH	15	CM
                            6	I	0	DEPTH	10	CM
                            6	I	1	DEPTH	9	CM
                            6	I	2	DEPTH	6	CM
                            6	I	3	DEPTH	11	CM
                            6	I	4	DEPTH	15	CM
                            6	I	5	DEPTH	15	CM
                            6	I	6	DEPTH	10	CM
                            6	I	7	DEPTH	10	CM
                            6	I	8	DEPTH	12	CM
                            6	I	9	DEPTH	13	CM
                            6	I	10	DEPTH	9	CM
                            6	I	11	DEPTH	14	CM
                            6	I	12	DEPTH	9	CM
                            6	I	13	DEPTH	15	CM
                            6	I	14	DEPTH	6	CM
                            6	J	0	DEPTH	6	CM
                            6	J	1	DEPTH	10	CM
                            6	J	2	DEPTH	26	CM
                            6	J	3	DEPTH	9	CM
                            6	J	4	DEPTH	31	CM
                            6	J	5	DEPTH	1	CM
                            6	J	6	DEPTH	16	CM
                            6	J	7	DEPTH	14	CM
                            6	J	8	DEPTH	9	CM
                            6	J	9	DEPTH	7	CM
                            6	J	10	DEPTH	7	CM
                            6	J	11	DEPTH	9	CM
                            6	J	12	DEPTH	10	CM
                            6	J	13	DEPTH	12	CM
                            6	J	14	DEPTH	15	CM
                            7	A	1	DEPTH	1	M
                            7	A	2	DEPTH	1.4	M
                            7	A	3	DEPTH	1.4	M
                            7	A	4	DEPTH	1.5	M
                            7	A	5	DEPTH	1.2	M
                            7	A	6	DEPTH	1.4	M
                            7	A	7	DEPTH	1.4	M
                            7	A	8	DEPTH	2	M
                            7	A	9	DEPTH	1.7	M
                            7	A	10	DEPTH	1.1	M
                            7	A	11	DEPTH	1.2	M
                            7	A	12	DEPTH	1.2	M
                            7	A	13	DEPTH	1.1	M
                            7	A	14	DEPTH	0.8	M
                            7	A	15	DEPTH	0.6	M
                            7	A	16	DEPTH	0.5	M
                            7	A	17	DEPTH	0.6	M
                            7	A	18	DEPTH	0.6	M
                            7	A	19	DEPTH	0.5	M
                            7	A	20	DEPTH	0.6	M
                            7	B	1	DEPTH	0.7	M
                            7	B	2	DEPTH	0.8	M
                            7	B	3	DEPTH	1.4	M
                            7	B	4	DEPTH	1.6	M
                            7	B	5	DEPTH	1.5	M
                            7	B	6	DEPTH	3.9624	M
                            7	B	7	DEPTH	5.4864	M
                            7	B	8	DEPTH	5.4864	M
                            7	B	9	DEPTH	5.1816	M
                            7	B	10	DEPTH	2.4384	M
                            7	B	11	DEPTH	4.2672	M
                            7	B	12	DEPTH	3.048	M
                            7	B	13	DEPTH	3.048	M
                            7	B	14	DEPTH	2.4384	M
                            7	B	15	DEPTH	2.8	M
                            7	B	16	DEPTH	6.7056	M
                            7	B	17	DEPTH	4.2672	M
                            7	B	18	DEPTH	2.4384	M
                            7	B	19	DEPTH	1.6	M
                            7	B	20	DEPTH	1.2	M
                            7	C	1	DEPTH	1.4	M
                            7	C	2	DEPTH	0.9	M
                            7	C	3	DEPTH	1.2	M
                            7	C	4	DEPTH	0.7	M
                            7	C	5	DEPTH	0.9	M
                            7	C	6	DEPTH	1.3	M
                            7	C	7	DEPTH	1.1	M
                            7	C	8	DEPTH	1.1	M
                            7	C	9	DEPTH	0.9	M
                            7	C	10	DEPTH	0.8	M
                            7	C	11	DEPTH	0.5	M
                            7	C	12	DEPTH	0.9	M
                            7	C	13	DEPTH	1.1	M
                            7	C	14	DEPTH	2.1	M
                            7	C	15	DEPTH	4.8768	M
                            7	C	16	DEPTH	4.8768	M
                            7	C	17	DEPTH	4.572	M
                            7	C	18	DEPTH	3.9624	M
                            7	C	19	DEPTH	3.3528	M
                            7	C	20	DEPTH	3.9624	M
                            7	D	1	DEPTH	2.1	M
                            7	D	2	DEPTH	2.2	M
                            7	D	3	DEPTH	2.5	M
                            7	D	4	DEPTH	2.7432	M
                            7	D	5	DEPTH	4.572	M
                            7	D	6	DEPTH	1.5	M
                            7	D	7	DEPTH	1.2	M
                            7	D	8	DEPTH	0.8	M
                            7	D	9	DEPTH	0.8	M
                            7	D	10	DEPTH	1	M
                            7	D	11	DEPTH	0.9	M
                            7	D	12	DEPTH	1.1	M
                            7	D	13	DEPTH	1.1	M
                            7	D	14	DEPTH	1.3	M
                            7	D	15	DEPTH	1.3	M
                            7	D	16	DEPTH	1.4	M
                            7	D	17	DEPTH	1.4	M
                            7	D	18	DEPTH	1.3	M
                            7	D	19	DEPTH	1.2	M
                            7	D	20	DEPTH	1	M
                            7	E	1	DEPTH	0.8	M
                            7	E	2	DEPTH	1.1	M
                            7	E	3	DEPTH	1.6	M
                            7	E	4	DEPTH	1.5	M
                            7	E	5	DEPTH	2.6	M
                            7	E	6	DEPTH	1.5	M
                            7	E	7	DEPTH	1.4	M
                            7	E	8	DEPTH	1.3	M
                            7	E	9	DEPTH	1.3	M
                            7	E	10	DEPTH	1.3	M
                            7	E	11	DEPTH	1.4	M
                            7	E	12	DEPTH	1.3	M
                            7	E	13	DEPTH	1	M
                            7	E	14	DEPTH	1.2	M
                            7	E	15	DEPTH	1.4	M
                            7	E	16	DEPTH	1.3	M
                            7	E	17	DEPTH	1.2	M
                            7	E	18	DEPTH	1.1	M
                            7	E	19	DEPTH	1.1	M
                            7	E	20	DEPTH	0.9	M
                            7	F	1	DEPTH	1.1	M
                            7	F	2	DEPTH	1.2	M
                            7	F	3	DEPTH	1	M
                            7	F	4	DEPTH	0.8	M
                            7	F	5	DEPTH	1.1	M
                            7	F	6	DEPTH	0.9	M
                            7	F	7	DEPTH	2	M
                            7	F	8	DEPTH	3.048	M
                            7	F	9	DEPTH	1	M
                            7	F	10	DEPTH	1.8	M
                            7	F	11	DEPTH	3.048	M
                            7	F	12	DEPTH	2.7432	M
                            7	F	13	DEPTH	3.048	M
                            7	F	14	DEPTH	2.4384	M
                            7	F	15	DEPTH	2.7432	M
                            7	F	16	DEPTH	2.3	M
                            7	F	17	DEPTH	2	M
                            7	F	18	DEPTH	2.1	M
                            7	F	19	DEPTH	2.1	M
                            7	F	20	DEPTH	2.2	M
                            7	G	1	DEPTH	2	M
                            7	G	2	DEPTH	2.4	M
                            7	G	3	DEPTH	3.048	M
                            7	G	4	DEPTH	2.7432	M
                            7	G	5	DEPTH	2.7432	M
                            7	G	6	DEPTH	2.4384	M
                            7	G	7	DEPTH	2.4384	M
                            7	G	8	DEPTH	2.4384	M
                            7	G	9	DEPTH	1.9	M
                            7	G	10	DEPTH	1.9	M
                            7	G	11	DEPTH	1.7	M
                            7	G	12	DEPTH	1.8	M
                            7	G	13	DEPTH	1.7	M
                            7	G	14	DEPTH	1.7	M
                            7	G	15	DEPTH	1.6	M
                            7	G	16	DEPTH	1.6	M
                            7	G	17	DEPTH	1.5	M
                            7	G	18	DEPTH	1.4	M
                            7	G	19	DEPTH	1.3	M
                            7	G	20	DEPTH	1.1	M
                            7	H	1	DEPTH	0.9	M
                            7	H	2	DEPTH	0.9	M
                            7	H	3	DEPTH	0.9	M
                            7	H	4	DEPTH	0.8	M
                            7	H	5	DEPTH	0.9	M
                            7	H	6	DEPTH	0.8	M
                            7	H	7	DEPTH	0.8	M
                            7	H	8	DEPTH	0.6	M
                            7	H	9	DEPTH	0.5	M
                            7	H	10	DEPTH	0.6	M
                            7	H	11	DEPTH	1	M
                            7	H	12	DEPTH	1.1	M
                            7	H	13	DEPTH	1.3	M
                            7	H	14	DEPTH	1.3	M
                            7	H	15	DEPTH	1.2	M
                            7	H	16	DEPTH	1.4	M
                            7	H	17	DEPTH	1.4	M
                            7	H	18	DEPTH	1.7	M
                            7	H	19	DEPTH	1.5	M
                            7	H	20	DEPTH	1	M
                            7	I	1	DEPTH	0.9	M
                            7	I	2	DEPTH	0.8	M
                            7	I	3	DEPTH	0.7	M
                            7	I	4	DEPTH	0.5	M
                            7	I	5	DEPTH	0.8	M
                            7	I	6	DEPTH	1	M
                            7	I	7	DEPTH	1.3	M
                            7	I	8	DEPTH	2	M
                            7	I	9	DEPTH	3	M
                            7	I	10	DEPTH	2.4384	M
                            7	I	11	DEPTH	1.2	M
                            7	I	12	DEPTH	1.6	M
                            7	I	13	DEPTH	1.5	M
                            7	I	14	DEPTH	1.6	M
                            7	I	15	DEPTH	1.3	M
                            7	I	16	DEPTH	1.1	M
                            7	I	17	DEPTH	1.2	M
                            7	I	18	DEPTH	1.2	M
                            7	I	19	DEPTH	1	M
                            7	I	20	DEPTH	0.9	M
                            7	J	1	DEPTH	0.8	M
                            7	J	2	DEPTH	0.8	M
                            7	J	3	DEPTH	0.8	M
                            7	J	4	DEPTH	1.4	M
                            7	J	5	DEPTH	0.8	M
                            7	J	6	DEPTH	0.5	M
                            7	J	7	DEPTH	0.5	M
                            7	J	8	DEPTH	0.6	M
                            7	J	9	DEPTH	0.7	M
                            7	J	10	DEPTH	0.7	M
                            7	J	11	DEPTH	0.8	M
                            7	J	12	DEPTH	1.5	M
                            7	J	13	DEPTH	2.2	M
                            7	J	14	DEPTH	2	M
                            7	J	15	DEPTH	1.3	M
                            7	J	16	DEPTH	0.7	M
                            7	J	17	DEPTH	0.5	M
                            7	J	18	DEPTH	1	M
                            7	J	19	DEPTH	1.1	M
                            7	J	20	DEPTH	1.4	M
                            8	A	1	DEPTH	1.1	M
                            8	A	2	DEPTH	3	M
                            8	A	3	DEPTH	3.2	M
                            8	A	4	DEPTH	2.9	M
                            8	A	5	DEPTH	2.7	M
                            8	A	6	DEPTH	2.4	M
                            8	A	7	DEPTH	1.7	M
                            8	A	8	DEPTH	1.2	M
                            8	A	9	DEPTH	1	M
                            8	A	10	DEPTH	0.7	M
                            8	A	11	DEPTH	0.7	M
                            8	A	12	DEPTH	0.7	M
                            8	A	13	DEPTH	0.6	M
                            8	A	14	DEPTH	0.6	M
                            8	A	15	DEPTH	0.6	M
                            8	A	16	DEPTH	0.8	M
                            8	A	17	DEPTH	0.7	M
                            8	A	18	DEPTH	1	M
                            8	A	19	DEPTH	0.9	M
                            8	A	20	DEPTH	1.7	M
                            8	B	1	DEPTH	2.4384	M
                            8	B	2	DEPTH	2.7432	M
                            8	B	3	DEPTH	3.048	M
                            8	B	4	DEPTH	3.3528	M
                            8	B	5	DEPTH	3.048	M
                            8	B	6	DEPTH	2.7432	M
                            8	B	7	DEPTH	2.4384	M
                            8	B	8	DEPTH	1.9	M
                            8	B	9	DEPTH	1.3	M
                            8	B	10	DEPTH	0.8	M
                            8	B	11	DEPTH	0.7	M
                            8	B	12	DEPTH	0.6	M
                            8	B	13	DEPTH	0.8	M
                            8	B	14	DEPTH	2.7432	M
                            8	B	15	DEPTH	2.4384	M
                            8	B	16	DEPTH	3.3528	M
                            8	B	17	DEPTH	5.1816	M
                            8	B	18	DEPTH	4.8768	M
                            8	B	19	DEPTH	3.6576	M
                            8	B	20	DEPTH	2.1	M
                            8	C	1	DEPTH	2.4384	M
                            8	C	2	DEPTH	3.3528	M
                            8	C	3	DEPTH	3.048	M
                            8	C	4	DEPTH	2.4384	M
                            8	C	5	DEPTH	1.8288	M
                            8	C	6	DEPTH	1.2192	M
                            8	C	7	DEPTH	0.7	M
                            8	C	8	DEPTH	0.8	M
                            8	C	9	DEPTH	0.9	M
                            8	C	10	DEPTH	0.9	M
                            8	C	11	DEPTH	0.9	M
                            8	C	12	DEPTH	0.8	M
                            8	C	13	DEPTH	0.5	M
                            8	C	14	DEPTH	0.7	M
                            8	C	15	DEPTH	0.6	M
                            8	C	16	DEPTH	0.6	M
                            8	C	17	DEPTH	0.7	M
                            8	C	18	DEPTH	3.3528	M
                            8	C	19	DEPTH	3.3528	M
                            8	C	20	DEPTH	3.6576	M
                            8	D	1	DEPTH	6.7	M
                            8	D	2	DEPTH	5.2	M
                            8	D	3	DEPTH	4.2672	M
                            8	D	4	DEPTH	3.9624	M
                            8	D	5	DEPTH	1.5	M
                            8	D	6	DEPTH	1.3	M
                            8	D	7	DEPTH	1.7	M
                            8	D	8	DEPTH	2	M
                            8	D	9	DEPTH	1.8	M
                            8	D	10	DEPTH	1.9	M
                            8	D	11	DEPTH	2.1	M
                            8	D	12	DEPTH	2.3	M
                            8	D	13	DEPTH	1.8	M
                            8	D	14	DEPTH	1.8	M
                            8	D	15	DEPTH	1.7	M
                            8	D	16	DEPTH	1.1	M
                            8	D	17	DEPTH	0.9	M
                            8	D	18	DEPTH	0.8	M
                            8	D	19	DEPTH	0.6	M
                            8	D	20	DEPTH	0.2	M
                            8	E	1	DEPTH	1.5	M
                            8	E	2	DEPTH	1.3	M
                            8	E	3	DEPTH	1.9	M
                            8	E	4	DEPTH	2.1	M
                            8	E	5	DEPTH	3.6576	M
                            8	E	6	DEPTH	2.7432	M
                            8	E	7	DEPTH	2.1336	M
                            8	E	8	DEPTH	4.572	M
                            8	E	9	DEPTH	1.5	M
                            8	E	10	DEPTH	1.7	M
                            8	E	11	DEPTH	1.6	M
                            8	E	12	DEPTH	1.1	M
                            8	E	13	DEPTH	1.1	M
                            8	E	14	DEPTH	0.7	M
                            8	E	15	DEPTH	0.8	M
                            8	E	16	DEPTH	0.7	M
                            8	E	17	DEPTH	0.4	M
                            8	E	18	DEPTH	3.048	M
                            8	E	19	DEPTH	1.8288	M
                            8	E	20	DEPTH	3.9624	M
                            8	F	1	DEPTH	3.6576	M
                            8	F	2	DEPTH	4.2672	M
                            8	F	3	DEPTH	4.2672	M
                            8	F	4	DEPTH	4.8768	M
                            8	F	5	DEPTH	3.9624	M
                            8	F	6	DEPTH	1.8288	M
                            8	F	7	DEPTH	2.4384	M
                            8	F	8	DEPTH	5.7912	M
                            8	F	9	DEPTH	3.048	M
                            8	F	10	DEPTH	6.096	M
                            8	F	11	DEPTH	4.572	M
                            8	F	12	DEPTH	8.5344	M
                            8	F	13	DEPTH	12.4968	M
                            8	F	14	DEPTH	10.9728	M
                            8	F	15	DEPTH	5.1816	M
                            8	F	16	DEPTH	2.4384	M
                            8	F	17	DEPTH	2.7432	M
                            8	F	18	DEPTH	1.8	M
                            8	F	19	DEPTH	1.4	M
                            8	F	20	DEPTH	1.8	M
                            8	G	1	DEPTH	1.8	M
                            8	G	2	DEPTH	1.2	M
                            8	G	3	DEPTH	0.8	M
                            8	G	4	DEPTH	0.5	M
                            8	G	5	DEPTH	0.7	M
                            8	G	6	DEPTH	0.6	M
                            8	G	7	DEPTH	0.7	M
                            8	G	8	DEPTH	0.7	M
                            8	G	9	DEPTH	3.6576	M
                            8	G	10	DEPTH	3.048	M
                            8	G	11	DEPTH	4.572	M
                            8	G	12	DEPTH	7.0104	M
                            8	G	13	DEPTH	7.0104	M
                            8	G	14	DEPTH	5.1816	M
                            8	G	15	DEPTH	2.4384	M
                            8	G	16	DEPTH	1.7	M
                            8	G	17	DEPTH	2	M
                            8	G	18	DEPTH	2	M
                            8	G	19	DEPTH	1.8	M
                            8	G	20	DEPTH	2	M
                            8	H	1	DEPTH	1	M
                            8	H	2	DEPTH	1.3	M
                            8	H	3	DEPTH	1.8	M
                            8	H	4	DEPTH	3.3528	M
                            8	H	5	DEPTH	1.8288	M
                            8	H	6	DEPTH	1.8	M
                            8	H	7	DEPTH	1.7	M
                            8	H	8	DEPTH	2.8	M
                            8	H	9	DEPTH	3.048	M
                            8	H	10	DEPTH	2.4384	M
                            8	H	11	DEPTH	2.7432	M
                            8	H	12	DEPTH	3.3528	M
                            8	H	13	DEPTH	1.524	M
                            8	H	14	DEPTH	0.9	M
                            8	H	15	DEPTH	1.4	M
                            8	H	16	DEPTH	2.4384	M
                            8	H	17	DEPTH	2.1336	M
                            8	H	18	DEPTH	1.8	M
                            8	H	19	DEPTH	1.2	M
                            8	H	20	DEPTH	1.3	M
                            8	I	1	DEPTH	1.3	M
                            8	I	2	DEPTH	1.5	M
                            8	I	3	DEPTH	1.7	M
                            8	I	4	DEPTH	1.5	M
                            8	I	5	DEPTH	1.6	M
                            8	I	6	DEPTH	4.572	M
                            8	I	7	DEPTH	3.6576	M
                            8	I	8	DEPTH	3.3528	M
                            8	I	9	DEPTH	3.048	M
                            8	I	10	DEPTH	3.048	M
                            8	I	11	DEPTH	4.2672	M
                            8	I	12	DEPTH	5.7912	M
                            8	I	13	DEPTH	4.2672	M
                            8	I	14	DEPTH	2.7432	M
                            8	I	15	DEPTH	3.9624	M
                            8	I	16	DEPTH	3.9624	M
                            8	I	17	DEPTH	3.6576	M
                            8	I	18	DEPTH	2.4384	M
                            8	I	19	DEPTH	1.8288	M
                            8	I	20	DEPTH	0.8	M
                            8	J	1	DEPTH	1.7	M
                            8	J	2	DEPTH	1.5	M
                            8	J	3	DEPTH	1.3	M
                            8	J	4	DEPTH	1.5	M
                            8	J	5	DEPTH	0.2	M
                            8	J	6	DEPTH	0.7	M
                            8	J	7	DEPTH	0.8	M
                            8	J	8	DEPTH	1	M
                            8	J	9	DEPTH	1	M
                            8	J	10	DEPTH	2.1	M
                            8	J	11	DEPTH	2.3	M
                            8	J	12	DEPTH	2.1	M
                            8	J	13	DEPTH	3.048	M
                            8	J	14	DEPTH	3.6576	M
                            8	J	15	DEPTH	3.9624	M
                            8	J	16	DEPTH	3.048	M
                            8	J	17	DEPTH	3.6576	M
                            8	J	18	DEPTH	5.1816	M
                            8	J	19	DEPTH	2.4384	M
                            8	J	20	DEPTH	3.3528	M
                            9	A	1	DEPTH	1.2	M
                            9	A	2	DEPTH	2.3	M
                            9	A	3	DEPTH	2.7	M
                            9	A	4	DEPTH	2.6	M
                            9	A	5	DEPTH	1.2	M
                            9	A	6	DEPTH	1.6	M
                            9	A	7	DEPTH	1	M
                            9	A	8	DEPTH	0.6	M
                            9	A	9	DEPTH	0.3	M
                            9	A	10	DEPTH	0.3	M
                            9	A	11	DEPTH	0.5	M
                            9	A	12	DEPTH	0.5	M
                            9	A	13	DEPTH	0.4	M
                            9	A	14	DEPTH	0.7	M
                            9	A	15	DEPTH	0.6	M
                            9	A	16	DEPTH	0.4	M
                            9	A	17	DEPTH	0.4	M
                            9	A	18	DEPTH	0.6	M
                            9	A	19	DEPTH	0.6	M
                            9	A	20	DEPTH	0.5	M
                            9	B	1	DEPTH	1.3	M
                            9	B	2	DEPTH	1.2	M
                            9	B	3	DEPTH	2.3	M
                            9	B	4	DEPTH	2.6	M
                            9	B	5	DEPTH	2.7432	M
                            9	B	6	DEPTH	2.4	M
                            9	B	7	DEPTH	2.2	M
                            9	B	8	DEPTH	1.8	M
                            9	B	9	DEPTH	1.6	M
                            9	B	10	DEPTH	1.3	M
                            9	B	11	DEPTH	1.1	M
                            9	B	12	DEPTH	0.9	M
                            9	B	13	DEPTH	0.8	M
                            9	B	14	DEPTH	0.6	M
                            9	B	15	DEPTH	0.4	M
                            9	B	16	DEPTH	0.4	M
                            9	B	17	DEPTH	0.3	M
                            9	B	18	DEPTH	0.4	M
                            9	B	19	DEPTH	0.8	M
                            9	B	20	DEPTH	3.048	M
                            9	C	1	DEPTH	3.9624	M
                            9	C	2	DEPTH	6.096	M
                            9	C	3	DEPTH	7.3152	M
                            9	C	4	DEPTH	4.572	M
                            9	C	5	DEPTH	1	M
                            9	C	6	DEPTH	1.8	M
                            9	C	7	DEPTH	2.1	M
                            9	C	8	DEPTH	2.5	M
                            9	C	9	DEPTH	2.4384	M
                            9	C	10	DEPTH	1.8288	M
                            9	C	11	DEPTH	1.7	M
                            9	C	12	DEPTH	1.1	M
                            9	C	13	DEPTH	0.8	M
                            9	C	14	DEPTH	0.4	M
                            9	C	15	DEPTH	0.4	M
                            9	C	16	DEPTH	0.4	M
                            9	C	17	DEPTH	0.5	M
                            9	C	18	DEPTH	0.7	M
                            9	C	19	DEPTH	0.6	M
                            9	C	20	DEPTH	0.5	M
                            9	D	1	DEPTH	0.5	M
                            9	D	2	DEPTH	0.5	M
                            9	D	3	DEPTH	0.4	M
                            9	D	4	DEPTH	0.4	M
                            9	D	5	DEPTH	0.4	M
                            9	D	6	DEPTH	0.5	M
                            9	D	7	DEPTH	1.8	M
                            9	D	8	DEPTH	4.572	M
                            9	D	9	DEPTH	6.096	M
                            9	D	10	DEPTH	4.572	M
                            9	D	11	DEPTH	2.7432	M
                            9	D	12	DEPTH	4.8768	M
                            9	D	13	DEPTH	2.2	M
                            9	D	14	DEPTH	1.8	M
                            9	D	15	DEPTH	1.7	M
                            9	D	16	DEPTH	0.9	M
                            9	D	17	DEPTH	1.6	M
                            9	D	18	DEPTH	1.9	M
                            9	D	19	DEPTH	1.4	M
                            9	D	20	DEPTH	1.6	M
                            9	E	1	DEPTH	1.7	M
                            9	E	2	DEPTH	1.6	M
                            9	E	3	DEPTH	1.8	M
                            9	E	4	DEPTH	1.5	M
                            9	E	5	DEPTH	1.6	M
                            9	E	6	DEPTH	1.5	M
                            9	E	7	DEPTH	1.4	M
                            9	E	8	DEPTH	1.2	M
                            9	E	9	DEPTH	0.9	M
                            9	E	10	DEPTH	0.8	M
                            9	E	11	DEPTH	0.6	M
                            9	E	12	DEPTH	0.6	M
                            9	E	13	DEPTH	0.5	M
                            9	E	14	DEPTH	0.5	M
                            9	E	15	DEPTH	0.4	M
                            9	E	16	DEPTH	0.4	M
                            9	E	17	DEPTH	0.4	M
                            9	E	18	DEPTH	0.4	M
                            9	E	19	DEPTH	1.2	M
                            9	E	20	DEPTH	1.3	M
                            9	F	1	DEPTH	1.9	M
                            9	F	2	DEPTH	1.6	M
                            9	F	3	DEPTH	1.8	M
                            9	F	4	DEPTH	3.3528	M
                            9	F	5	DEPTH	3.3528	M
                            9	F	6	DEPTH	3	M
                            9	F	7	DEPTH	3.6576	M
                            9	F	8	DEPTH	1.3	M
                            9	F	9	DEPTH	1.3	M
                            9	F	10	DEPTH	0.8	M
                            9	F	11	DEPTH	0.7	M
                            9	F	12	DEPTH	0.9	M
                            9	F	13	DEPTH	1.2	M
                            9	F	14	DEPTH	1	M
                            9	F	15	DEPTH	0.8	M
                            9	F	16	DEPTH	1	M
                            9	F	17	DEPTH	0.6	M
                            9	F	18	DEPTH	0.5	M
                            9	F	19	DEPTH	0.5	M
                            9	F	20	DEPTH	2.1	M
                            9	G	1	DEPTH	1.5	M
                            9	G	2	DEPTH	3.048	M
                            9	G	3	DEPTH	5.4864	M
                            9	G	4	DEPTH	4.572	M
                            9	G	5	DEPTH	4.8768	M
                            9	G	6	DEPTH	7.3152	M
                            9	G	7	DEPTH	3.6576	M
                            9	G	8	DEPTH	1.2	M
                            9	G	9	DEPTH	1.8	M
                            9	G	10	DEPTH	2.1	M
                            9	G	11	DEPTH	2.4	M
                            9	G	12	DEPTH	5.7912	M
                            9	G	13	DEPTH	4.8768	M
                            9	G	14	DEPTH	4.572	M
                            9	G	15	DEPTH	5.7912	M
                            9	G	16	DEPTH	9.144	M
                            9	G	17	DEPTH	10.668	M
                            9	G	18	DEPTH	10.9728	M
                            9	G	19	DEPTH	3.3528	M
                            9	G	20	DEPTH	1.4	M
                            9	H	1	DEPTH	2.1	M
                            9	H	2	DEPTH	1.3	M
                            9	H	3	DEPTH	1.4	M
                            9	H	4	DEPTH	1.1	M
                            9	H	5	DEPTH	1.1	M
                            9	H	6	DEPTH	1.4	M
                            9	H	7	DEPTH	1.4	M
                            9	H	8	DEPTH	1.1	M
                            9	H	9	DEPTH	1.1	M
                            9	H	10	DEPTH	0.6	M
                            9	H	11	DEPTH	0.5	M
                            9	H	12	DEPTH	0.6	M
                            9	H	13	DEPTH	0.7	M
                            9	H	14	DEPTH	0.4	M
                            9	H	15	DEPTH	0.4	M
                            9	H	16	DEPTH	0.6	M
                            9	H	17	DEPTH	2.4384	M
                            9	H	18	DEPTH	2.7432	M
                            9	H	19	DEPTH	2.7432	M
                            9	H	20	DEPTH	7.62	M
                            9	I	1	DEPTH	6.4008	M
                            9	I	2	DEPTH	7.62	M
                            9	I	3	DEPTH	3.9624	M
                            9	I	4	DEPTH	1.4	M
                            9	I	5	DEPTH	0.7	M
                            9	I	6	DEPTH	0.7	M
                            9	I	7	DEPTH	1.2	M
                            9	I	8	DEPTH	1.1	M
                            9	I	9	DEPTH	0.7	M
                            9	I	10	DEPTH	0.5	M
                            9	I	11	DEPTH	0.4	M
                            9	I	12	DEPTH	1.8	M
                            9	I	13	DEPTH	3.3528	M
                            9	I	14	DEPTH	3.3528	M
                            9	I	15	DEPTH	1.1	M
                            9	I	16	DEPTH	0.9	M
                            9	I	17	DEPTH	1.4	M
                            9	I	18	DEPTH	2.4384	M
                            9	I	19	DEPTH	3.3528	M
                            9	I	20	DEPTH	2.7432	M
                            9	J	1	DEPTH	1.4	M
                            9	J	2	DEPTH	2.1	M
                            9	J	3	DEPTH	3.6576	M
                            9	J	4	DEPTH	2.4384	M
                            9	J	5	DEPTH	0.8	M
                            9	J	6	DEPTH	4	M
                            9	J	7	DEPTH	0.4	M
                            9	J	8	DEPTH	0.9	M
                            9	J	9	DEPTH	1.5	M
                            9	J	10	DEPTH	1.4	M
                            9	J	11	DEPTH	1	M
                            9	J	12	DEPTH	1	M
                            9	J	13	DEPTH	0.7	M
                            9	J	14	DEPTH	0.9	M
                            9	J	15	DEPTH	1.5	M
                            9	J	16	DEPTH	0.5	M
                            9	J	17	DEPTH	1	M
                            9	J	18	DEPTH	0.9	M
                            9	J	19	DEPTH	1	M
                            9	J	20	DEPTH	1.1	M
                            1	A	0	WETWID	6.4	M
                            1	A	5	WETWID	10.5	M
                            1	B	5	WETWID	10	M
                            1	B	0	WETWID	10.5	M
                            1	C	5	WETWID	8.1	M
                            1	C	0	WETWID	8.6	M
                            1	D	5	WETWID	6.8	M
                            1	D	0	WETWID	7.1	M
                            1	E	5	WETWID	4.8	M
                            1	E	0	WETWID	5.1	M
                            1	F	5	WETWID	5	M
                            1	F	0	WETWID	7.5	M
                            1	G	0	WETWID	4.6	M
                            1	G	5	WETWID	7.3	M
                            1	H	0	WETWID	5.7	M
                            1	H	5	WETWID	8.3	M
                            1	I	0	WETWID	3.7	M
                            1	I	5	WETWID	4.4	M
                            1	J	0	WETWID	5.8	M
                            1	J	5	WETWID	5.9	M
                            2	A	5	WETWID	8.4	M
                            2	A	0	WETWID	10.8	M
                            2	B	5	WETWID	6.6	M
                            2	B	0	WETWID	9.9	M
                            2	C	5	WETWID	7	M
                            2	C	0	WETWID	7.3	M
                            2	D	5	WETWID	5.3	M
                            2	D	0	WETWID	7	M
                            2	E	0	WETWID	5.6	M
                            2	E	5	WETWID	6.4	M
                            2	F	5	WETWID	5	M
                            2	F	0	WETWID	6	M
                            2	G	0	WETWID	5.1	M
                            2	G	5	WETWID	6	M
                            2	H	0	WETWID	5.5	M
                            2	H	5	WETWID	7.2	M
                            2	I	0	WETWID	3.5	M
                            2	I	5	WETWID	6.7	M
                            2	J	0	WETWID	5.8	M
                            2	J	5	WETWID	5.8	M
                            3	A	5	WETWID	6.5	M
                            3	A	0	WETWID	14	M
                            3	B	5	WETWID	3.6	M
                            3	B	0	WETWID	5.7	M
                            3	C	0	WETWID	4.1	M
                            3	C	5	WETWID	4.4	M
                            3	D	5	WETWID	4.4	M
                            3	D	0	WETWID	7.3	M
                            3	E	5	WETWID	4.5	M
                            3	E	0	WETWID	4.7	M
                            3	F	0	WETWID	5.5	M
                            3	F	5	WETWID	5.8	M
                            3	G	5	WETWID	4.3	M
                            3	G	0	WETWID	13.2	M
                            3	H	0	WETWID	3.7	M
                            3	H	5	WETWID	4.3	M
                            3	I	0	WETWID	4.3	M
                            3	I	5	WETWID	5.2	M
                            3	J	0	WETWID	7.5	M
                            3	J	5	WETWID	9.3	M
                            4	A	5	WETWID	5.8	M
                            4	A	0	WETWID	13.5	M
                            4	B	5	WETWID	3	M
                            4	B	0	WETWID	4.5	M
                            4	C	0	WETWID	3.7	M
                            4	C	5	WETWID	4.7	M
                            4	D	5	WETWID	4	M
                            4	D	0	WETWID	5.3	M
                            4	E	5	WETWID	3.7	M
                            4	E	0	WETWID	4.4	M
                            4	F	5	WETWID	5.1	M
                            4	F	0	WETWID	5.6	M
                            4	G	5	WETWID	4.2	M
                            4	G	0	WETWID	7.4	M
                            4	H	0	WETWID	3.1	M
                            4	H	5	WETWID	3.8	M
                            4	I	0	WETWID	3.9	M
                            4	I	5	WETWID	5.2	M
                            4	J	0	WETWID	6.8	M
                            4	J	5	WETWID	8.7	M
                            5	A	7	WETWID	1.3	M
                            5	A	0	WETWID	1.5	M
                            5	B	7	WETWID	1.4	M
                            5	B	0	WETWID	1.5	M
                            5	C	7	WETWID	1.9	M
                            5	C	0	WETWID	2	M
                            5	D	7	WETWID	1.5	M
                            5	D	0	WETWID	1.8	M
                            5	E	7	WETWID	1.8	M
                            5	E	0	WETWID	2.3	M
                            5	F	0	WETWID	1.7	M
                            5	F	7	WETWID	1.7	M
                            5	G	7	WETWID	1.5	M
                            5	G	0	WETWID	1.8	M
                            5	H	0	WETWID	1.6	M
                            5	H	7	WETWID	2.4	M
                            5	I	0	WETWID	1.9	M
                            5	I	7	WETWID	1.9	M
                            5	J	0	WETWID	2.4	M
                            5	J	7	WETWID	4.5	M
                            6	A	0	WETWID	1	M
                            6	A	7	WETWID	3.1	M
                            6	B	7	WETWID	1.7	M
                            6	B	0	WETWID	2.1	M
                            6	C	0	WETWID	1.2	M
                            6	C	7	WETWID	1.3	M
                            6	D	7	WETWID	1	M
                            6	D	0	WETWID	1.3	M
                            6	E	7	WETWID	0.9	M
                            6	E	0	WETWID	2	M
                            6	F	7	WETWID	1.2	M
                            6	F	0	WETWID	1.8	M
                            6	G	7	WETWID	1.5	M
                            6	G	0	WETWID	2	M
                            6	H	7	WETWID	0.7	M
                            6	H	0	WETWID	1.6	M
                            6	I	0	WETWID	0.6	M
                            6	I	7	WETWID	0.8	M
                            6	J	7	WETWID	0.8	M
                            6	J	0	WETWID	4.6	M"
                                                        )
  df2 <- read.table(bob2, header=TRUE,stringsAsFactors=FALSE)
  close (bob2)

  df2$TRANSECT <- as.character(df2$TRANSECT)
  df2$STATION <- ifelse(df2$UID %in% 7:9, df2$STATION - 1, df2$STATION)
  df2$PARAMETER <- as.character(df2$PARAMETER)
  df2$RESULT <- as.character(df2$RESULT)
  df2$SAMPLE_TYPE <- ifelse(df2$UID %in% 1:6, 'PHAB_CHANW', 'PHAB_CHANBFRONT')
  df2$FLAG <- NA

  return(df2)
}


metsChannelMorphology.createProtocolData <- function()
#
{
      bob4 <- textConnection("UID  PROTOCOL
                              1  WADEABLE
                              2  WADEABLE
                              3  WADEABLE
                              4  WADEABLE
                              5  WADEABLE
                              6  WADEABLE
                              7  BOATABLE
                              8  BOATABLE
                              9  BOATABLE"
                              )

  protocols <- read.table(bob4, header=TRUE,stringsAsFactors=FALSE)
  close (bob4)

  protocols$PROTOCOL <- as.character(protocols$PROTOCOL)

  return(protocols)
}


metsChannelMorphology.createExpectedResults <- function()
#
{
     bob3 <- textConnection(" UID  METRIC  RESULT
                              1  xbkf_w  12.01818182
                              2  xbkf_w  9.5
                              3  xbkf_w  8.845454546
                              4  xbkf_w  8.872727273
                              5  xbkf_w  1.736363636
                              6  xbkf_w  2.272727273
                              7  xbkf_w  78.1
                              8  xbkf_w  150.4545455
                              9  xbkf_w  162.5454546
                              1  sdbkf_w  5.726747451
                              2  sdbkf_w  2.326370564
                              3  sdbkf_w  3.22842489
                              4  sdbkf_w  3.226171387
                              5  sdbkf_w  0.257964057
                              6  sdbkf_w  1.042200469
                              7  sdbkf_w  28.17977999
                              8  sdbkf_w  53.39918283
                              9  sdbkf_w  72.67649364
                              1  n_bw  11
                              2  n_bw  11
                              3  n_bw  11
                              4  n_bw  11
                              5  n_bw  11
                              6  n_bw  11
                              7  n_bw  10
                              8  n_bw  11
                              9  n_bw  11
                              1  n_bh  11
                              2  n_bh  11
                              3  n_bh  11
                              4  n_bh  11
                              5  n_bh  11
                              6  n_bh  11
                              7  n_bh  11
                              8  n_bh  11
                              9  n_bh  11
                              1  xbkf_h  0.554545455
                              2  xbkf_h  0.390909091
                              3  xbkf_h  0.248181818
                              4  xbkf_h  0.368181818
                              5  xbkf_h  0.063636364
                              6  xbkf_h  0.063636364
                              7  xbkf_h  1.936363636
                              8  xbkf_h  3.345454546
                              9  xbkf_h  3.563636364
                              1  sdbkf_h  0.163484778
                              2  sdbkf_h  0.104446594
                              3  sdbkf_h  0.048335946
                              4  sdbkf_h  0.056003247
                              5  sdbkf_h  0.050452498
                              6  sdbkf_h  0.050452498
                              7  sdbkf_h  0.377551923
                              8  sdbkf_h  0.508652409
                              9  sdbkf_h  0.450050502
                              1  n_incis  11
                              2  n_incis  11
                              3  n_incis  11
                              4  n_incis  11
                              5  n_incis  11
                              6  n_incis  11
                              7  n_incis  11
                              8  n_incis  11
                              9  n_incis  11
                              1  xinc_h  1.736363636
                              2  xinc_h  1.436363636
                              3  xinc_h  0.781818182
                              4  xinc_h  0.886363636
                              5  xinc_h  0.8
                              6  xinc_h  0.418181818
                              7  xinc_h  5.754545455
                              8  xinc_h  11.09090909
                              9  xinc_h  9.272727273
                              1  sdinc_h  0.743334013
                              2  sdinc_h  0.441073072
                              3  sdinc_h  0.293489972
                              4  sdinc_h  0.251088539
                              5  sdinc_h  0.279284801
                              6  sdinc_h  0.213626692
                              7  sdinc_h  1.327677398
                              8  sdinc_h  3.448319749
                              9  sdinc_h  4.734784242
                              1  xdepth  0.36060000
                              2  xdepth  0.35910000
                              3  xdepth  0.5714018692
                              4  xdepth  0.4026363636
                              5  xdepth  0.3390666667
                              6  xdepth  0.1155333333
                              7  xdepth  1.626216
                              8  xdepth  2.391104
                              9  xdepth  1.89608
                              1  n_d  100
                              2  n_d  100
                              3  n_d  107
                              4  n_d  110
                              5  n_d  150
                              6  n_d  150
                              7  n_d  200
                              8  n_d  200
                              9  n_d  200
                              1  sddepth  0.2702218318
                              2  sddepth  0.2620628924
                              3  sddepth  0.2431726175
                              4  sddepth  0.1672918522
                              5  sddepth  0.08595571411
                              6  sddepth  0.05124669466
                              7  sddepth  1.069558828
                              8  sddepth  1.765916398
                              9  sddepth  1.870213771
                              1  n_w  20
                              2  n_w  20
                              3  n_w  20
                              4  n_w  20
                              5  n_w  20
                              6  n_w  20
                              7  n_w  11
                              8  n_w  11
                              9  n_w  11
                              1  xwidth  6.805
                              2  xwidth  6.545
                              3  xwidth  6.115
                              4  xwidth  5.32
                              5  xwidth  1.92
                              6  xwidth  1.56
                              7  xwidth  66.54545455
                              8  xwidth  60.54545455
                              9  xwidth  55.09090909
                              1  sdwidth  2.041020127
                              2  sdwidth  1.674098343
                              3  sdwidth  2.938541529
                              4  sdwidth  2.406263756
                              5  sdwidth  0.683335473
                              6  sdwidth  0.936061177
                              7  sdwidth  28.5564831
                              8  sdwidth  30.56914666
                              9  sdwidth  25.5439016
                              1  n_wd  20
                              2  n_wd  20
                              3  n_wd  20
                              4  n_wd  20
                              5  n_wd  20
                              6  n_wd  20
                              7  n_wd  10
                              8  n_wd  10
                              9  n_wd  10
                              1  n_wdr  20
                              2  n_wdr  20
                              3  n_wdr  20
                              4  n_wdr  20
                              5  n_wdr  20
                              6  n_wdr  20
                              7  n_wdr  10
                              8  n_wdr  10
                              9  n_wdr  10
                              1  n_bfrat  10
                              2  n_bfrat  10
                              3  n_bfrat  10
                              4  n_bfrat  10
                              5  n_bfrat  10
                              6  n_bfrat  10
                              7  n_bfrat  9
                              8  n_bfrat  10
                              9  n_bfrat  10
                              1  xwxd  2.66325
                              2  xwxd  2.3595
                              3  xwxd  3.1076
                              4  xwxd  2.0826
                              5  xwxd  0.6833
                              6  xwxd  0.1636
                              7  xwxd  79.13
                              8  xwxd  117.64136
                              9  xwxd  111.2844
                              1  xwd_rat  24.80588726
                              2  xwd_rat  24.90067238
                              3  xwd_rat  13.12054455
                              4  xwd_rat  16.56728055
                              5  xwd_rat  5.693622175
                              6  xwd_rat  16.41718906
                              7  xwd_rat  69.17896825
                              8  xwd_rat  37.6305331
                              9  xwd_rat  44.33470982
                              1  bfwd_rat  13.84535526
                              2  bfwd_rat  14.81547289
                              3  bfwd_rat  12.07508224
                              4  bfwd_rat  12.79515661
                              5  bfwd_rat  4.53685
                              6  bfwd_rat  19.06095
                              7  bfwd_rat  27.30324642
                              8  bfwd_rat  28.7209445
                              9  bfwd_rat  26.53093894
                              1  sdwxd  2.007056364
                              2  sdwxd  1.80578525
                              3  sdwxd  1.091975246
                              4  sdwxd  0.861682039
                              5  sdwxd  0.373720005
                              6  sdwxd  0.1092705
                              7  sdwxd  33.76253841
                              8  sdwxd  48.99152319
                              9  sdwxd  77.27505946
                              1  sdwd_rat  13.69300458
                              2  sdwd_rat  11.68925373
                              3  sdwd_rat  9.779058121
                              4  sdwd_rat  14.96103854
                              5  sdwd_rat  1.805103113
                              6  sdwd_rat  14.94596436
                              7  sdwd_rat  37.82070402
                              8  sdwd_rat  31.91723431
                              9  sdwd_rat  46.13666453"
                           )
  metstest <-  read.table(bob3, header=TRUE,stringsAsFactors=FALSE)
  close (bob3)

  metstest$METRIC <- as.character(metstest$METRIC)
  metstest$RESULT <- as.character(metstest$RESULT)

  return(metstest)

}
# end of file