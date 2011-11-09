metsBedStabilityTest <- function()
# Unit tests metsBedStability()
# Expected values for UID 5+ taken from WEMAP calculations unchanged except
# for the following:
#   At UID 11, s_ldmb_bw5 changed from NA to -0.080588008, s_lrbs_bw5 changed
#     from NA to 1.2704980082 and s_rp100 changed from NA to 441.9807675
#     as EMAP does not calculate estimated bed stability for rivers.


{
  protocols <- metsBedStability.protocols ()
  metsExpected <- metsBedStability.expectedMets ()

  intermediateMessage('.2.0 Test with both protocols', loc='end')
  testData <- metsBedStability.testData ()
  metsBedStabilityTest.process (testData, metsExpected, protocols)
  
  intermediateMessage ('.2.1 Test with wadeable protocol', loc='end')
  test.w <- subset(testData, UID %in% subset (protocols, PROTOCOL=='WADEABLE')$UID)
  expected.w <- subset (metsExpected, UID %in% subset (protocols, PROTOCOL=='WADEABLE')$UID)
  metsBedStabilityTest.process (test.w, expected.w, protocols)
  
  intermediateMessage ('.2.2 Test with boatable protocol', loc='end')
  test.b <- subset(testData, UID %in% subset (protocols, PROTOCOL=='BOATABLE')$UID)
  expected.b <- subset (metsExpected, UID %in% subset (protocols, PROTOCOL=='BOATABLE')$UID)
  metsBedStabilityTest.process (test.b, expected.b, protocols)
 } 

metsBedStabilityTest.process <- function (testData, metsExpected, protocols)   
#
{
  rr <- metsBedStability.1(testData, protocols)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.  The calculation of s_rp100 is only
  # accurate to 10E-4 due to the exponential calculations.  The checks of
  # calculation results are split along this line.
  errs <- dfCompare(subset(metsExpected, substr(METRIC,1,2) != 's_')
                   ,subset(rr, substr(METRIC,1,2) != 's_')
                   ,c('UID','METRIC'), zeroFudge=10^-7
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability metrics are broken"
             )

  errs <- dfCompare(subset(metsExpected, substr(METRIC,1,2) == 's_')
                   ,subset(rr, substr(METRIC,1,2) == 's_')
                   ,c('UID','METRIC'), zeroFudge=10^-4
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability s_* metric is broken"
             )

}

metsBedStability.protocols <- function ()
# create dataframe of protocolas for bed stability unit tests
 {
  protocols <- data.frame(UID=as.character(1:22)
                        ,PROTOCOL=c(rep('WADEABLE',10)
                                   ,rep('BOATABLE', 10)
                                   ,rep('NONE', 2)
                                   )
                        ,stringsAsFactors=FALSE
                        )
   return (protocols)                     
   }
   
metsBedStability.testData <- function()
# creates dataframe of bed stability data for unit test
{
  testData <- rbind(# Very simple numbers to simply test functionality
                # Simple reach, no missing values
                data.frame(UID=rep('1',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,         1,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and nonzero fishcover
               ,data.frame(UID=rep('2',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and zero fishcover
               ,data.frame(UID=rep('3',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          0,     3.95494014
                                   )
                          )
               # Simple reach nonmissing LWD and missing fishcover
               ,data.frame(UID=rep('4',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,       0.3,         NA,     3.95494014
                                   )
                          )

                # Real numbers from WEMAP to test accuracy of calculations
                # 2004 WUTP99-0735  2 -- normal wadeable reach
               ,data.frame(UID=rep('5',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(51.16,  0.75545,	21.818,	 10.0105
                                   ,1.22,  0.5408,    0.5408, 19.0993
                                   ,22.08840009, 0.00004,   0,      27.7557
                                   )
                          )
                          
               # 2004 WWAP04-R048  1-- big substrate
               ,data.frame(UID=rep('6',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(71.19, 0.61818, 6.673, 5.255
                                   ,9.75, 3.204, 3.19253, 28.3768
                                   ,25.80377156, 0.01461, 0.01364, 44.2636
                                   )
                          )

               # 2003 WWAP99-0542  1 -- Lots of LWD
               ,data.frame(UID=rep('7',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(24.007, 0.3, 2.927, 2.445
                                   ,8.05, 0.75694, 0.7244, 6.4191
                                   ,4.41366392, 0.37977, 0.11818, 10.4878
                                   )
                          )

               #  2004 WDEQ-0003       1 - has zero xslope
               ,data.frame(UID=rep('8',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(31.033,	0.4,	3.4818,	2.385
                                   ,0, 0.37664, 0.37664, 27.2566
                                   , 46.12060600, 0.00015, 0, 22.6112
                                   )
                          )

               # 2004 WSDP04-R050  1 -- missing v1w_msq (really zero) and xfc_lwd==0
               ,data.frame(UID=rep('9',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(66.51, 0, 8.745, 9.1474
                                   ,1.57, 0.31911, 0.29885, 22.2393
                                   ,21.92365464,  NA,		0, 28.7622
                                   )
                          )
               # 2004 WCOP04-R004  1-- missing v1w_msq (really zero) with non-zero xfc_lwd
               ,data.frame(UID=rep('10',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(42.67, 0.33636, 3.718, 3.085
                                   , 0.775, -1.31451, -1.31451, 10.3489
                                   ,5.80084933, NA, 0.00455, 8.8661
                                   )
                          )
               # End of values from wadeable reaches

               # 2004   WCAP99-1103        1 -- normal boatable reach
               ,data.frame(UID=rep('11',12)
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(3.29555, 1.47273, 255.273, 163.364
                                   ,0.036, 1.18991, NA, 112.73
                                   ,NA, 0.005818, 0.00455, 1.69386
                                   )
                          )
               #
#               ,data.frame(UID=rep('12',12)
#                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
#                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
#                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
#                                   )
#                          ,RESULT=c(1,         1,          1,          1
#                                   ,1,         1,          2,          1
#                                   ,2,       0.3,         NA,     3.95494014
#                                   )
#                          )

               )
  testData$UID <- as.character(testData$UID)
  testData$METRIC <- as.character(testData$METRIC)
  testData <- subset(testData, METRIC != 's_rp100')
return(testData)
}


metsBedStability.expectedMets <- function()
# creates dataframe of bank morphology metrics calculation results for unit test

{
  metsExpected <- rbind(data.frame(UID=rep('1', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 0.835690571
                                       ,0.831325766, -0.164309429, 1.164309429
                                       ,0.164309429, 0.168674234, 1.164309429
                                       ,1.168674234, 113.9277151, 2.056629387
                                       ,-1.056629387, 2
                                       )
                              )
                   ,data.frame(UID=rep('2', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(-0.164309429,	1.164309429, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 2
                                       )
                              )
                   ,data.frame(UID=rep('3', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 1.835690571
                                       ,1.831325766, 1.835690571, -0.835690571
                                       ,-0.835690571, -0.831325766, 0.164309429
                                       ,0.168674234, NA, NA
                                       ,NA, 2
                                       )
                              )
                   ,data.frame(UID=rep('4', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 1.437750563
                                       ,1.426755179, 0.437750563, 0.562249437
                                       ,-0.437750563, -0.426755179, 0.562249437
                                       ,0.573244821, 146.4599268, 2.165718813
                                       ,-1.165718813, 2
                                       )
                              )
                   ,data.frame(UID=rep('5', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(1.630980938, -1.090180938, 1.95385339
                                       ,1.941619453, 1.953783007, -1.412983007
                                       ,-1.41305339, -1.400819453, -1.41305339
                                       ,-1.400819453, 105.9303106, 2.025020246
                                       ,-1.484220246, 22.08840009
                                       )
                              )
                   ,data.frame(UID=rep('6', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(2.67711418, 0.52688582, 2.832055426
                                       ,2.842904539, 2.815915646, 0.388084354
                                       ,0.371944574, 0.361095461, 0.360474574
                                       ,0.349625461, 1588.155449, 3.200893009
                                       ,0.003106991, 25.80377156
                                       )
                              )
                   ,data.frame(UID=rep('7', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(2.121824344, -1.364884344, 1.418982992
                                       ,1.436906832, 0.418982992, 0.337957008
                                       ,-0.662042992, -0.679966832, -0.694582992
                                       ,-0.712506832, 148.8617733, 2.172783188
                                       ,-1.415843188, 4.41366392
                                       )
                              )
                   ,data.frame(UID=rep('8', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(-0.672485667, 1.049125667, -0.523367109
                                       ,-0.768424143, -0.523802116, 0.900442116
                                       ,0.900007109, 1.145064143, 0.900007109
                                       , 1.145064143, 0.32636017, -0.486302849
                                       ,0.862942849, 46.12060600
                                       )
                              )
                   ,data.frame(UID=rep('9', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(1.854477172, -1.535367172, 1.677706613
                                       ,1.680788736, 1.677706613, -1.358596613
                                       ,-1.358596613, -1.361678736, -1.378856613
                                       ,-1.381938736, NA, NA
                                       ,NA, 21.92365464
                                       )
                              )
                   ,data.frame(UID=rep('10', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(1.355114917, -2.669624917, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 5.80084933
                                       )
                              )
                   ,data.frame(UID=rep('11', 14)
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100'
                                       )
                              ,RESULT=c(0.909920977, 0.279989023, 0.951821206
                                       ,-0.080588008, 0.949639991, 0.240270009
                                       ,0.238088794, 1.2704980082, NA
                                       ,NA, 17.97017283, 1.254552254
                                       ,-0.064642254, 441.9807675
                                       )
                              )
                   )
  metsExpected$UID <- as.character(metsExpected$UID)
  metsExpected$METRIC <- as.character(metsExpected$METRIC)

return(metsExpected)
}

# end of file