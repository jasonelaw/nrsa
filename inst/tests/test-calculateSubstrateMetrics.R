context("Substrate")

test_that("Boatable littoral substrate metrics work correctly", {
  
  expected.metrics <- sort(c("pct_dbbl", "pct_dbcb", "pct_dbfn", "pct_dbgc", "pct_dbgf", 
                             "pct_dbhp", "pct_dbom", "pct_dbot", "pct_dbrc", "pct_dbrr", "pct_dbrs", 
                             "pct_dbsa", "pct_dbsb", "pct_dbwd", "pct_dbxb", "pct_dsbl", "pct_dscb", 
                             "pct_dsfn", "pct_dsgc", "pct_dsgf", "pct_dshp", "pct_dsom", "pct_dsot", 
                             "pct_dsrc", "pct_dsrr", "pct_dsrs", "pct_dssa", "pct_dssb", "pct_dswd", 
                             "pct_dsxb", "pct_sbbl", "pct_sbcb", "pct_sbfn", "pct_sbgc", "pct_sbgf", 
                             "pct_sbhp", "pct_sbom", "pct_sbot", "pct_sbrc", "pct_sbrr", "pct_sbrs", 
                             "pct_sbsa", "pct_sbsb", "pct_sbwd", "pct_sbxb", "pct_ssbl", "pct_sscb", 
                             "pct_ssfn", "pct_ssgc", "pct_ssgf", "pct_sshp", "pct_ssom", "pct_ssot", 
                             "pct_ssrc", "pct_ssrr", "pct_ssrs", "pct_sssa", "pct_sssb", "pct_sswd", 
                             "pct_ssxb"))
  uid <- rep(1, 15)
  classes <- c('RS', 'RR', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA',
               'FN', 'HP', 'WD', 'OT', 'BL', 'OM', 'RC')
  ans <- calculateBoatLittoralSubstrateMetrics(uid, classes, classes, classes, classes)
  
  expect_that(sort(ans$metric), is_identical_to(expected.metrics))
  expect_that(ans$result, equals(rep(1/15, 60)))
  
})

test_that("Boatable thalweg substrate metrics work correctly", {
  
  # Could use more tests
  expected.metrics <- c("lsub_d16", "lsub_d25", "lsub_d50", "lsub_d75", "lsub_d84", 
                        "lsub_dmm", "lsub_iqr", "lsubd_sd", "n", "pct_bh", "pct_bl", 
                        "pct_cb", "pct_fn", "pct_gr", "pct_ot", "pct_sa", "pct_safn")
  uid <- rep(1,7)
  classes <- c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT')
  ans <- calculateBoatThalwegSubstrateMetrics(uid, classes)
  
  expect_that(sort(ans$metric), is_identical_to(expected.metrics))
  
  expect_that(ans$result[grepl('pct', ans$metric)], equals(100 * c(rep(1/7, 7), 2/7)))
  
})


