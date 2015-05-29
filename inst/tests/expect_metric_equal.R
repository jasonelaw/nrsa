expect_metric_equal <- function(metrics, expected, metric){
  uids <- unique(c(metrics$uid, expected$uid))
  expected <- expected[match(uids, expected$uid),]
  metrics  <- metrics[match(uids, metrics$uid),]
  if (is.numeric(metrics[,metric])){
    eval(bquote(expect_equal(metrics[,.(metric)], expected[,.(metric)])))
  } else {
    eval(bquote(expect_identical(metrics[,.(metric)], expected[,.(metric)])))
  }
}
