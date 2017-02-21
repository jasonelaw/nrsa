context("Fish IBI")
library(plyr)
kMetrics <- c('n.native.species', 'n.native.families', 'n.native.benthic.species',
              'n.native.watercolumn', 'n.sensitive', 'n.nlns', 'percent.tolerant', 'n.hider',
              'percent.filter.feeding', 'percent.top.carnivore', 'percent.omnivorous',
              'percent.lunker', 'percent.anomaly')

d <- ldply(1:5,  function(x){ 
  d <- data.frame(site = x,
             species = sample(nrsa:::fishTraits()$species, size = (trunc(runif(1, 5, 20)))))
  d$length = trunc(runif(nrow(d), 5, 100))
  d
})

zero.ans <- data.frame(n.native.species = 0, n.native.families = 0, n.native.benthic.species = 0,
                       n.native.watercolumn = 0, n.sensitive = 0, n.nlns = 0, percent.tolerant = 10, n.hider = 0,
                       percent.filter.feeding = 0, percent.top.carnivore = 0, percent.omnivorous = 10,
                       percent.lunker = 0, percent.anomaly = 10)

raw.scale.range <- data.frame(n.native.species = 10, n.native.families = 10, n.native.benthic.species = 10,
                       n.native.watercolumn = 0, n.sensitive = 0, n.nlns = 0, percent.tolerant = 10, n.hider = 0,
                       percent.filter.feeding = 0, percent.top.carnivore = 0, percent.omnivorous = 10,
                       percent.lunker = 0, percent.anomaly = 10)

test_that("Subindex scaling functions correct", {
  zero <- as.data.frame(as.list(structure(rep(0,length(kMetrics)), .Names = kMetrics)))
  zero$order <- 1
  expect_that(nrsa:::rescaleMetrics(zero)[,kMetrics], equals(zero.ans))
  zero$order <- 2
  expect_that(nrsa:::rescaleMetrics(zero)[,kMetrics], equals(zero.ans))
})

calculateFishIBI(d$site, 1L, d$species, d$length, F, join.traits.by = 'species')
