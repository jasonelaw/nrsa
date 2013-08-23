\name{addWadeSubstrateSizes}
\alias{addBoatSubstrateSizes}
\alias{addWadeSubstrateSizes}
\title{Add substrate size variables to wadeable data}
\usage{
  addWadeSubstrateSizes(uid, size.class)

  addBoatSubstrateSizes(uid, size.class)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{size.class}{a vector size class codes}
}
\description{
  The substrate size metrics are calculated on variables
  representing four different sets of the size class data.
  This function convert the size classes to a diameter and
  logDiameter for each of those size classes (geometric
  mean of the extreme sizes) for each of these sets. There
  are three data sets we need to work with (1) ALL the
  size_classes (mm) (2) subclasses excluding HP, RD, RR,
  RS, RC, OT, WD (tt) (3) subclasse INCLUDES all classes
  and lumps the boulder class (XB+SB= BL) (bl) (4)
  subclasses excluding HP, RD, RR, RS, RC, OT, WD, and
  lumps the boulder class (XB+SB= BL) (ttbl)
}
\examples{
d <- expand.grid(uid = 1:2,
size.class = c("OM", "OT", "WD", "HP", "FN", "SA", "GF", "GC","CB", "SB", "XB", "BL", "RS", "RR", "RC"))
addWadeSubstrateSizes(d$uid, d$size.class)
d <- expand.grid(uid = 1:2,
size.class =c('BH', 'BL' ,'CB',  'GR' ,'SA',  'FN' ,'OT'))
addBoatSubstrateSizes(d$uid, d$size.class)
}
