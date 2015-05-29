#TODO: Deal with Temperature spawning versus nonspawning.
library(functional)
library(polynom)

# Constants
kWatersheds <- c('Fanno Creek', 'Johnson Creek', 'Tualatin Tributaries', 'Tryon Creek', 
                 'Willamette River Triburaries', 'Columbia River', 'Columbia Slough', 'Willamette River')

  # Polynomial coefficients: from lower to higher order, left to right
kDORiver           <- c( -41.343,   29.802,    -2.2154,    0.0596)
kDOStream          <- c( -58.232,   27.31,     -1.6464,    0.0392)
kCopper            <- c( 102.17,   -25.887,     2.6551,   -0.1171)
kTSS               <- c( 104.04,    -2.7238,    0.0278,   -0.0001)
kPhosphorusFanno   <- c( 103.89,  -399.76,    527.6,    -240.3)
kPhosphorus        <- c( 102.79,  -331.24,    395.32,   -167.67)
kTemperature       <- c(  29.668,   18.751,    -1.3944,    0.0248)
kTemperatureRiver  <- c(-101.87,    38.843,    -2.2667,    0.0365)
kMercury           <- c(  98.426, -23.898,   2.5939, -0.114)
kMercuryWillamette <- c( 100.41,  -54.278,  12.318,  -1.0076)
kEcoli             <- c( 101.29,   -0.1314,  8e-05,  -2.1e-08)
kAmmoniaRatio      <- c( 100,     -40)
kDOsat             <- c(0.55, 0.0095, -5e-05)

  # Bounds on some values
kIndexRange <- c(10, 100)
kDOSatRange <- c(0.45, 1)

# Functions
clamp <- function(x, a, b) pmax(a, pmin(x, b))

iclamp <- Curry(clamp, a = kIndexRange[1], b = kIndexRange[2])

within <- function(x, r) (x > r[1]) & (x < r[2])

#' Function to deal with annoying discontinuities in index; goal is
#' to get index function as close to continous as possible. Solve
#' for roots when p(x) = 10 and p(x) = 100. When there are 3 real root 
#' choose correct real root (using deriv(p(x)) = 0 to bracket).
#' Use roots (correct 10 and correct 100) to clamp input to poly, use (10,100) to clamp output of poly.
#' Poly either has one real root so the real Filter gets it or has three real
#' roots and need to bracket using the derivative = 0.
#' See http://en.wikipedia.org/wiki/Cubic_function#The_nature_of_the_roots
#'
createIndexFun <- function(x){
  p <- polynomial(x)
  s10 <- solve(p, 10)
  s100 <- solve(p, 100)
  s10 <- Re(Filter(function(x) Im(x) == 0, s10))
  s100 <- Re(Filter(function(x) Im(x) == 0, s100))
  if (length(s10) > 1) {
    s10 <- Filter(function(x) within(x, solve(deriv(p))), solve(p, 10))
  }
  if (length(s100) > 1){
    s100 <- Filter(function(x) within(x, solve(deriv(p))), solve(p, 100))
  }
  if (s10 < s100){
    f <- Compose(Curry(clamp, a = s10, b = s100), as.function(p), iclamp)
  } else {
    f <- Compose(Curry(clamp, a = s100, b = s10), as.function(p), iclamp)
  }
  f
}

adjustDO <- function(index, DO, Cel){
  do.sol <- calculateOxygenSolubility(Cel)
  do.sat <- DO / do.sol * 100
  do.adjust <- Compose(as.function(polynomial(kDOsat)), Curry(clamp, a = kDOSatRange[1], b = kDOSatRange[2]))
  ret <- ifelse(do.sat > 100, index * do.adjust(do.sat), index)
  ret
}

calculateAmmoniaRatio <- function(NH4, pH, Cel){
  t2 <- (0.0278 / (1 + 10^(7.688 - pH))) + (1.1994 / (1 + 10^(pH - 7.688)))
  t3 <- 2.126 * 10^(0.028 * (20 - pmax(7, Cel)))
  ccc <- 0.8876 * (t2) * (t3)
  NH4 / ccc
}

# From http://water.usgs.gov/admin/memo/QW/qw11.03.pdf
calculateOxygenSolubility <- function(Cel){
  K <- Cel + 273.15
  exp(-139.34411 + 1.575701e5/K - 6.642308e7/K^2 + 1.243800e10/K^3 - 8.621949e11/K^4)
}

calcCu       <- createIndexFun(kCopper)
calcTSS      <- createIndexFun(kTSS)
calcDO       <- createIndexFun(kDOStream)
calcDORiv    <- createIndexFun(kDORiver)
calcP        <- createIndexFun(kPhosphorus)
calcPF       <- createIndexFun(kPhosphorusFanno)
calcTemp     <- createIndexFun(kTemperature)
calcTempRiv  <- createIndexFun(kTemperatureRiver)
calcHg       <- createIndexFun(kMercury)
calcHgWill   <- createIndexFun(kMercuryWillamette)
calcEcoli    <- createIndexFun(kEcoli)
calcNH4      <- createIndexFun(kAmmoniaRatio)

#' Calculate the Portland Water Quality Index

#' Calculate the Portland Water Quality Index as used in the watershed report cards.
#' The function is vectorized. Each argument should be the same length, excepting pH which
#' may be length 1.
#' @param watershed name of the watershed
#' @param Cu dissolved copper concentration in ug / L
#' @param DO dissolved oxygen in mg / L
#' @param Ecoli E. coli concentration in 1 / 100 mL
#' @param Hg total mercury concentration in ug / L
#' @param NH4 ammonia-nitrogen concentration in mg / L
#' @param P phosphorus concentration in mg / L
#' @param TSS total suspended solids concentration in mg / L
#' @param Cel temperature in Celsius
#' @param pH pH
#' @return A data.frame of water quality index subindices and the overall index.
PWQI <- function(watershed, Cu, DO, Ecoli, Hg, NH4, P, TSS, Cel, pH = 7.8){
  i.cu    <- calcCu(Cu)
  i.tss   <- calcTSS(TSS)
  i.ecoli <- calcEcoli(Ecoli)
  NH4r    <- calculateAmmoniaRatio(NH4, pH, Cel)
  i.nh4   <- calcNH4(NH4r)
  i.do    <- ifelse(watershed %in% kWatersheds[c(6:8)], calcDORiv(DO), calcDO(DO)) # Will, Slough, Col
  i.p     <- ifelse(watershed %in% kWatersheds[c(1,3)], calcPF(P), calcP(P)) # Fanno
  i.temp  <- ifelse(watershed %in% kWatersheds[c(6,8)], calcTempRiv(Cel), calcTemp(Cel)) # Will, Col
  i.hg    <- ifelse(watershed %in% kWatersheds[8], calcHgWill(Hg), calcHg(Hg)) # Will
  i.do    <- adjustDO(i.do, DO, Cel)
  indices <-  cbind(Cu = i.cu, TSS = i.tss, Ecoli = i.ecoli, NH4 = i.nh4, 
                    DO = i.do, P = i.p, Temp = i.temp, Hg = i.hg)
  pwqi <- apply(indices, 1, function(x) 1/mean(1/x))
  ret <- data.frame(watershed, Cu = i.cu, TSS = i.tss, Ecoli = i.ecoli, NH4 = i.nh4, 
                    DO = i.do, P = i.p, Temp = i.temp, Hg = i.hg, PWQI = pwqi)
  ret[-1] <- lapply(ret[-1], round, digits = 0)
  ret
}

#library(reshape2)
#d <- expand.grid(analyte = kParameters, watershed = kWatersheds)
# kParameters <- c('Dissolved Copper' = 'Cu', 'Dissolved Oxygen' = 'DO', 'E. coli' = 'Ecoli',
#                  'Total Mercury' = 'Hg', 'Ammonia' = 'NH4', 
#                  'Total Phosphorus' = 'P', 'Total Suspended Solids' = 'TSS', 'Temperature' = 'Cel')
#kTestVals <- c(1.783, 6.1, 108, 5.142, 0.284, 0.259, 38, 18.5)
#d$result <- rep(kTestVals, times = length(kWatersheds))
#dc <- dcast(d, watershed ~ analyte, value.var = 'result')
#with(dc, PWQI(watershed, Cu, DO, Ecoli, Hg, NH4, P, TSS, Cel))


