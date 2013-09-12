\name{calculateFishCoverMeans}
\alias{calculateBankCoverVar}
\alias{calculateFishCoverMeans}
\title{Calculate fish cover metrics}
\usage{
  calculateFishCoverMeans(uid, parameter, cover)

  calculateBankCoverVar(uid, parameter, cover)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{parameter}{a vector parameter codes as returned by
  \link{formatFishCover}. Values restricted to be one of
  alg, rck, brs, lvt, aqm, ohv, hum, ucb, lwd.}

  \item{cover}{a vector of cover values as returned by
  \link{formatFishCover}.  These are not the raw cover
  values (0-4) from the field form.}

  \item{parameter}{For calculateBankCoverVar, should a
  vector of 'ohv' or 'ucb' codes.}
}
\description{
  \code{calculateFishCoverMeans} calculates fish cover
  metrics.  The metrics returned are: pfc_alg, pfc_rck,
  pfc_brs, pfc_lvt, pfc_aqm, pfc_ohv, pfc_hum, pfc_ucb,
  pfc_lwd xfc_alg, xfc_rck, xfc_brs, xfc_lvt, xfc_aqm,
  xfc_ohv, xfc_hum, xfc_ucb, xfc_lwd pfc_all, pfc_big,
  pfc_nat xfc_all, xfc_big, xfc_nat.
  \code{calculateBankCoverVar} calculates a group of fish
  cover metrics dealing with the variability in cover
  provided by bank features (overhanging or undercut
  banks).  The metrics are sdfc_ucb, sdfc_ohv idrucb,
  idrohv, iqrucb, iqrohv.
}
\examples{
df <- expand.grid(uid = 1:10, transect = LETTERS[1:10],
                  parameter = c("ALGAE", "BOULDR", "BRUSH", "LVTREE",
                                "MACPHY", "OVRHNG", "STRUCT", "UNDCUT", "WOODY"))
df$cover <- sample(0:4, size = nrow(df), replace = T)
fc <- formatFishCover(df$uid, df$parameter, df$cover)
fcm <- calculateFishCoverMeans(fc$uid, fc$parameter, fc$cover)
fcsd <- with(subset(fc, parameter \%in\% c('ohv', 'ucb')),
             calculateBankCoverVar(uid, parameter, cover))
}

