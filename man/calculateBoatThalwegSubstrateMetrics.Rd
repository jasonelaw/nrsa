\name{calculateBoatThalwegSubstrateMetrics}
\alias{calculateBoatLittoralSubstrateMetrics}
\alias{calculateBoatThalwegSubstrateMetrics}
\title{Calculate non-wadeable substrate metrics}
\usage{
  calculateBoatThalwegSubstrateMetrics(uid, size.class)

  calculateBoatLittoralSubstrateMetrics(uid, db, ds, sb,
    ss)
}
\arguments{
  \item{uid}{a vector of site-visit indicators}

  \item{size.class}{a vector size classes}

  \item{db}{dominant, bottom substrate classes}

  \item{ds}{dominant, shoreline substrate classes}

  \item{sb}{secondary, bottom substrate classes}

  \item{ss}{secondary, shoreline substrate classes}
}
\description{
  \code{calculateBoatSubstrateMetrics} is used to calculate
  non-wadeable substrate metrics
  d16,d50,d84,dgm,LDCBF_G08,lsub2d16inor,lsub2d50inor,lsub2d84inor,lsubd_sd,lsub_d16,
  ,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,lsub_iqr,n,pct_bh,pct_bl,pct_cb,
  ,pct_dbbl,pct_dbcb,pct_dbfn,pct_dbgc,pct_dbgf,pct_dbhp,pct_dbom,pct_dbot,pct_dbrc,
  ,pct_dbrr,pct_dbrs,pct_dbsa,pct_dbsb,pct_dbwd,pct_dbxb,pct_dsbl,pct_dscb,pct_dsfn,
  ,pct_dsgc,pct_dsgf,pct_dshp,pct_dsom,pct_dsot,pct_dsrc,pct_dsrr,pct_dsrs,pct_dssa,
  ,pct_dssb,pct_dswd,pct_dsxb,pct_fn,pct_gr,pct_ot,pct_sa,pct_safn,pct_sbbl,pct_sbcb,
  ,pct_sbfn,pct_sbgc,pct_sbgf,pct_sbhp,pct_sbom,pct_sbot,pct_sbrc,pct_sbrr,pct_sbrs,
  ,pct_sbsa,pct_sbsb,pct_sbwd,pct_sbxb,pct_ssbl,pct_sscb,pct_ssfn,pct_ssgc,pct_ssgf,
  ,pct_sshp,pct_ssom,pct_ssot,pct_ssrc,pct_ssrr,pct_ssrs,pct_sssa,pct_sssb,pct_sswd,
  ,pct_ssxb,
}

