\name{joinExtraTransects}
\alias{joinExtraTransects}
\title{Join extra side channel transects}
\usage{
  joinExtraTransects(uid, transect, station, wetwid,
    bankwid, bankhgt, incishgt)
}
\description{
  This function joins the extra side channel transects with
  the parallel transect. Each measurement on the main
  channel is joined with the parallel measurement on the
  side channel using an aggregation function.  All
  measurements done at the transect (rather than at a
  station) are assumed to be at station 0 for purposes this
  function.  This allows us to do the join in one pass
  rather than separately.
}

