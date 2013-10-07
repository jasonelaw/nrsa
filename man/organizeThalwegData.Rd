\name{organizeThalwegData}
\alias{organizeThalwegData}
\title{Organize thalweg data for residual pool analysis}
\usage{
  organizeThalwegData(uid, transect, station, is.wadeable,
    depth, increment, slope, minimum.proportion = 0.85)
}
\arguments{
  \item{uid}{a vector of site identifiers}

  \item{transect}{a vector of transect identifiers}

  \item{station}{a vector of station identifiers}

  \item{is.wadeable}{logical; \code{TRUE} if the site is
  wadeable}

  \item{depth}{a vector of thalweg depths}

  \item{increment}{a vector of distances from each thalweg
  point to the previous}

  \item{slope}{a vector of average site slope}

  \item{minimum.proportion}{the minimum proportion sampled
  necessary for the site to be included.}
}
\description{
  This function adds a sequence indicator to a thalweg
  profile, creates additional 'nick points', and removes
  incomplete sites.
}

