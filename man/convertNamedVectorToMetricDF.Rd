\name{convertNamedVectorToMetricDF}
\alias{convertNamedVectorToMetricDF}
\title{Convert a named vector to a metric data.frame}
\usage{
  convertNamedVectorToMetricDF(x)
}
\arguments{
  \item{x}{a named vector}
}
\value{
  a 'metric' data.frame
}
\description{
  Converts a named vector to a metric data.frame.  The
  names should be the site identifiers, and the deparsed
  name of the the argument \code{x}, becomes the metric
  name.
}
\examples{
my_metric <- c('uid1' = 0.1, 'uid2' = 0.2, 'uid3' = 0.3)
convertNamedVectorToMetricDF(my_metric)
}

