\name{LRCC}
\alias{LRCC}
\title{Lowess-Range-Chart}
\description{
 Trial limits for the Lowess-Range Chart, and an estimator of the process
standard deviation.  This chart is appropriate for subgroups of size 1.
}
\usage{
LRCC(x, sigma, plotit = FALSE)
}
\arguments{
  \item{x}{a numeric vector containing the process measurements }
  \item{sigma}{(optional) the standard deviation of the measurements}
  \item{plotit}{logical value indicating whether a plot should be produced}
}

\value{
  an estimate of the standard deviation and a plot of the chart
  }
\references{Braun, W.J. (2002) The LR-chart: An Alternative to the MR-chart.
}
\author{W.J. Braun }
\seealso{ \code{\link{xbarRCC}} }

\examples{
LRCC(rnorm(10))
}
\keyword{models}% at least one, from doc/KEYWORDS
