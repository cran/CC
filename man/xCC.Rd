\name{xCC}
\alias{xCC}
\title{x-Chart Parameters }
\description{
This function computes control limits for the x-charts, optionally
revising them in the case that individual values plot outside
the trial control limits.   
}
\usage{
xCC(x, sigma, k = 3, mu, newdata)
}
\arguments{
  \item{x}{a numeric vector consisting of the subgroup averages }
  \item{k}{the control chart parameter governing the width of the control limits}
  \item{sigma}{an estimate of the in-control standard deviation}
  \item{mu}{(optional) an estimate of the in-control mean}
  \item{newdata}{(optional) new observations}
}
\value{
  a list consisting of the upper and lower control limits and the centerline.
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{plot.CC} }
\keyword{models}% at least one, from doc/KEYWORDS
