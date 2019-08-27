\name{RCC}
\alias{RCC}
\title{R Chart Parameters }
\description{
This function computes control limits for R-charts, optionally
revising them in the case that ranges plot out
of the trial control limits.   
}
\usage{
RCC(R, n, k=3, sigma)
}
\arguments{
  \item{R}{ a numeric vector consisting of the subgroup ranges }
  \item{n}{ the subgroup size}
  \item{k}{ the control chart parameter governing the width of the control limits}
  \item{sigma}{ (optional) an estimate of the in-control standard deviation}
  }

\value{
  a list consisting of the upper and lower control limits and the centerline
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{plot.CC} }

\examples{
x <- matrix(rnorm(120, sd = 3), ncol=12)
x.R <- xbarRCC(x)
plot(x.R)
x.R <- xbarRCC(x, newdata=rnorm(12))
plot(x.R)
}
\keyword{models}% at least one, from doc/KEYWORDS
