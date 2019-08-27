\name{d_2}
\alias{d_2}
\title{ range factors }
\description{
 The sample range \code{R} is a biased estimator for the
 population standard deviation.  For a normally distributed sample of
 size \code{n}, \code{R/d2(n)} is unbiased.  
}
\usage{
d_2(n)
}
\arguments{
  \item{n}{ the sample size}
}

\value{
  a numeric value representing the multiplier to render R unbiased
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{\link{d_3}} }

\examples{
x <- rnorm(10, sd = 3)
R <- diff(range(x))
R/d_2(10)   # expected value of this is 3
}
\keyword{models}% at least one, from doc/KEYWORDS
