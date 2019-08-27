\name{d_3}
\alias{d_3}
\title{ range factors }
\description{
 The sample range \code{R} is a biased estimator for the
 population standard deviation of R.  For a normally distributed sample of
 size \code{n}, \code{d_3(n)R} is an unbiased estimator of the
 standard deviation of R.
}
\usage{
d_3(n)
}
\arguments{
  \item{n}{ the sample size}
}

\value{
  a numeric value representing the multiplier to render R unbiased for
  the standard deviation of R
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{\link{d_2}} }

\examples{
x <- rnorm(10, sd = 3)
R <- diff(range(x))
d_3(10)*R   # expected value of this is (V(R))^{1/2}
}
\keyword{models}% at least one, from doc/KEYWORDS
