\name{xbarRCC}
\alias{xbarRCC}
\title{Computing xbar and R-chart Parameters}
\description{
This function computes control limits for the xbar and R-charts, optionally
revising them in the case that individual sample means and/or ranges plot out
of the trial control limits.   
}
\usage{
xbarRCC(qc.obj, k=3, sigma, mu, revise=TRUE, newdata)
}
\arguments{
  \item{qc.obj}{an object of class CC}
  \item{k}{the control chart parameter governing the width of the control limits}
  \item{sigma}{ (optional) an estimate of the in-control standard deviation}
  \item{mu}{(optional) an estimate of the in-control mean}
  \item{revise}{if TRUE, mu and sigma are estimated using only range values that plot inside
the trial control limits}
  \item{newdata}{ an n-vector or matrix having n columns, consisting of 
data from new subgroups}
}
\value{
  an object of class "CC"
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{plot.CC} }
\examples{
x <- matrix(rnorm(120, sd = 3), ncol=12)
x.R <- xbarRCC(x)
plot(x.R)
}
\keyword{models}% at least one, from doc/KEYWORDS
