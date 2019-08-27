\name{xLRCC}
\alias{xLRCC}
\title{ Computing x and LR-chart Parameters }
\description{
This function computes control limits for the x and LR-charts, optionally
revising them in the case that individual sample means and/or ranges plot out
of the trial control limits.   
}
\usage{
xLRCC(qc.obj, k=3, sigma, mu, revise=TRUE, newdata)
}
\arguments{
  \item{qc.obj}{an object of class CC}
  \item{k}{the control chart parameter governing the width of the control limits}
  \item{sigma}{ (optional) an estimate of the in-control standard deviation}
  \item{mu}{(optional) an estimate of the in-control mean}
  \item{revise}{if TRUE, mu and sigma are estimated using only range values that plot inside
the trial control limits}
  \item{newdata}{ an n-vector consisting of 
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
rr.xLR <- xLRCC(rr$Avg)
plot(rr.xLR)
}
\keyword{models}% at least one, from doc/KEYWORDS
