\name{rrCC}
\alias{rrCC}
\title{Individuals Charts Parameters for Baseline RR Mean and Variability}
\description{
This function computes control limits for the x-charts for
the mean and standard deviation (reciprocal) of R wave to R wave values for heart rate, optionally
revising them in the case that individual sample means and/or ranges plot out
of the trial control limits.   
}
\usage{
rrCC(RR, k=3, revise=TRUE, newdata)
}
\arguments{
  \item{RR}{a data frame with 2 columns, the first containing rr averages and the second containing rr variances}
  \item{k}{the control chart parameter governing the width of the control limits}
  \item{revise}{if TRUE, mu and sigma are estimated using only range values that plot inside
the trial control limits}
  \item{newdata}{ a 2 column matrix consisting of 
data from new subgroups, with averages in the first column and variances
in the second column}
}
\value{
  an object of class "CC"
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{plot.CC} }
\examples{
rr.xLR <- rrCC(rr)
plot(rr.xLR)
}
\keyword{models}% at least one, from doc/KEYWORDS
