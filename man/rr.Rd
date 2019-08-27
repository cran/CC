\name{rr}
\alias{rr}
\docType{data}
\title{Baseline Heart Rate Summaries}
\description{Daily averages and variances for 2 minute records of 
R wave-to-R wave (RR) intervals for a single male individual. Measurements
were recorded during the summer of 2018.   
}
\usage{data(rr)}
\format{A data frame with 76 observations on the following 2 variables.
\describe{
    \item{\code{Avg}}{Daily averages}
    \item{\code{Var}}{Daily variances}
}
}
\examples{
sapply(rr, mean)
}
\keyword{datasets}

