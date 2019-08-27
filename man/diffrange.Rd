\name{diffrange}
\alias{diffrange}
\title{ difference of range }
\description{
 This function computes the length of the range of a sample. 
}
\usage{
diffrange(x)
}
\arguments{
  \item{x}{ a numeric vector}
}

\value{
  a numeric object 
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\seealso{ \code{\link{RCC}}, \code{\link{xbarRCC}} }

\examples{
x <- rnorm(10)
diffrange(x)
}
\keyword{models}% at least one, from doc/KEYWORDS
