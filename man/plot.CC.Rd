\name{plot.CC}
\alias{plot.CC}
\title{Plot Shewhart Control Charts}
\description{
 This function plots control charts of various types.
}
\usage{
\method{plot}{CC}(x, start = 1, ...)
}
\arguments{
  \item{x}{Object of class "CC"}
  \item{start}{Index of first plotted point}
  \item{...}{Arguments to be passed to methods, such as graphical
          parameters (see 'par').  This method also accepts the
          argument 'start' which specifies the index of first plotted point,
          which is 1L by default.}
}
\value{
  a plot of control charts for variability and location
  }
\references{Montgomery, D.C. (2000) Introduction to Statistical Quality
Control, Wiley }
\author{W.J. Braun }
\keyword{hplot}% at least one, from doc/KEYWORDS
