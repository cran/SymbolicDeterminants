\name{parsemini}
\alias{parsemini}
\title{Display Symbolic Representation of Determinant of pxp Matrix
}
\description{
Produces symbolic (textual) representation of determinant of pxp matrix from
previously defined determinant guide (minidetguide) for that matrix
}
\usage{
parsemini(p, storage, symmetric, cols=1:p, verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Name of directory where detguide is stored}
  \item{symmetric}{Logical. T causes printing of determinant for symmetric matrix}
  \item{cols}{Column numbers of minidetguide to be calculated.  Default (1:p) causes all to
         be calculated}
  \item{verbose}{Logical. TRUE causes printing of function ID before and after running}
}

\value{
Returns symbolic representation to screen and to existing external storage.
}
\author{
William R. Fairweather
}
\examples{parsemini(p=5, storage=tempdir(), symmetric=FALSE,cols=1:5, verbose=TRUE)
}
\keyword{symbolicmath }
\keyword{array }
