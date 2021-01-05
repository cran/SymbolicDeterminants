\name{parsedetguide}
\alias{parsedetguide}
\title{Display Symbolic Representation of Determinant of pxp Matrix
}
\description{
Produces symbolic (textual) representation of determinant of pxp matrix from
previously defined determinant guide (detguide) for that matrix
}
\usage{
parsedetguide(p, storage, browser="Microsoft Edge", symmetric=FALSE, verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Name of directory where detguide is stored}
  \item{browser}{Name of program to be used as HTML browser}
  \item{symmetric}{Logical. T causes printing of determinant for symmetric matrix}
  \item{verbose}{Logical. TRUE causes printing of function ID before and after running}
}

\value{
Returns symbolic representation to screen and to existing external storage.
}
\author{
William R. Fairweather
}
\examples{
parsedetguide(p=3, storage=tempdir(), browser="Microsoft Edge", symmetric=FALSE, verbose=TRUE)
}
\keyword{symbolicmath }
\keyword{array }
