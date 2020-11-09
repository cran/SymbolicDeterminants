\name{confirm.det}
\alias{confirm.det}
\title{Confirm Accuracy of Symbolic Determinant Defined by Initialdets or Makedetguide Functions}
\description{
     Compares the determinant calculated by multiplying eigenvalues of a matrix to that
     calculated by applying the symbolic representation created by this package. 
}
\usage{confirm.det(x=NULL, p, storage=tempdir(), verbose=TRUE)}
  \arguments{
  \item{x }{a user-defined matrix, or NULL}
  \item{p }{size of matrix (pxp) for which a detguide has been prepared}
  \item{storage }{Name of directory from which to get detguide}
  \item{verbose }{Logical. T causes printing of function ID before and after running}
}
\value{Determinant of x produced obtained by multiplying its eigenvalues and determinant 
   of x obtained by applying the detguide defined by p
}
\author{
William R. Fairweather
}
\examples{
\dontrun{confirm.det(x=NULL, p=5, storage=tempdir(), verbose=TRUE)
} 
}

\keyword{symbolmath }
\keyword{array }