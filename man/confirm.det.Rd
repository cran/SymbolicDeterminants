\name{confirm.det}
\alias{confirm.det}
\title{Confirm Accuracy of Symbolic Determinant Created by anewdetguide() Function}
\description{
     Compares the determinant calculated by multiplying eigenvalues of a matrix to that
     calculated by applying the symbolic representation created by this package. 
}
\usage{confirm.det(x=NULL, p, storage, verbose=TRUE)}
  \arguments{
  \item{x }{a user-defined matrix, or NULL}
  \item{p }{size of matrix (pxp) for which a detguide has been prepared}
  \item{storage }{Quoted name of directory from which to get detguide}
  \item{verbose }{Logical. T causes printing of function ID before and after running}
}
\value{Determinant of x produced obtained by multiplying its eigenvalues and determinant 
   of x obtained by applying the detguide defined by p
}
\author{
William R. Fairweather
}
\examples{confirm.det(x=NULL, p=5,storage=tempdir(),verbose=TRUE)}

\keyword{symbolmath }
\keyword{array }
