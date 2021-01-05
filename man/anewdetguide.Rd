\name{anewdetguide}
\alias{anewdetguide}
\title{Create Determinant Guide (detguide) for pxp Matrix
}
\description{
     A determinant guide is a list of 2 levels, each of which contains a list of p!/2
     levels, which codes the use of each element of a pxp matrix in the calculation
     of the determinant of that matrix. From the guide, the symbolic representation 
     of the determinant can be produced directly.
}
\usage{
   anewdetguide(p,storage,verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Name of directory where detguides are stored.}
  \item{verbose}{Logical. TRUE causes printing of function ID before and after running}
}
\details{
   Each determinant guide (detguide) is created by cofactor expansion from the detguide of 
   the next smaller square matrix. If p is set larger than any existing detguide, all 
   lower detguides larger than the largest existing one are first created in turn.
}
\value{"list"
  \item{comp1}{Products in determinant with +1 coefficient}
  \item{comp2}{Products in determinant with -1 coefficient}
}
\author{
William R. Fairweather
}
\examples{
anewdetguide(p=5,storage=tempdir(),verbose=TRUE)}
\keyword{symbolicmath}
\keyword{array}
