\name{anewminidet}
\alias{anewminidet}
\title{Create Determinant Guide (Minidetguide) for pxp Matrix
}
\description{
     A minidetguide is a series of p directories, each of which contains a matrix of 
     size (p-1) x (p-1)!/2, which codes the use of each element of a pxp matrix
     in the calculation of the determinant of that matrix. From the minidetguide, 
     the symbolic representation of the determinant can be produced directly.
}
\usage{
   anewminidet(p,storage,cols,verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Name of directory where detguides are stored.}
  \item{cols}{Numbers of columns to be created. Default is 1:p, indicating all columns}
  \item{verbose}{Logical. TRUE causes printing of function ID before and after running}
}
\details{
   Each determinant guide (minidetguide) is created by creating the set of all possible
   permutations of the numbers 1 through p except j and storing them as a matrix in 
   subdirectory R1Cj. This represents the set of columns corresponding to rows 2 through
   p of the products with leading element 1,j of the determinant.
}
\value{matrix described in details above
}
\author{
William R. Fairweather
}
\examples{
   anewminidet(p=5,storage=tempdir(),cols=1:5,verbose=TRUE)}
\keyword{symbolicmath}
\keyword{array}
