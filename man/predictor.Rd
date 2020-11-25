\name{predictor}
\alias{predictor}
\title{Calculates Size of Elements of Determinant Guide and of
   Symbolic Representation
}
\description{
In order to anticipate the impact of p on calculation time and storage, 
some indication can be found from size.predictor( ).
}
\usage{
predictor(p,verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{verbose }{Logical. TRUE" causes printing of function ID before and after running}
}
\details{Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
   If storage directory is in same folder as R Workspace, storage=".\\name" is sufficient.
}
\value{
   Summary of calculations in other functions of this package and their relationship
   to p, including the number of levels of positive terms and of negative terms, the 
   number of elements of the matrix in each term of the determinant, and the number
   of lines of text required for the symbolic representation of the determinant.
}
\author{
William R. Fairweather
}
\examples{predictor(p=10,verbose=TRUE)}
\keyword{symbolicmath }
\keyword{array }
