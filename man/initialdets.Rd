\name{initialdets}
\alias{initialdets}
\title{Set Up Storage Directories and Calculate Determinant Guides for p = 2, 3, 4 and 5}
\description{
     A determinant guide is a list of 2 levels, each of which contains a list of p!/2
     levels, that codes the use of each element of a pxp matrix in the calculation
     of the determinant of that matrix. From the guide, the 3 or 4 line symbolic
     representation of the determinant can be produced. This function creates the
     determinant guide for p = 2, 3, 4 and 5.
}
\usage{
initialdets(storage=tempdir(), diagnose=FALSE, verbose=TRUE)}
\arguments{
  \item{storage }{Name of directory where detguides are stored.tempdir() erases entries at end of current session}
  \item{diagnose }{Logical. T causes printing of diagnostic content}
  \item{verbose }{Logical. T causes printing of function ID before and after running}
}
\value{a list including elements
   \item{1}{max.created}
   \item{2}{detguide for p=2}
   \item{3}{detguide for p=3}
   \item{4}{detguide for p=4}
   \item{5}{detguide for p=5}
   \item{6}{storage location}
}
\author{
William R. Fairweather
}
\examples{
\donttest{initialdets(storage=tempdir(), diagnose=FALSE, verbose=TRUE)
} 
}
\keyword{symbolmath }
\keyword{array }
