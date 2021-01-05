\name{detindex}
\alias{detindex}
\title{Index of Determinant Guides and Symbolic Representation Files}
\description{
Produces a table of existing detguides and parsed detguides for all p}
\usage{
detindex(storage, verbose=TRUE)
}
\arguments{
  \item{storage}{Quoted name of storage directory}
  \item{verbose }{Logical. TRUE causes printing of function ID before and after running}
}
\details{Provide full path in storage, using double backslashes.  Example:  storage="c:/determinants".  
   If storage directory "name" is in same folder as R Workspace, storage="./name" is sufficient.
}
\value{Table of existing detguides and existing results of parsing the detguides for each p in
    the storage directory}
\author{
William R. Fairweather
}
\examples{
detindex(storage=tempdir(),verbose=TRUE)}

\keyword{symbolicmath}
\keyword{array}
