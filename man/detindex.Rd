\name{detindex}
\alias{detindex}
\title{Index of Determinant Guides and Symbolic Representation Files}
\description{
Produces a table of existing detguides and parsed detguides for all p or a list of minidetguides and 
    parsed minidetguides for a particular p}
\usage{
detindex(storage,  mini=c("B","D","M"), pmini=1, verbose=TRUE)
}
\arguments{
  \item{storage}{Quoted name of storage directory}
  \item{mini}{Index includes (B)oth detguides and minidetguides, (D)etguides only, (M)inidetguides only}
  \item{pmini}{p for minidetguides and related files; ignored if mini='D'}
  \item{verbose}{Logical. TRUE causes printing of function ID before and after running}
}
\details{Provide full path in storage.  Example:  storage="c:/determinants".  
   If storage directory "name" is in same folder as R Workspace, storage="./name" is sufficient.
}
\value{
     \item{Detguides}{Table of existing detguides and existing results of parsing the detguides for each p in
            the storage directory}
     \item{Minidetguides}{Table of existing minidetguides and existing results of parsing the minidetguides for 
            p=pmini}
     \item{Call}{Call to this function}
}
\author{
William R. Fairweather
}
\examples{
detindex(storage=tempdir(),mini="B",pmini=5,verbose=TRUE)}

\keyword{symbolicmath}
\keyword{array}
