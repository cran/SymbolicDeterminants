\name{retrieve}
\alias{retrieve}
\title{Retrieve Specific Detguide or Parsed Detguide
}
\description{
Retrieves a specified file from storage directory
}
\usage{
retrieve(p, storage, verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Quoted name of directory for storage of detguides}
  \item{verbose}{Logical. TRUE causes printing of program ID before and after running}
}
\details{Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
   If storage directory is in same folder as R Workspace, storage="./name" is sufficient.
   File to be retrieved is indicated interactively from console.
}
\value{Selects and retrieves a detguide file from the storage/p subdirectory
         or displays the symbolic determinant in a text editor
}
\author{
William R. Fairweather
}
\examples{retrieve(p=3,storage=tempdir(), verbose=TRUE)}

\keyword{symbolicmath}
\keyword{array}
