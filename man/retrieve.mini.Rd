\name{retrieve.mini}
\alias{retrieve.mini}
\title{Retrieve Specific Minidetguide or Parsed Minidetguide 
}
\description{
Retrieves a specified file from storage directory
}
\usage{
retrieve.mini(p, storage, 
    browser="C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", 
    verbose=TRUE)
}
\arguments{
  \item{p}{Size of matrix (pxp)}
  \item{storage}{Quoted name of directory for storage of detguides}
  \item{browser}{Name of program to be used as HTML browser} 
  \item{verbose}{Logical. TRUE causes printing of program ID before and after running}
}
\details{Provide full path in storage.  Example:  storage="c:/determinants".  
   File to be retrieved is indicated interactively from console.
}
\value{Selects and retrieves a minidetguide file from the storage/p/ subdirectory
         or displays the symbolic determinant in an HTML editor
}  
\author{
William R. Fairweather
}
\examples{\dontrun{retrieve.mini(p=5,storage=tempdir(), 
    browser="C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", 
    verbose=TRUE)}
}
\keyword{symbolicmath}
\keyword{array}
