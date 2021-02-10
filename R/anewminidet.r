#' @export
anewminidet <-
function(p, storage, cols=1:p, verbose=TRUE)
{
     #                          anewminidet
     #
     # VALUE      Minidetguide or part of one for pxp matrix based on Leibniz formula of determinant.
     #            Part is defined by cols.
     #
     # INPUT     p           Size of matrix V (pxp)
     #           storage     Location for storage of matrices that make up minidetguide
     #           cols        Subset of minidetguide to prepare with this run. Default (1:p) causes
     #                          entire minidetguide to be calculated
     #
     #          verbose      Logical. TRUE causes printing of program ID before and after running.
     #
     # NOTE:    Minidetguide contains only the Plus part of a detguide, and the row (i) label of the 
     #          element of V is understood to be 2, 3, ..., p without showing it explicitely
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running anewminidet", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     ###############################################
     # Helper function to determine Plus and Minus #
     ###############################################
     PlusMinus <- function(x,column_num){
               # x           is a column vector including all column indicators #
               # column_num  is the number of the column in the minidetguide 
               #                before removing Minus coefficient termms #
          nx <- length(x)
          z <- rep(0,nx)
          for(i in 2:nx){
               z[i] <- sum(x[1:i]>x[i])
          }
          zsum <- sum(z)
          out <- 0
          if(zsum==2*floor(zsum/2)) out <- column_num
          out
     } 

#        MAIN PROGRAM STARTS HERE       #

     #####################################
     # Set up directory and subdirectory #
     #####################################
     storeoutput <- paste(storage, p, sep="/")
     if(!fs::dir_exists(storeoutput)) fs::dir_create(storeoutput, recursive = TRUE)
     #
     ##########################################################
     # Set up structure for detguide headed by a Row 1 element#
     # Vp detguide for this element has (p-1)! products       #
     # and each product has p-1 elements                      #
     # Store as a matrix of column designations, with row     #
     # designations understood to be   2,3,...,p              #
     ##########################################################
     prodpm1 <- prod(1:(p-1))
 
     for(jj in 1:p){
          if(any(jj==cols)){
               print(paste("Preparing minidetguide R1C",jj, sep=""), quote=FALSE)
               vset <- 1:p
               vset <- vset[-jj]
               xx <- arrangements::permutations(k=p-1, v=vset, layout="column")
               ######################################################
               # Restore full minidetguide and determine Plus Minus #
               ######################################################
               yy <- rbind(jj,xx)
               zz <- rep(0,prodpm1)
               for(i in 1:prodpm1){
                    zz[i] <- PlusMinus(yy[,i], i)
               }  #  i
               xx <- xx[,zz]   # Subset to Plus terms
               # Create file to receive minidetguide #
               R1Cx <- paste("R1C", jj, sep="")
               outfile <- paste(storeoutput, R1Cx, sep="/")
               if(!fs::dir_exists(outfile)) fs::dir_create(outfile, recursive = TRUE)
               outfile <- paste(outfile, "minidetguide.txt", sep="/")
               dump(list = "xx", file=outfile)
               xx <- NULL
          }    # if any
     }   #   jj
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running anewminidet", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
