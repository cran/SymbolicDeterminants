#' @export
confirm.minidet <-
function(x=NULL, p, storage, verbose=TRUE)
{
     #                          confirm.mini.det
     #
     # VALUE    Two calculations of matrix determinant, the first by product of eigenvalues 
     #          and the second by evaluating the minidetguide of symbolic determinant.
     #
     # INPUT    x            Square matrix of known numbers. If NULL, random comparitor is created here.
     #          p            Size of matrix (pxp) for which symbolic representation of determinant 
     #                          has been prepared.  Same size as x.  Prepared by aminidet() function. 
     #          storage      Quoted name of directory for storage of detguides. 
     #
     #          verbose     Logical. T causes printing of program ID before and after running.
     #
     # DETAILS  Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
     #              If storage directory "name" is in same folder as R Workspace, storage=".\\name" is sufficient.
     #  
     MC <- match.call()
     if(verbose) {
          print("", quote = F)
          print("Running confirm.mini.det", quote = F)
          print("", quote = F)
          print(date(), quote = F)
          print("", quote = F)
          print("Call:", quote = F)
          print(MC)
          print("", quote = F)
     }
     ####################################################################
     # Create helper function to parse and apply product of each matrix #
     ####################################################################
     matproductmini <- function(u,thisCol){
          # VALUE is sum of products of respective terms
          # INPUT    u is a (p-1) x nterms matrix
          #          thisCol is value of R1Cs (ie, mm)
          sumprod <- 0
          for(i in 1:nterms){
               prod1 <- 1
               ###########################################
               # Rebuild each product in detguide format #
               ###########################################
               z <- matrix(1:p, nrow=p,ncol=2, byrow=FALSE)
               z[,2] <-c(thisCol,u[,i]) 
               for(jj in 1:p){
                    row1 <- z[jj,1]
                    col1 <- z[jj,2]
                    prod1 <- prod1 * x[row1,col1]
               }     #   jj
               sumprod <- sumprod + prod1
          }         #   i
          sumprod
     }     # matproductmini

############            MAIN PROGRAM STARTS HERE                #################

     #######################################
     # Ensure that all minidetguides exist #
     #######################################
     detfilep <- paste(storage,p,sep="/")
     allminis <- paste("R1C",1:p, sep="")
     allminis <- paste(detfilep,allminis,sep="/")
     R1Cs <- paste(allminis,"minidetguide.txt",sep="/")
     minidetguides <- rep(FALSE,p)
     for(i in 1:p){
          minidetguides[i] <- file.exists(R1Cs[i])
     }
     if(!all(minidetguides)){
          print("The following minidetguides do not exist:", quote=F)
          print(R1Cs[!minidetguides], quote=F)
          stop("Execution aborted")
     }
     ####################
     # Need comparator? #
     ####################
     if(is.null(x)){
          x <- matrix(stats::runif(p*p),p,p)
     }
     ###################################
     # Ensure comparability of x and y #
     ###################################
     dimx <- dim(x)[1]
     if(dimx!=p)stop("x and the minidetguide must represent numeric and symbolic matrices of same size")
     #
     ###############################################
     # Calculate determinant from eigenvalues of x #
     ###############################################
     ev <- eigen(x,only.values=TRUE)[[1]]
     detx <- prod(ev)
     #
     #############################################################
     # Calculate determinant by applying symbolic representation #
     # of each minidetguide                                      #
     # Call internal matproduct() support function from here,    #
     # first to evaluate the Plus products and then the Minus    #
     #############################################################
     dety <- 0
     nterms <- prod(3:(p-1))       #   each minidetguide is nterms wide
     for(mm in 1:p){
          y <- source(R1Cs[mm])[[1]]           # a (p-1) x nterms matrix
          dety <- dety + matproductmini(y, mm)

          y[c(p-2,p-1),] <- y[c(p-1,p-2),]
          dety <- dety - matproductmini(y, mm)
     }    #   mm
     #
     ########################
     # Compare determinants #
     ########################
     print("Comparing determinants", quote=FALSE)
     print("", quote = FALSE)
     print(paste("By eigenvalues ",detx), quote = FALSE)
     print(paste("By symbolic rep",dety), quote = FALSE)
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running confirm.mini.det", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
