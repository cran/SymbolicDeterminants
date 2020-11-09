#' @export
confirm.det <-
function(x=NULL, p, storage=tempdir(), verbose=TRUE)
{
     #                          confirm.det
     #
     # VALUE    Two calculations of matrix determinant, the first by product of eigenvalues 
     #          and the second by evaluating of guide of symbolic determinant.
     #
     # INPUT    x            Square matrix of known numbers. If NULL, random comparitor created here.
     #          p            Size of matrix (pxp) for which symbolic representation of determinant 
     #                          has been prepared.  Same size as x
     #                       Usually prepared by initialize() or make.guide() function. 
     #          storage      Name of directory for storage of detguides. tempdir() causes loss of detguides created in this session.
     #                       Recommend to select a different name. This function will create it if it doesn't already exist
     #
     #          diagnose    Logical. T causes printing of diagnostic content
     #          verbose     Logical. T causes printing of program ID before and after running.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = F)
          print("Running confirm.det", quote = F)
          print("", quote = F)
          print(date(), quote = F)
          print("", quote = F)
          print("Call:", quote = F)
          print(MC)
          print("", quote = F)
     }
     detfile <- paste(storage,p,"detguide.txt",sep="/")
     y <- source(detfile)[[1]]
     dimy <- dim(y[[1]][[1]])[1]
     ####################
     # Need comparator? #
     ####################
     if(is.null(x)){
          x <- matrix(stats::runif(dimy*dimy),dimy,dimy)
     }
     ###################################
     # Ensure comparability of x and y #
     ###################################
     dimx <- dim(x)[1]
     if(dimx!=dimy){
          Hmisc::prn(dimx)
          Hmisc::prn(dimy)
          stop("x and y must represent numeric and symbolic matrices of same size")
     }
     ####################################################################
     # Create helper function to parse and apply product of each matrix #
     ####################################################################
     matproduct <- function(n){
          # VALUE is vector of positive product and negative product of respective terms
          # INPUT    n is number of list level of y 
          u1 <- y[[1]][[n]]
          u2 <- y[[2]][[n]]
          prod1 <- prod2 <- 1
          for(i in 1:dimy){
               row1 <- u1[i,1]
               col1 <- u1[i,2]
               prod1 <- prod1 * x[row1,col1]
               row2 <- u2[i,1]
               col2 <- u2[i,2]
               prod2 <- prod2 * x[row2,col2]
          } 
          c(prod1,prod2)
     }
     ###############################################
     # Calculate determinant from eigenvalues of x #
     ###############################################
     ev <- eigen(x,only.values=TRUE)[[1]]
     detx <- prod(ev)
     #
     #############################################################
     # Calculate determinant by applying symbolic representation #
     # Call internal matproduct() support function from here     #
     #############################################################
     dety <- 0
     leny <- length(y[[1]])
     for(i in 1:leny){
          uu <- matproduct(i)
          dety <- dety + uu[1] - uu[2]
     }
     ########################
     # Compare determinants #
     ########################
     print("Comparing determinants", quote=FALSE)
     print("", quote = FALSE)
     print(paste("By eigenvalues ",detx))
     print(paste("By symbolic rep",dety))
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running confirm.det", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
