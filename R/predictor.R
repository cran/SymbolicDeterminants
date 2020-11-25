#' @export
predictor <-
function(p, verbose=TRUE)
{
     #                          predictor
     #
     # VALUE    Size of various attributes of symbolic representation of general matrix
     #
     # INPUT    p            Size of matrix (pxp)
     #          verbose      Logical. T causes printing of program ID before and after running.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = F)
          print("Running predictor", quote = F)
          print("", quote = F)
          print(date(), quote = F)
          print("", quote = F)
          print("Call:", quote = F)
          print(MC)
          print("", quote = F)
     }
     linereturn <- "\n"
     #
     #############################################
     # Confirm that the input parameters conform #
     #############################################
     if(p < 2) stop("p must be an integer greater than 1")
     if(floor(p) != p)stop("p must be an integer")
     #
     ########################################
     # Set up and print general information #
     ########################################
     line1 <- "The symbolic representation (formula) for the determinant of a pxp matrix is a sum" 
     line2 <- "of p! terms, half of which have a +1 coefficient and half of which have a -1 coefficient."
     line3 <- "Each term is the product of p elements of the matrix. A term can appear only once in each"
     line4 <- "product in general, but can appear twice if the matrix is symmetric. Pairs appear as" 
     line5 <- "squared terms in the output."

     line7 <- "Printing a symbolic representation takes considerable space on a page. The number of spaces" 
     line8 <- "required to print each term depends on the value of p:"

     line9  <- "    p     spaces" 
     line10 <- "  1 - 9       4"
     line11 <- " 10 - 99      6"
     line12 <- "100 - 999     8"

    line14 <- "Three additional spaces are used in each line for the sign of the product."

    line15 <- "The number of lines required depends upon p and whether the matrix is symmetric."  
    line16 <- "The p!/2 Plus terms are printed and then the same number of Minus terms. Each term requires" 
    line17 <- "4 lines: 1 for the exponent, 1 for the matrix symbol, 1 for the subscripts, and one space" 
    line18 <- "between line sets. For nonsymmetric matrices, the exponent line is omitted."

    cat(line1,line2,line3,line4,line5,sep=linereturn)
    cat(line7,line8,sep=linereturn)
    cat(line9,line10,line11,line12,sep=linereturn)
    cat(line14,sep=linereturn)
    cat(line15,line16,line17,line18,sep=linereturn)

     print("", quote = F)
     print(paste("For p=",p,"the number of terms is p!=",prettyNum(prod(1:p),big.mark=","),", half of which"), quote=FALSE)
     print("have a plus sign and half have a minus sign.", quote=FALSE)
     print("", quote = F)
     print(paste("Each product term contains exactly",p,"elements of the matrix."), quote=FALSE)
     print("", quote = F)
     print(paste("The total number of printed lines is approximately", prettyNum(4*prod(1:p),big.mark=",")), quote=FALSE)
     #
     if(verbose) {
          print("", quote = F)
          print("Finished running predictor", quote = F)
          print("", quote = F)
          print(date(), quote = F)
          print("", quote = F)
     }
}
