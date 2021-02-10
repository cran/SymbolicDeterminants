#' @export
parsemini <-
function(p, storage, symmetric, cols=1:p, verbose=TRUE)
{
     #                          parsemini
     #
     # VALUE   Symbolic representation of determinant of matrix based on minidetguides
     #
     # INPUT    p           Size of matrix (pxp)
     #          storage     Directory where all detguides and symbolic representations are stored.
     #          symmetrc    Logical. TRUE causes symbolic representation of symmetric matrix to be created
     #          cols        Column numbers of minidetguide to be calculated.  Default (1:p) causes all to
     #                           be calculated.
     #
     #          verbose      Logical. TRUE causes printing of program ID before and after running.
     #
     # NOTE     Use retrieve.mini() to see results of this function
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running parsemini", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     ########################
     # Two helper functions #
     ########################
     my.html.element <- function(i, j, k = NULL, p, verbose = FALSE) {
          out <- paste("v<sup>", k, "</sup><sub>", 
                i, ",", j, "</sub>", sep = "")
          if (is.null(k)) 
                out <- paste("v<sub>", i, ",", j, 
                  "</sub>", sep = "")
          out <- paste(out, "&nbsp; ", sep = "")
          out
      }
      #
      my.html.product <- function(x) {
        dimx <- dim(x)
        p <- dimx[1]
        out <- NULL
        for (n in 1:p) {
            if (dimx[2] == 2) {
                out <- paste(out, my.html.element(i = x[n, 1], 
                  j = x[n, 2], p = p), sep = "")
            }
            else {
                if (x[n, 3] > 0) {
                  if (x[n, 3] == 1) {
                    out <- paste(out, my.html.element(i = x[n, 
                      1], j = x[n, 2], k = NULL, p = p), sep = "")
                  }
                  else {
                    out <- paste(out, my.html.element(i = x[n, 
                      1], j = x[n, 2], k = x[n, 3], p = p), sep = "")
                  }
                }
            }
        }
        out
     }
     
     #           MAIN FUNCTION STARTS HERE       #

    if (p < 4) 
        stop("p must be an integer greater than 3")
    if (floor(p) != p) 
        stop("p must be an integer")

     ###################################################
     # Retrieve Plus for minidetguide R1C1 thru R1Cp   #
     # Sort it for printing. Print Plus and then Minus #
     # for each R1Cx in turn. Process symmetric        #
     # matrices separately below                       # 
     ###################################################

     linereturn <- "\n"
     nlinesets2 <- prod(3:(p-1))        
     pickup <- paste(storage, p, sep="/")
     mat <- matrix(0, nrow=p, ncol=2)
     mat[,1] <- 1:p
     if (!symmetric){
          for(jj in 1:p){
            if(any(jj==cols)){
               R1Cx <- paste("R1C",jj, sep="")
               subpickup <- paste(pickup, R1Cx, "minidetguide.txt", sep="/")
               if(!file.exists(subpickup)) stop("There is no minidetguide in the directory.")
               xx <- source(subpickup)[[1]]

               ord <- "xx[1,]"
               for(nn in 2:(p-1)){
                    nextord <- paste("xx[", nn, ",]", sep="")
                    ord <- paste(ord, nextord, sep=",") 
               }   #  nn
               xxord <- paste("xx[,order(",ord,")]",sep='')
               xx <- eval(parse(text=xxord))
               xx <- rbind(jj,xx)
#  xx is now an ordered minidetguide for Plus products

               parseguide <- paste(pickup, R1Cx, "parseguide.htm", sep = "/")
               parsexx <- '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
               lineno <- 1

               xxhead <- paste("Symbolic Representation of the Determinant of a Real, ", 
                   p, "x", p, " Matrix", "<br>", "<br>", sep = "")
               if (file.exists(parseguide)) 
                   file.remove(file = parseguide)

               file.create(parseguide)
               cat(parsexx, file = parseguide, sep = linereturn, append = TRUE)
               cat(xxhead, file = parseguide, sep = linereturn, append = TRUE)
               cat("<html>", file = parseguide, sep = linereturn, 
                    append = TRUE)
               cat("</head>", file = parseguide, sep = linereturn, 
                    append = TRUE)
               cat("<body>", file = parseguide, sep = linereturn, 
                   append = TRUE)
               primeline2 <- "&nbsp;&nbsp;&nbsp;&nbsp;"
              ######################################
              # Create HTML file for Plus products #
              ######################################
              for (m in 1:nlinesets2) {
                   mat[,2] <- xx[,m]
                   ww <- my.html.product(mat)
                   ww <- paste(primeline2, ww, "&nbsp;", "&nbsp;", 
                        "&nbsp;", "&nbsp;", "&nbsp;", 
                        "&nbsp;", paste(R1Cx, lineno, sep=" - "), "<br>", sep = "")
                   cat(ww, file = parseguide, sep = linereturn, 
                        append = TRUE)
                   primeline2 <- "&nbsp;+&nbsp;"
                   lineno <- lineno + 1
              }   # m

              ############################################
              # Continue HTML file adding Minus products #
              ############################################
              primeline2 <- "&nbsp;-&nbsp;"
              xx[c(p-1,p),] <- xx[c(p,p-1),]

              cat("&nbsp; <br>", file = parseguide, sep = linereturn, 
                      append = TRUE)

              for (m in 1:nlinesets2) {
                   mat[,2] <- xx[,m]
                   ww <- my.html.product(mat)
                   ww <- paste(primeline2, ww, "&nbsp;", "&nbsp;", 
                        "&nbsp;", "&nbsp;", "&nbsp;", 
                        "&nbsp;", paste(R1Cx, lineno, sep=" - "), "<br>", sep = "")
                  cat(ww, file = parseguide, sep = linereturn, 
                       append = TRUE)
                  lineno <- lineno + 1
              }   # m
              cat("</p>", file = parseguide, sep = linereturn, 
                      append = TRUE)

              cat("</body>", file = parseguide, sep = linereturn, 
                     append = TRUE)
              cat("</html>", file = parseguide, sep = linereturn, 
                     append = TRUE)
            }   # if any
          }   #   jj
     }     #  NOT symmetric
###########################################################################################################################################

     if(symmetric){
          #########################################################################################################################
          # Prepare a detguide from the minidetguide for each Plus, each Minus within each element of the first row of the matrix #
          #########################################################################################################################
          for(jj in 1:p){
            if(any(jj==cols)){
               R1Cx <- paste("R1C",jj, sep="")
               subpickup <- paste(pickup, R1Cx, "minidetguide.txt", sep="/")
               if(!file.exists(subpickup)) stop("There is no minidetguide in the directory.")
               xx <- source(subpickup)[[1]]
               symprod <- prod(3:(p-1))
               basemat <- matrix(2:p, nrow=p-1, ncol=2, byrow=FALSE)
               #
               ############################################################ 
               # Make a list with (p-1)!/2 levels, px2 matrices out of x  #
               # Convert unstructured matrix to symmetric matrix          #
               # At each level, reverse to upper triangle matrix          #
               ############################################################
               yy <- vector("list", symprod)
               for(m in 1:nlinesets2){
                    yym <- basemat
                    yym[,2] <- xx[,m]
                    yym <- cbind(yym,1)
                    for(q in 1:(p-1)){
                         if(yym[q,1] > yym[q,2]){
                              yymtemp <- yym[q,1]
                              yym[q,1] <- yym[q,2]
                              yym[q,2] <- yymtemp
                         }
                    }
                    ########################################################
                    # Order yym by column 2 within column 1                #
                    # and determine coefficients after appending first row #
                    ########################################################
                    yym <- yym[order(yym[,1],yym[,2]),]
                    yym <- rbind(1,yym)
                    yym[1,2] <- jj
                    zzm <- yym[-1,]
                    for(cc in 1:(p-1)){
                         if(yym[cc,1]==zzm[cc,1] & yym[cc,2]==zzm[cc,2]){
                              yym[cc,3] <- 2
                              yym[cc+1,3] <- 0
                         }
                    }
                    yy[[m]] <- yym
               }   #   m
               ###################################
               # Set index for multiple products #
               ###################################
               coeffs <- rep(1,nlinesets2) 
               for(m in 1:(nlinesets2-1)){
                    A <- yy[[m]][,1:2]
                    for(mm in (m+1):nlinesets2){
                         B <- yy[[mm]][,1:2]
                         uu <- A==B
                         if(all(c(uu))){
                              coeffs[m] <- coeffs[m] + 1
                              coeffs[mm] <- 0
                         }
                    }    #   mm
               }    #   m
               TotalPlus <- symprod - sum(coeffs==0)
               SinglePlus <- sum(coeffs==1)
               MultiplePlus <- sum(coeffs>1)
               #
               ################################################################
               # Set up HTML file and print Plus half of symbolic determinant #
               ################################################################
               parseguidesym <- paste(pickup, R1Cx, "parseguidesym.htm", 
                          sep = "/")

               parsexx <- '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
               lineno <- 1

               xxhead <- paste("Symbolic Representation of the Determinant of a Real, Symmetric ", p, "x", p, " Matrix", sep="")
               if(file.exists(parseguidesym)) file.remove(file=parseguidesym)
               file.create(parseguidesym)
               cat(parsexx, file=parseguidesym, sep=linereturn, append=TRUE)

               cat("<html>", file=parseguidesym, sep=linereturn, append=TRUE)

               cat("<head>", file=parseguidesym, sep=linereturn, append=TRUE)
               cat(xxhead, file=parseguidesym, sep=linereturn, append=TRUE)
               cat("</head>",file=parseguidesym, sep=linereturn, append=TRUE)
               cat("<body>", file=parseguidesym, sep=linereturn, append=TRUE)
               cat("<p>",file=parseguidesym, sep=linereturn, append=TRUE)
     
               primeline2 <- "&nbsp;&nbsp;&nbsp;&nbsp;"
               for(m in 1:nlinesets2){
                    #############################################################
                    # Add coefficient if needed and skip any with coefficient 0 #
                    #############################################################
                    if(coeffs[m]!=0){
                         ww <- yy[[m]]
                         ww <- my.html.product(ww)  
                         if(coeffs[m] > 1){
                              ww <- paste(coeffs[m]," &nbsp;", ww)
                         }
                         if(coeffs[m] <=1){
                              ww <- paste("&nbsp;&nbsp;&nbsp;",ww)

                         }
                         ww <- paste(primeline2, ww, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", paste(R1Cx, lineno, sep=" - "), "<br>", sep="")
                         cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
                    }
                    if(m==1)primeline2 <- "&nbsp;+ &nbsp;"
                    lineno <- lineno + 1
               }      #  m
               cat("</p>",file=parseguidesym, sep=linereturn, append=TRUE)
               #
               ###########################################
               # Do the same as above for Minus products #
               # First, convert xx to Minus products     #
               ###########################################
               xx[c(p-2,p-1),] <- xx[c(p-1,p-2),]

               symprod <- prod(3:(p-1))
               basemat <- matrix(2:p, nrow=p-1, ncol=2, byrow=FALSE)
               #
               ############################################################ 
               # Make a list with (p-1)!/2 levels, px2 matrices out of x  #
               # Convert unstructured matrix to symmetric matrix          #
               # At each level, reverse to upper triangle matrix          #
               ############################################################
               yy <- vector("list", symprod)
               for(m in 1:nlinesets2){
                    yym <- basemat
                    yym[,2] <- xx[,m]
                    yym <- cbind(yym,1)
                    for(q in 1:(p-1)){
                         if(yym[q,1] > yym[q,2]){
                              yymtemp <- yym[q,1]
                              yym[q,1] <- yym[q,2]
                              yym[q,2] <- yymtemp
                         }
                    }
                    ########################################################
                    # Order yym by column 2 within column 1                #
                    # and determine coefficients after appending first row #
                    ########################################################
                    yym <- yym[order(yym[,1],yym[,2]),]
                    yym <- rbind(1,yym)
                    yym[1,2] <- jj

                    zzm <- yym[-1,]
                    for(cc in 1:(p-1)){
                         if(yym[cc,1]==zzm[cc,1] & yym[cc,2]==zzm[cc,2]){
                              yym[cc,3] <- 2
                              yym[cc+1,3] <- 0
                         }
                    }
                    yy[[m]] <- yym

               }   #   m
               ###################################
               # Set index for multiple products #
               ###################################
               coeffs <- rep(1,nlinesets2) 
               for(m in 1:(nlinesets2-1)){
                    A <- yy[[m]][,1:2]
                    for(mm in (m+1):nlinesets2){
                         B <- yy[[mm]][,1:2]
                         uu <- A==B
                         if(all(c(uu))){
                              coeffs[m] <- coeffs[m] + 1
                              coeffs[mm] <- 0
                         }
                    }    #   mm
               }    #   m
               TotalMinus <- symprod - sum(coeffs==0)
               SingleMinus <- sum(coeffs==1)
               MultipleMinus <- sum(coeffs>1)
               #
               ################################################################
               # Set up HTML file and print Minus half of symbolic determinant #
               ################################################################
               cat("<p>",file=parseguidesym, sep=linereturn, append=TRUE)

               primeline2 <- "&nbsp;-&nbsp;"
               for(m in 1:nlinesets2){
                    #############################################################
                    # Add coefficient if needed and skip any with coefficient 0 #
                    #############################################################
                    if(coeffs[m]!=0){
                         ww <- yy[[m]]
                         ww <- my.html.product(ww)  
                         if(coeffs[m] > 1){
                              ww <- paste(coeffs[m],"&nbsp;", ww)
                         }
                         if(coeffs[m] <= 1){
                              ww <- paste("&nbsp;&nbsp;&nbsp;",ww)
                         } 
                         ww <- paste(primeline2, ww, "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;",  paste(R1Cx, lineno, sep=" - "), "<br>", sep="")
                         cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
                    }
                    lineno <- lineno + 1
               }      #  m
               cat("</p>",file=parseguidesym, sep=linereturn, append=TRUE)
               #
               #############################
               # Output summary statistics #
               #############################
               cat("<p>", file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("Total unique Plus products:", TotalPlus, "<br>",sep=" ")
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("&nbsp;&nbsp;&nbsp;Single:", SinglePlus, "<br>",sep=" ")
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("&nbsp;&nbsp;&nbsp;Multiple:", MultiplePlus, "<br>",sep=" ")               
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               cat("</p>",file=parseguidesym, sep=linereturn, append=TRUE)
              
               cat("<p>", file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("Total unique Minus products:", TotalMinus, "<br>",sep=" ")
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("&nbsp;&nbsp;&nbsp;Single:", SingleMinus, "<br>",sep=" ")
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               ww <- paste("&nbsp;&nbsp;&nbsp;Multiple:", MultipleMinus, "<br>",sep=" ")               
               cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
               cat("</p>",file=parseguidesym, sep=linereturn, append=TRUE)

               cat("</body>", file=parseguidesym, sep=linereturn, append=TRUE)
               cat("</html>", file=parseguidesym, sep=linereturn, append=TRUE)
             }   # if any
          }   #  for jj
     }   # symmetric 
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running parsemini", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
