#' @export
parsedetguide <-
function(p, storage, browser="C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", symmetric=FALSE, verbose=TRUE)
{
     #                          parsedetguide
     #
     # VALUE       Symbolic representation of the determinant of a pxp matrix made from detguide.  detguide for p is created 
     #             by anewdetguide( ) function.
     #
     # INPUT    p            Size of square matrix (p x p) for which determinant is wanted. (p > 2)
     #          storage      Quoted name of directory for storage of detguides (ex. "c:/determinants") e" is sufficient.
     #          browser      OPTIONAL. Non-empty character string giving the name of the program to be used as the URL browser. Will
     #                            be set interactively.
     #          symmetric    TRUE, if representation of determinant for symmetric matrix is wanted.  
     #
     #          verbose     Logical. T causes printing of program ID before and after running.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running parsedetguide", quote = FALSE) 
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     #########################################################
     # Support functions for printing of element and product #
     #########################################################
     my.html.element <- function(i, j, k=NULL, p, verbose=FALSE)
     {
          # VALUE      Mathematical expression of element of matrix V, possibly raised to power
          #
          # INPUT      i          row
          #            j          column
          #            k          exponent
          #            p          size of matrix (pxp)
          #
          # NOTE:  This is a support function used in parsing detguides.
          #        If p > 9, subscripts will contain a comma to separate row from column.
          #        </title down to <body>  and  </body> to </html> are written to file outside of this function. 
          #
          if(p < 10){
               out <- paste("v<sup>", k, "</sup><sub>", i, j, "</sub>", sep="")
               if(is.null(k))out <- paste("v<sub>", i, j, "</sub>", sep="")
          } else{
               out <- paste("v<sup>", k, "</sup><sub>", i,",", j, "</sub>", sep="")
               if(is.null(k))out <- paste("v<sub>", i, ",",j, "</sub>", sep="")
          }
          out <- paste(out,"&nbsp; ",sep="")
     out
     }
     # 
     #
     my.html.product <- function(x)
     {
          #
          # VALUE      HTML code for the product of matrx elements that constitute a line of the determinant.
          #
          # INPUT    x   Matrix from a detguide.  x is px2 or px3, depending on whether there is no exponent or there is
          dimx <- dim(x)
          p <- dimx[1]
          out <- NULL
          for(n in 1:p){
               if(dimx[2]==2){
               out <- paste(out, my.html.element(i=x[n,1], j=x[n,2], p=p), sep="") 
               }else{
                    if(x[n,3] > 0){ 
                         # skip element if exponent = 0 and leave exponent blank if exponent = 1
                         if( x[n,3]==1){
                              out <- paste(out, my.html.element(i=x[n,1], j=x[n,2], k=NULL, p=p), sep="") 
                         }else{
                              out <- paste(out, my.html.element(i=x[n,1], j=x[n,2], k=x[n,3], p=p), sep="") 
                         }
                    }  #  if
               }       #  else
          }            #  for 1:p
          out
     }
     #########################
     # End support functions #
     #########################

     #############################################
     # Main program starts here                  #
     # Confirm that the input parameters conform #
     #############################################
     if(p < 3) stop("p must be an integer greater than 2")
     if(floor(p) != p)stop("p must be an integer")

     oldmax <- paste(storage, "max.created.txt", sep="/")
     max.created <- source(oldmax)[[1]]
     if(p > max.created)stop(paste("The largest detguide created so far is for p =", max.created)) 

     linereturn <- "\n"
     nlinesets <- prod(3:p)    # p! /2
     #
     ################################################
     # Set up browser for various operating systems #
     ################################################
     print("", quote = FALSE)
     print("Let's identify the path to the HTML browser for this computer. ", quote=FALSE)
     print("", quote = FALSE)
     questions <- paste( 
                   "     1. Using a Windows operating system. Let this function supply the path",
                   "     2. Not using Windows; I entered the path to the HTML browser when I called this function",
                   "     3. Not using Windows; I want to type in the complete path to the HTML browser now",
                   "     4. Abort this function; I will go look up the complete path to the HTML browser                  ",
                   "            ",
                   sep = "\n")
     cat(questions, file="")
     instruction <- "Enter a number from 1 to 4:  "
     cat(" ", file="")
     jj <- substring(readline(prompt=instruction),1,1)
     if(jj != 1 & jj != 2 & jj != 3)stop("Aborting function")
     if(jj == 1) browser <- NULL
     if(jj == 2) xnull <- NULL    # no action necessary
     if(jj == 3){
          cat(" ", file="")
          instruction <- "Type the path. Omit quotes:   "
          browser <- readline(prompt=instruction)
          cat(" ", file="")
     }    # jj=3 
     #
     #######################################################
     # Input detguide from storage subdirectory            #
     # Run separate routines depending on symmetric or not #
     # mat is the list that defines a determinant          #
     #######################################################
     getguide <- paste(storage, p, "detguide.txt", sep="/")
     mat <- source(getguide)[[1]]
     if(symmetric){
          for(j in 1:2){
               for(m in 1:nlinesets){
                    vv <- mat[[j]][[m]]
                    ################################################### 
                    # Convert unstructured matrix to symmetric matrix #
                    ###################################################
                    vv1 <- vv[,1]
                    vv2 <- vv[,2]
                    matrev <- vv[,c(2,1)]
                    index <- vv[,1] > vv[,2]
                    vv[index,] <- matrev[index,]
                    vv <- cbind(vv,1)                       # add third column here
                    vv <- vv[order(vv[,1],vv[,2]),]         # lexicographic order by column within row
                    #
                    #####################################################################################
                    # Change exponent to 2 for quadratic term; eliminate reduntant term with exponent 0 #
                    #####################################################################################
                    if(vv[1,1]==vv[2,1] & vv[1,2]==vv[2,2])vv[1:2,3]<- c(2,0)
                    mat3 <- vv[-1,]
                    mat4 <- vv[-dim(vv)[1],]
                    index2 <- mat3[,1]==mat4[,1] & mat3[,2]==mat4[,2]
                    index2 <- (1:(p-1))[index2]
                    if(sum(index2)>0){
                       vv[index2,3] <- 2
                       vv[index2+1,3] <- 0
                    }
                    mat[[j]][[m]] <- vv
                }  # m 
           }    # j
           ################################################################
           # Add a fourth column to count the number of quadratic terms   #
           # Only the first element of this column is used to order terms #
           ################################################################
           for(j in 1:2){
                for(m in 1:nlinesets){
                     uu <- mat[[j]][[m]]
                     uu <- cbind(uu,0)
                     uu[1,4] <- sum(uu[,3]==2)                      
                     mat[[j]][[m]] <- uu
                }     # m
           }        #  j
     }else{
         for(m in 1:nlinesets){
               for(j in 1:2){
                    vv <- mat[[j]][[m]]
                    vv <- vv[order(vv[,1],vv[,2]),]
                    mat[[j]][[m]] <- vv
                }  # j 
           }    # m
     }    # not symmetric
     #
     ##################################
     # Set up each line for printing  #
     ##################################
     primeline1 <- "   "
     lineno <- 1
     if(symmetric){
          primeline2 <- "&nbsp;+&nbsp;"
          ###############################################
          # Order by number of quadratics, then by i, j #
          ###############################################
          fororder <- 1:nlinesets
          fororder1 <- fororder2 <-cbind(fororder,0)
               for(m in 1:nlinesets){
                    ww <- mat[[1]][[m]]
                    fororder1[m,2] <- ww[1,4]
                    ww <- mat[[2]][[m]]
                    fororder2[m,2] <- ww[1,4]
               }   # m
          fororder1<- fororder1[order(fororder1[,2]),]
          fororder2<- fororder2[order(fororder2[,2]),]
          zz <- vector("list",2)
          zz[[1]] <- vector("list",nlinesets)
          zz[[2]] <- vector("list",nlinesets)
          for(m in 1:nlinesets){
               zz[[1]][[m]] <- mat[[1]][[fororder1[m,1]]]
               zz[[2]][[m]] <- mat[[2]][[fororder2[m,1]]]
          }   # m
          for(j in 1:2){
               for(m in 1:nlinesets){
                    matnew <- zz[[j]][[m]]
                    matnew <- matnew[order(matnew[,1],matnew[,2]),]
                    mat[[j]][[m]] <- matnew
               }       # m
          }   # j
          ######################################################
          # Determine where lines are duplicated and mark them #
          ######################################################
          uu1 <- uu2 <- matrix(0,nrow=nlinesets, ncol=2*p)
          for(m in 1:nlinesets){
               uu1[m,] <- c(mat[[1]][[m]][,c(1,2)])
               uu2[m,] <- c(mat[[2]][[m]][,c(1,2)])
          }  # m
          ww1 <- apply(uu1,1,FUN=paste,sep=",", collapse=",")
          vv1 <- ww1
          ww1 <- ww1[-nlinesets]
          vv1 <- vv1[-1]
          ww1 <- c(ww1==vv1, FALSE)

          ww2 <- apply(uu2,1,FUN=paste,sep=",", collapse=",")
          vv2 <- ww2
          ww2 <- ww2[-nlinesets]
          vv2 <- vv2[-1]
          ww2 <- c(ww2==vv2, FALSE)
          coeffs <- matrix(-1, nrow=nlinesets, ncol=2)
          for(j in 1:(nlinesets-1)){
               if(ww1[j]){
                    coeffs[j,1] <- 2
                    coeffs[j+1,1] <- 0
               }
               if(ww2[j]){
                    coeffs[j,2] <- 2
                    coeffs[j+1,2] <- 0
               }
          }    #j
          for(j in 1:2){
               for(m in 1:nlinesets){
                    if(coeffs[m,j] != 0){
                         augline2 <- " "
                         if(coeffs[m,j]==2)augline2 <- "2"
                    }    #  coeffs
               }         # m
          }              # j
          # No corresponding section for non symmetric matrix
     }
     ###################################################
     # Set up HTML file and print symbolic determinant #
     ###################################################
     parseguide <-    paste(storage, p, "parseguide.htm" , sep="/")
     parseguidesym <- paste(storage, p, "parseguidesym.htm",sep="/")

     xx <- '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
     lineno <- 1
     if(symmetric){
          xxhead <- paste("Symbolic Representation of the Determinant of a Real, Symmetric ", p, "x", p, " Matrix", sep="")
          if(file.exists(parseguidesym)) file.remove(file=parseguidesym)
          file.create(parseguidesym)
          cat(xx,file=parseguidesym, sep=linereturn, append=TRUE)

          cat("<html>", file=parseguidesym, sep=linereturn, append=TRUE)

          cat("<head>", file=parseguidesym, sep=linereturn, append=TRUE)
          cat(xxhead, file=parseguidesym, sep=linereturn, append=TRUE)
          cat("</head>",file=parseguidesym, sep=linereturn, append=TRUE)

          cat("<body>", file=parseguidesym, sep=linereturn, append=TRUE)

          primeline2 <- "&nbsp;&nbsp;&nbsp;&nbsp;"
          for(j in 1:2){
               cat("<p>",file=parseguidesym, sep=linereturn, append=TRUE)
               for(m in 1:nlinesets){
                    ################################################################
                    # Add coefficient 2, if needed and skip any with coefficient 0 #
                    ################################################################
                    if(coeffs[m,j]!=0){
                         ww <- mat[[j]][[m]]
                         ww <- my.html.product(ww)  
                         if(coeffs[m,j]==2){
                              ww <- paste("2&nbsp;", ww)
                         }else{
                              ww <= paste("&nbsp;&nbsp;&nbsp;",ww)
                         } 
                         ww <- paste(primeline2, ww, "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", lineno, "<br>", sep="")
                         cat(ww, file=parseguidesym, sep=linereturn, append=TRUE)
                    }
                    if(j==1)primeline2 <- "&nbsp;+&nbsp;"
                    lineno <- lineno + 1
               }      #  m
               primeline2 <- "&nbsp;-&nbsp;"
               cat("</p>",file=parseguidesym, sep=linereturn, append=TRUE)
          }           #  j
          cat("</body>", file=parseguidesym, sep=linereturn, append=TRUE)
          cat("</html>", file=parseguidesym, sep=linereturn, append=TRUE)
          utils::browseURL(parseguidesym, browser = browser)
     }else{
          xxhead <- paste("Symbolic Representation of the Determinant of a Real, ", p, "x", p, " Matrix", sep="")
          if(file.exists(parseguide)) file.remove(file=parseguide)
          file.create(parseguide)
          cat(xx,file=parseguide, sep=linereturn, append=TRUE)

          cat("<html>", file=parseguide, sep=linereturn, append=TRUE)
          cat("<head>", file=parseguide, sep=linereturn, append=TRUE)
          cat(xxhead, file=parseguide, sep=linereturn, append=TRUE)

          cat("</head>",file=parseguide, sep=linereturn, append=TRUE)
          cat("<body>", file=parseguide, sep=linereturn, append=TRUE)

          primeline2 <- "&nbsp;&nbsp;&nbsp;&nbsp;"
          for(j in 1:2){
               cat("<p>",file=parseguide, sep=linereturn, append=TRUE)
               for(m in 1:nlinesets){
                    ww <- mat[[j]][[m]]
                    ww <- my.html.product(ww)  
                    ww <- paste(primeline2, ww, "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", "&nbsp;", lineno, "<br>", sep="")
                    cat(ww, file=parseguide, sep=linereturn, append=TRUE)
                    if(j==1)primeline2 <- "&nbsp;+&nbsp;"
                    lineno <- lineno + 1
               }      #  m
               primeline2 <- "&nbsp;-&nbsp;"
               cat("</p>",file=parseguide, sep=linereturn, append=TRUE)
          }           #  j
          cat("</body>", file=parseguide, sep=linereturn, append=TRUE)
          cat("</html>", file=parseguide, sep=linereturn, append=TRUE)
          utils::browseURL(parseguide, browser = browser)
     }   # not symmetric
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running parsedetguide", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
