parsedetguide <-
function(p, symmetric=FALSE, storage=tempdir(), diagnose=FALSE, verbose=TRUE)
{
     #                          parsedetguide
     #
     # VALUE       Symbolic representation of the determinant of a pxp matrix made from det.guide.p.  det.guide.p is created 
     #             from initialize( ) function for p <= 5 and from makedetguide( ) function otherwise.
     #
     # INPUT    p            Size of square matrix (p x p) for which determinant is needed.
     #          symmetric    TRUE, if determinant for symmetric matrix is wanted.  
     #          storage      Name of directory for storage of detguides. tempdir() causes loss of detguides created in this session.
     #                       Recommend to select a different name. This function will create it if it doesn't already exist
     #
     #          diagnose     TRUE causes printing of diagnostic content
     #          verbose      TRUE causes printing of program ID before and after running.
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
     #############################################
     # Confirm that the input parameters conform #
     #############################################
     if(p < 2) stop("p must be an integer greater than 1")
     if(floor(p) != p)stop("p must be an integer")

     oldmax <- paste(storage, "max.created.txt", sep="/")
     max.created <- source(oldmax)[[1]]
     if(p > max.created)stop(paste("The largest detguide created so far is for p =", max.created)) 
     if(p > 99) stop("Sorry, but the largest determinant we can print is for p = 99")

     linereturn <- "\n"
     nlinesets <- prod(3:p)
     #
     #######################################################
     # Input det.guide from storage subdirectory           #
     # Run separate routines depending on symmetric or not #
     # mat is the list that defines a determinant          #
     #######################################################
     getguide <- paste(storage, p, "detguide.txt", sep="/")
     mat <- source(getguide)[[1]]
     if(symmetric){
          for(j in 1:2){
               for(m in 1:nlinesets){
                    vv <- mat[[j]][[m]]
                    ##################################################################### 
                    # Ensure that all diagonal terms will print on left of each product #
                    ##################################################################### 
                    vv1 <- vv[,1]
                    vv2 <- vv[,2]
                    vv[vv1==vv2,1] <- -300 + vv[vv1==vv2,1]


                    matrev <- vv[,c(2,1)]
                    index <- vv[,1] > vv[,2]
                    vv[index,] <- matrev[index,]
                    vv <- cbind(vv,1)
                    vv <- vv[order(vv[,1],vv[,2]),]

                    # Change exponent to 2 for quadratic term; eliminate reduntant term with exponent 0 #
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
     }  #symmetric
     else{
          for(m in 1:nlinesets){
               for(j in 1:2){
                    vv <- mat[[j]][[m]]
                    vv <- vv[order(vv[,1],vv[,2]),]
                    mat[[j]][[m]] <- vv
                }  # j 
           }    # m
     }    # not symmetric
     ###############################################
     # Set printing of linesets into 3 or 4 lines, #
     # depending on whether symmetric matrix       #
     # Line blocks depend on value of p            #
     ###############################################
     primeline1 <- "   "
     parseguide <-    paste(storage, p, "parseguide.txt" , sep="/")
     parseguidesym <- paste(storage, p, "parseguidesym.txt",sep="/")
     xx <- NULL
     lineno <- 1
     if(symmetric){
          if(file.exists(parseguidesym)) file.remove(file=parseguidesym)
          dump(xx,parseguidesym)
          primeline2 <- " + "
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
          }
          ##########################
          # Print the output lines #
          ##########################
          for(j in 1:2){
               for(m in 1:nlinesets){
                 if(coeffs[m,j] != 0){
                    augline2 <- " "
                    if(coeffs[m,j]==2)augline2 <- "2"
                    # Restore subscripts #
                    ww <- mat[[j]][[m]]
                    ww1 <- (1:p)[ww[,1]<0]
                    ww[ww1,1] <- 300 + ww[ww1,1]

                    line1 <- "    "
                    line3 <- "    "
                    line2 <- primeline2
                    for(r in 1:p){
                         if(j==1 & m==1 & r==1)line2 <- "   "
                         wwr3 <- ww[r,3]
                         if(wwr3==1) wwr3 <- " "

                         if(p >= 100){
                              if(wwr3 != 0){    # block with 8 spaces
                                 line1 <- paste(line1," ", wwr3, "      ", sep="")
                                   line2 <- paste(line2, "v","       ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2],"    ", sep="")
                              }
                         }    # p   999

                         if(p >= 10 & p <= 99){
                              if(wwr3 != 0){    # block with 6 spaces
                                   line1 <- paste(line1," ", wwr3, "    ", sep="")
                                   line2 <- paste(line2, "v","     ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2],"  ", sep="")
                              }
                         }    #p 99

                         if(p <= 9){
                              if(wwr3 != 0){   # block with 4 spaces
                                   line1 <- paste(line1, " ", wwr3, "  ", sep="")
                                   line2 <- paste(line2, "v","   ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2], sep="")
                              }
                              line2A <- paste(substr(line2,start=1,stop=3),augline2,substr(line2,start=4,stop=100000),sep="")
                         }     # p 9
                    }   # r
                    ####################################
                    # output allows space for exponent #
                    ####################################
                    line2A <- paste(line2A, "                       ", lineno)
                    cat(c(line1,line2A,line3),sep=linereturn)
                    cat(c(" "," "),sep=linereturn)
 
                    cat(c(line1,line2A,line3),file=parseguidesym,sep=linereturn,append=TRUE)
                    cat(c(" "," "),file=parseguidesym,sep=linereturn,append=TRUE)
                 }  #  coeffs
                    lineno <- lineno + 1
              }     # m
               primeline2 <- " - "
          }     # j 
     }    # symmetric
     else{        # not symmetric
          if(file.exists(parseguide)) file.remove(file=parseguide)
          dump(xx,parseguide)
          primeline2 <- " + "
          for(j in 1:2){
               for(m in 1:nlinesets){
                    ww <- mat[[j]][[m]]
                    line1 <- line3 <- "   "
                    line2 <- primeline2
                    for(r in 1:p){
                         if(j==1 & m==1 & r==1)line2 <- "   "

                         if(p >= 100){
                                   line2 <- paste(line2, "v","       ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2],"    ", sep="")
                         }    # p   999

                         if(p >= 10 & p <= 99){
                                   line2 <- paste(line2, "v","     ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2],"  ", sep="")
                         }    #p 99

                         if(p <= 9){
                                   line2 <- paste(line2, "v","   ", sep="")
                                   line3 <- paste(line3, " ", ww[r,1], ",", ww[r,2], sep="")
                         }     # p 9
                    }   # r
                    line2 <- paste(line2, "                       ", lineno)
                    cat(c(line2,line3),sep=linereturn)
                    cat(c(" "," "),sep=linereturn)
 
                    cat(c(line2,line3),file=parseguide,sep=linereturn,append=TRUE)
                    cat(c(" "," "),file=parseguide,sep=linereturn,append=TRUE)
                    lineno <- lineno + 1
               }     # m
               primeline2 <- " - "
          }     # j 
     }       # not symmetric
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running parsedetguide", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
