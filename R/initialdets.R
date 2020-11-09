#' @export
initialdets <-
function(storage=tempdir(), diagnose=FALSE, verbose=TRUE)
{
     #                          initialdets
     #
     # VALUE     Creates det.guide.p for p = 2 thru 5. Outputs list containing max.created (number 
     #                of largest detguide created and each of the detguides 2-5.  Also, creates
     #                subdirectory and each sub-subdirectory 2, 3, 4 and 5 for storage.
     #                max.created is also stored.
     #
     # INPUT    storage      Name of directory for storage of detguides. tempdir() causes loss of detguides created in this session.
     #                       Recommend to select a different name. This function will create new directory if it doesn't already exist.
     #                       Output includes name of storage directory for use in other functions of this package, notably parsedetguide()
     #
     #          diagnose     Logical. T causes printing of diagnostic content
     #          verbose      Logical. T causes printing of program ID before and after running.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running initialdets", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     outlist <- list(max.created=0, det.guide.2=0, det.guide.3=0, det.guide.4=0, det.guide.5=0, storage=storage) 
     #
     ############################
     # Start with 2x2 det guide #
     ############################
     print("********************************************", quote=FALSE)
     print("Preparing det.guide.2", quote=FALSE)
     print("", quote = FALSE)
     det.guide.2 <- vector("list",2)
     det.guide.2[[1]] <- vector("list",1)
     det.guide.2[[2]] <- vector("list",1)
     Plus <- matrix(c(1,1,2,2),2,2,byrow=TRUE)
     Minus <- matrix(c(1,2,2,1),2,2,byrow=TRUE)
     det.guide.2[[1]][[1]] <- Plus
     det.guide.2[[2]][[1]] <- Minus
     det.location <- paste(storage, 2, sep="/")
     dir.create(det.location, recursive=TRUE)
     dump(list="det.guide.2", file=paste(det.location,"detguide.txt",sep="/"))
     max.created <- 2
     outlist[[1]] <- max.created
     outlist[[2]] <- det.guide.2
     #
     ################################################
     # Iteratively create det.guide.p for p=3, 4, 5 #
     # Make the pxp matrix in det.guide format      #
     ################################################
     for(p in 3:5){
          print("********************************************", quote=FALSE)
          print(paste("Preparing det.guide.",p,sep=""), quote=FALSE)
          print("", quote = FALSE)

          mi <- rep(1:p,each=p)
          mj <- rep(1:p,times=p)
          xp <- as.matrix(data.frame(mi,mj))
          guidename <- paste("det.guide.",p-1,sep="")
          guidename <- paste(storage, p-1, "detguide.txt", sep="/")
          topguide <- source(guidename)[[1]]
          ###################################################
          # reverse structure of topguide to [[nlevels  [[2 #
          ###################################################
          nlevels <- length(topguide[[1]])
          uu <- vector("list",nlevels)

          for(m in 1:nlevels){
               uu[[m]] <- vector("list",2)
               for(r in 1:2){
                    uu[[m]][[r]] <- topguide[[r]][[m]]
               }   #  r
          }  # m
          topguide <- uu
                     if(diagnose){Hmisc::prn(guidename)
                         Hmisc::prn(topguide) 
                     }
          ####################################################
          # xpsmall is the structure of a matrix of size p-1 #
          ####################################################
          mi <- rep(1:(p-1),each=p-1)
          mj <- rep(1:(p-1),times=p-1)
          xpsmall <- as.matrix(data.frame(mi,mj))
          #

          ###############################################################
          # det.guides for p > 2 have inner levels when created, but    #
          # will be restructured here to have 1 outer and 1 inner level #       
          # tempout is [[p [[2 ;                                        #
          # out is [[nlevels [[p [[2                                    #
          # out2 is [[pfactorial/2 [[2                                  #
          ###############################################################
          nlevels <- length(topguide)
                       if(diagnose)Hmisc::prn(nlevels)
          out <- vector("list",nlevels)
          for(m in 1:nlevels){
               guide <- topguide[[m]]
               #########################################################################
               # Form the cofactor of each term in sequence depending on p even or odd #                                                 #
               #########################################################################
               basematrix <- xp[xp[,1]!=1,]
               pm1 <- p - 1
               tempout <- vector("list",2)
               tempout[[1]] <- vector("list",p)
               tempout[[2]] <- vector("list",p)
               for(j in 1:p){
                    uu <- 2*trunc(j/2)
                    amatrix <- basematrix[basematrix[,2]!=j,]   
               #######################################################################
               # amatrix is still in format of det.guide.                            #
               # Match rows of amatrix to those of guide to determine Plus and Minus #
               # Reorder rows of amatrix into 2 groups: Plus and Minus               #
               #######################################################################
                            if(diagnose)Hmisc::prn(amatrix)
                    dim1 <- dim(amatrix)[1]
                    dimg2 <- dim(guide[[1]])[1]
                    PlusMinus <- rep(0,dim1)
                    index2 <- rep(FALSE,dim1)
                    index3 <- rep(FALSE,dimg2)
                    for(ti in 1:dim1){
                         testrow <- xpsmall[ti,]
                         for(tj in 1:dimg2){
                              index3[tj] <- testrow[1]==guide[[1]][tj,1] & testrow[2]==guide[[1]][tj,2]
                              index2[ti] <- any(index3)
                         }
                    }
                    PlusMinus[index2] <- 1
                    bmatrix1 <- cbind(amatrix,PlusMinus)
                    index2 <- rep(FALSE,dim1)
                    index3 <- rep(FALSE,dimg2)
                    for(ti in 1:dim1){
                         testrow <- xpsmall[ti,]
                         for(tj in 1:dimg2){
                              index3[tj] <- testrow[1]==guide[[2]][tj,1] & testrow[2]==guide[[2]][tj,2]
                              index2[ti] <- any(index3)
                         }
                    }
                    PlusMinus[index2] <- -1
                    bmatrix2 <- cbind(amatrix,PlusMinus)
                            if(diagnose)Hmisc::prn(bmatrix1)
                    outjPlus <- bmatrix1[bmatrix1[,3]==1,] 
                    outjMinus <- bmatrix2[bmatrix2[,3]==-1,]
                    outjPlus <- outjPlus[,1:2]
                    outjMinus <- outjMinus[,1:2]
                            if(diagnose){Hmisc::prn(j)
                                Hmisc::prn(xp)
                            }
                    Di <- rbind(c(1,j),outjPlus)
                    Ei <- rbind(c(1,j),outjMinus)
                    if(uu!=j){
                         tempout[[j]] <- list(Di,Ei)
                    }else{
                         tempout[[j]] <- list(Ei,Di)
                    }
               }   #for j          
                              if(diagnose){Hmisc::prn(tempout)
                                     Hmisc::prn(out)
                                     Hmisc::prn(m)
                              }
               out[[m]] <- tempout
          }   #for m 1 to nlevels
          ################################################################################
          # Collapse the internal structure of the list out into a list with p!/2 levels #
          ################################################################################
          pfactorial <- prod(1:p)
          out2 <- vector("list",pfactorial/2)
          uu <- vector("list",2)
          jj <- 1
          for(i in 1:p){
               for(j in 1:nlevels){
                    uu[[1]] <- out[[j]][[i]][[1]]
                    uu[[2]] <- out[[j]][[i]][[2]]
                    out2[[jj]] <- uu
                    jj <- jj + 1
               }
          }
          ###############################################
          # Reverse out2 to latest format:[[2 [[nlevels #
          ###############################################
          nlevels <- length(out2)
          uu <- vector("list",2)
          for(m in 1:2){
               uu[[m]] <- vector("list",nlevels)
               for(r in 1:nlevels){
                    uu[[m]][[r]] <- out2[[r]][[m]]
               }   #  r
          }  # m
          out2 <- uu
          #
          ###########################
          # Save latest det.guide.p #
          ###########################
          max.created <- p
          outlist[[1]] <- max.created
          outlist[[p]] <- out2

          storemax <- paste(storage, "max.created.txt", sep="/")
          dump(list="max.created", file=storemax)

          det.location <- paste(storage, p, sep="/")
          dir.create(det.location, recursive=TRUE)
          det.location <- paste(det.location,"detguide.txt",sep="/")
          dump(list="out2", file=det.location)
     }      # for p
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running initialdets", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
     outlist
}
