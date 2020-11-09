makedetguide <-
function(p, storage=tempdir(), diagnose=FALSE, verbose=TRUE)
{
     #                          makedetguide
     #
     # VALUE     Creates det.guide.p for all p from max.created to p in input variable
     #
     # INPUT    p            Matrix size (pxp) for wanted symbolic representation of determinant
     #          storage      Name of directory for storage of detguides. tempdir() causes loss of detguides created in this session.
     #                       Recommend to select a different name. This function will create it if it doesn't already exist
     #
     #          diagnose     Logical. T causes printing of diagnostic content
     #          verbose      Logical. T causes printing of program ID before and after running.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running makedetguide", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     oldmax <- paste(storage, "max.created.txt", sep="/")
     max.created <- source(oldmax)[[1]]
     if(p <= max.created)stop("Requested detguide p is less than or equal to max.created")
     #
     ##################################################
     # Determine sequence of det.guides to be created #
     # and set up run                                 #
     ##################################################
     for(pi in (max.created+1):p){
          print("********************************************", quote=FALSE)
          print(paste("Preparing det.guide.",pi,sep=""), quote=FALSE)
          print("", quote = FALSE)

          mi <- rep(1:pi,each=pi)
          mj <- rep(1:pi,times=pi)
          xp <- as.matrix(data.frame(mi,mj))
          guidename <- paste(storage, pi-1, "detguide.txt", sep="/")
          topguide <- source(guidename)[[1]]
                     if(diagnose){Hmisc::prn(guidename)
                          Hmisc::prn(length(topguide[[1]])) 
                     }
          #####################################################
          # Reverse structure of topguide to [[nlevels   [[2  #
          #####################################################
          nlevels <- length(topguide[[1]])
                       if(diagnose)Hmisc::prn(nlevels)
          uu <- vector("list",nlevels)
          for(m in 1:nlevels){
               uu[[m]] <- vector("list",2)
               for(r in 1:2){
                    uu[[m]][[r]] <- topguide[[r]][[m]]
               }   #  r
          }  #   m
          topguide <- uu
                        if(diagnose){Hmisc::prn(guidename)
                        }
          #####################################################
          # xpsmall is the structure of a matrix of size pi-1 #
          #####################################################
          mi <- rep(1:(pi-1),each=pi-1)
          mj <- rep(1:(pi-1),times=pi-1)
          xpsmall <- as.matrix(data.frame(mi,mj))
          ###############################################################
          # det.guides for p > 2 have inner levels when created, but    #
          # will be restructured here to have 1 outer and 1 inner level #       
          # tempout is [[p [[2 ;                                        #
          # out is [[nlevels [[p [[2                                    #
          # out2 is [[pfactorial/2 [[2                                  #
          ###############################################################
            nlevels <- length(topguide)
            out <- vector("list",nlevels)
            for(m in 1:nlevels){
               guide <- topguide[[m]]
               #########################################################################
               # Form the cofactor of each term in sequence depending on p even or odd #                                                 
               #########################################################################
               basematrix <- xp[xp[,1]!=1,]
               pm1 <- pi - 1
               tempout <- vector("list",pi)
               for(j in 1:pi){
                    uu <- 2*trunc(j/2)
                    amatrix <- basematrix[basematrix[,2]!=j,]   
                    #######################################################################
                    # amatrix is still in format of det.guide.                            #
                    # Match rows of amatrix to those of guide to determine Plus and Minus #
                    # Reorder rows of amatrix into 2 groups: Plus and Minus               #
                    #######################################################################
#                                   if(diagnose)Hmisc::prn(amatrix)
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
                            if(diagnose)Hmisc::prn(bmatrix1)
                    index2 <- rep(FALSE,dim1)
                    index3 <- rep(FALSE,dimg2)
                    for(ti in 1:dim1){
                         testrow <- xpsmall[ti,]
                         for(tj in 1:dimg2){
                              index3[tj] <- testrow[1]==guide[[2]][tj,1] & testrow[2]==guide[[2]][tj,2]
                              index2[ti] <- any(index3)
                         }   # tj
                    }   # ti
                    PlusMinus[index2] <- -1
                    bmatrix2 <- cbind(amatrix,PlusMinus)
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
                                     Hmisc::prn(m)
                              }
               out[[m]] <- tempout
                              if(diagnose){
                                  headout <- utils::head(out)
                                  Hmisc::prn(headout)
                              }
          }   #  for m 1 to nlevels
          #
          #################################################################################
          # Collapse the internal structure of the list out into a list with pi!/2 levels #
          #################################################################################
          pfactorial <- prod(1:pi)
          out2 <- vector("list",pfactorial/2)
          uu <- vector("list",2)
          jj <- 1
          for(i in 1:pi){
               for(j in 1:nlevels){
                    uu[[1]] <- out[[j]][[i]][[1]]
                    uu[[2]] <- out[[j]][[i]][[2]]
                    out2[[jj]] <- uu
                    jj <- jj + 1
               }   # j
          }   #  i
                               if(diagnose){
                                      headout2 <- utils::head(out2)
                                      Hmisc::prn(out2)
                               }
         #
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
          max.created <- pi
          dumpfile <- paste(storage, "max.created.txt", sep="/")
          dump(list="max.created",file=dumpfile)

          det.location <- paste(storage, pi, sep="/")
          dir.create(det.location, recursive=TRUE)
          det.location <- paste(det.location,"detguide.txt",sep="/")
          dump(list="out2", file=det.location)
     }      # for p
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running makedetguide", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
}
