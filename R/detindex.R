#' @export
detindex <-
function(storage, mini=c("B","D","M"), pmini=1, verbose=TRUE)
{
     #                          detindex
     #
     # VALUE    List containing index of detguides and related files in storage directory or of one set of minidetguides for p=pmini
     #
     # INPUT    storage     Quoted name of directory for storage of detguides. 
     #          mini        Index includes (B)oth detguides and minidetguides, (D)etguides only, (M)inidetguides only 
     #          pmini       p for minidetguides and related files; ignored if mini="D"
     #
     #          verbose     Logical. T causes printing of program ID before and after running.
     #
     # DETAILS  Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
     #              If storage directory is in same folder as R Workspace, storage=".\\name" is sufficient.
     #  
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running detindex", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     if(!file.exists(storage)) stop("Incorrect path to storage directory")

     out<- dir(path=storage)
     out <- out[out != "max.created.txt"]     
     out <- sort(as.numeric(out))
     ndirs <- length(out)


     if(mini=="B" | mini=="D"){
          detguide <- parseguide <- parseguidesym <- rep(0,ndirs)

          subdir <- paste(storage,out,sep="/")
          for(i in 1:ndirs){
               filenames <- dir(path=subdir[i])
               if(any(filenames=="detguide.txt"))     detguide[i] <- 1
               if(any(filenames=="parseguide.txt"))   parseguide[i] <- 1
               if(any(filenames=="parseguidesym.txt"))parseguidesym[i] <- 1
          }
          index=data.frame(p=out, detguide, parseguide, parseguidesym)
     }    # mini==B or D
     #
     if(mini=="B" | mini=="M"){
          storage <- paste(storage,pmini, sep="/")
          if(!file.exists(storage)) stop("No storage for this value of pmini")
          out<- dir(path=storage)
          ndirs <- length(out)

          minidetguide <- parseguide <- parseguidesym <- rep(0,ndirs)

          subdir <- paste(storage,out,sep="/")
          for(i in 1:ndirs){
               filenames <- dir(path=subdir[i])
               if(any(filenames=="minidetguide.txt")) minidetguide[i] <- 1
               if(any(filenames=="parseguide.htm"))   parseguide[i] <- 1
               if(any(filenames=="parseguidesym.htm"))parseguidesym[i] <- 1
          }
          index2=data.frame(R1Cs=out, minidetguide, parseguide, parseguidesym)
     }    #   mini=="B" or M
     #
     if(mini=="D")outlast <- list(Detguides=index, Call=MC) 
     if(mini=="M")outlast <- list(Minidetguides=index2, Call=MC) 
     if(mini=="B")outlast <- list(Detguides=index, Minidetguides=index2, Call=MC) 
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running detindex", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("1 indicates file is present, 0 not present", quote=FALSE)
     }
     outlast
}
