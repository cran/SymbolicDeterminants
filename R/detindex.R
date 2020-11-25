#' @export
detindex <-
function(storage, verbose=TRUE)
{
     #                          detindex
     #
     # VALUE    List containing index of detguides and related files in storage directory
     #
     # INPUT    storage     Quoted name of directory for storage of detguides. 
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
     ndirs <- length(out)
     detguide <- parseguide <- parseguidesym <- rep(0,ndirs)
     subdir <- paste(storage,out,sep="/")
     for(i in 1:ndirs){
          filenames <- dir(path=subdir[i])
          if(any(filenames=="detguide.txt"))     detguide[i] <- 1

          if(any(filenames=="parseguide.txt"))   parseguide[i] <- 1
          if(any(filenames=="parseguidesym.txt"))parseguidesym[i] <- 1
     }
     index=data.frame(p=out, detguide, parseguide, parseguidesym)
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running detindex", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("1 indicates file is present, 0 not present", quote=FALSE)
     }
     list(index, Call=MC)
}
