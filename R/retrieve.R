#' @export
retrieve <-
function(p, storage, browser="Microsoft Edge", verbose=TRUE)
{
     #                          retrieve
     #
     # VALUE     Selects and retrieves a detguide file from the storage/p subdirectory
     #              or displays the symbolic determinant in a text editor
     #
     # INPUT     p            Size of matrix (pxp) 
     #           storage      Quoted name of directory for storage of detguides. 
     #           browser      Non-empty character string giving the name of the program to be used as the URL browser, for 
     #                            Windows operating system NULL is OK     #
     #           verbose      Logical. T causes printing of program ID before and after running.
     #
     # DETAILS  Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
     #              If storage directory is in same folder as R Workspace, storage=".\\name" is sufficient.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running retrieve", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
          print("Call:", quote = FALSE)
          print(MC)
          print("", quote = FALSE)
     }
     #################################################
     # Concatenate to full path of storage directory #
     #################################################
     store <- paste(storage, p, sep="/")

     if(!(file.exists(store))){
          message("Storage path incorrect or no storage file exists")
          message(paste("Entered storage path was",store,sep=" "))
     }
     #################################
     # Prepare interactive questions #
     #################################
     questions <- paste(
                   "1. detguide",
                   "2. parseguide",
                   "3. parseguidesym",
                   "0. CANCEL            ",
                   sep = "\n")

     print("", quote = FALSE)
     print("Enter the number for the file you want:", quote=FALSE)
     print("", quote = FALSE)
     jj <- substring(readline(prompt=questions),1,1)

     out <- ""
     if(jj != "0"){
          if(jj=="1"){
               store <- paste(store,"detguide.txt",sep="/")
               out <- source(store)
               out <- out[[1]]
          }
          if(jj=="2"){
               store <- paste(store,"parseguide.htm",sep="/")
               utils::browseURL(store, browser=browser)
          }
          if(jj=="3"){
               store <- paste(store,"parseguidesym.htm",sep="/")

               utils::browseURL(store, browser=browser)
          }
     }    #   jj
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running retrieve", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
     out    
}
