#' @export
#' @seealso utils browseURL
retrieve <-
function(p, storage, browser="C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", verbose=TRUE)
{
     #                          retrieve
     #
     # VALUE     Selects and retrieves a detguide file from the storage/p subdirectory
     #              or displays the symbolic determinant in a text editor
     #
     # INPUT     p            Size of matrix (pxp) 
     #           storage      Quoted name of directory for storage of detguides (ex. "c:/determinants"). 
     #           browser      OPTIONAL. Non-empty character string giving the name of the program to be used as the URL browser. Will
     #                            be set interactively.
     #           verbose      Logical. T causes printing of program ID before and after running.
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

     if(!(fs::file_exists(store))){
          message("Storage path incorrect or no storage file exists")
          message(paste("Entered storage path was", store, sep=" "))
     }
     #################################
     # Prepare interactive questions #
     #################################
     print("", quote = FALSE)
     print("Enter the number for the file you want:", quote=FALSE)
     print("", quote = FALSE)
     questions <- paste(
                   "      1. detguide text file",
                   "      2. symbolic representation of unstructured matrix",
                   "      3. symbolic representation of symmetric matrix",
                   "      0. CANCEL            ",
                   "            ",
                   sep = "\n")

     jrj <- substring(readline(prompt=questions),1,1)

     out <- ""
     if(jrj != "0"){
          if(jrj=="1"){
               store <- paste(store,"detguide.txt",sep="/")
               out <- source(store)
               out <- out[[1]]
          }     #  jrj = 1
          if(jrj=="2" | jrj == 3){
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
               }          # jj=3 
          }              # jrj = 2 or 3 
          #
          if(jrj=="2"){
               store <- paste(store,"parseguide.htm",sep="/")
               utils::browseURL(store, browser=browser)
          }     # jrj = 2 
          if(jrj=="3"){
               store <- paste(store,"parseguidesym.htm",sep="/")
               utils::browseURL(store, browser=browser)
          }       #  jrj  = 3
     }    #   jrj
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
