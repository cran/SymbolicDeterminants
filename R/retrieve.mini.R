#' @export
#' @seealso utils browseURL
retrieve.mini <-
function(p, storage, browser="C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", verbose=TRUE)
{
     #                          retrieve.mini
     #
     # VALUE     Selects and retrieves a minidetguide file from the storage/p/R1Cx subdirectory
     #              or displays the symbolic determinant in a text editor
     #
     # INPUT     p            Size of matrix (pxp) 
     #           storage      Quoted name of directory for storage of detguides. 
     #           browser      OPTIONAL. Non-empty character string giving the name of the program to be used as the URL browser. Will
     #                            be set interactively.
     #           verbose      Logical. T causes printing of program ID before and after running.
     #
     # DETAILS  Provide full path in storage, using double backslashes.  Example:  storage="c:\\determinants".  
     #              If storage directory is in same folder as R Workspace, storage=".\\name" is sufficient.
     #
     MC <- match.call()
     if(verbose) {
          print("", quote = FALSE)
          print("Running retrieve.mini", quote = FALSE)
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
          stop("Function terminated")
     }
     out <- ""
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
     kk <- substring(readline(prompt=instruction),1,1)
     if(kk != 1 & kk != 2 & kk != 3)stop("Aborting function execution")
     if(kk == 1) browser <- NULL
     if(kk == 2) xnull <- NULL    # no action necessary
     if(kk == 3){
          cat(" ", file="")
          instruction <- "Type the path. Omit quotes:   "
          browser <- readline(prompt=instruction)
          cat(" ", file="")
     }          # kk=3 
     #
     #################################
     # Prepare interactive questions #
     #################################
     print("", quote = FALSE)
     print("Enter the number for the type of file you want, or 0 to exit:", quote=FALSE)
     print("", quote = FALSE)

     questions <- paste(
                   "0. CANCEL            ",
                   "1. sub-minidetguide text file",
                   "2. symbolic representation of unstructured matrix",
                   "3. symbolic representation of symmetric matrix",
                   "                        ",
                   sep = "\n")
     cat(questions, file="")
     instruction <- "Enter a number from 0 to 3:  "
     cat(" ", file="")
     jrj <- substring(readline(prompt=instruction),1,1)
     #
     ############################################
     # Show one of the symbolic representations #
     ############################################
     jk <- 0
     if(jrj > 1){
          questions <- paste(
                "s. Single file for s-th column of determinant (eg, '4' for R1C4:)",
                "A. Files for All columns, in sequence",
                "0. CANCEL                       ",
                "                    ",
                   sep = "\n")

          print("", quote = FALSE)
          print("Enter the number or letter for the range of files you want, or 0 to exit:", quote=FALSE)
          print("", quote = FALSE)
          jk <- substring(readline(prompt=questions),1,1)

          if(jk != "0"){
               if(jk=="s")stop("Enter a number, not the letter s") 
               if(jk=="A"){
                    zzz <- "parseguidesym.htm"
                    if(jrj=="2"){zzz <- "parseguide.htm"}
                    store2 <- NULL
                    for(mm in 1:p){
                         R1Cs <- paste("R1C",mm, sep="")
                         storeR1Cs <- paste(store, R1Cs, zzz, sep="/")
                         store2 <- c(store2, storeR1Cs)
                    }    #  mm
                    for(mm in 1:p) utils::browseURL(url=store2[mm], browser=browser)
               }else{
                    R1Cs <- paste("R1C", jk, sep="") 
                    storeR1Cs <- paste(store, R1Cs, sep="/")

                    if(kk != "0"){
                         if(jrj=="2"){
                              store2 <- paste(storeR1Cs,"parseguide.htm",sep="/")
                              utils::browseURL(store2, browser=browser)
                         }
                         if(jrj=="3"){
                              store2 <- paste(storeR1Cs,"parseguidesym.htm",sep="/")
                              utils::browseURL(store2, browser=browser)
                         }
                    }       #   kk != "0"
               }    #    jk == 1
          }    #  jk != 0
     }    #  jrj > 1
     #
     #######################
     # Show a minidetguide #
     #######################
     if(jrj==1){
          print("", quote = FALSE)
          questions <- "Enter the number for the sub-mini file you want(eg, '4' for R1C4:)"
          print("", quote = FALSE)
          jm <- substring(readline(prompt=questions),1,1)
          R1Cs <- paste("R1C", jm, sep="") 
          storeR1Cs <- paste(store, R1Cs, sep="/")

          out <- ""
          if(jrj != "0"){
               if(jrj=="1"){
                    store2 <- paste(storeR1Cs,"minidetguide.txt",sep="/")
                    out <- source(store2)
                    out <- out[[1]]
               }
               if(jrj=="2"){
                    store2 <- paste(storeR1Cs,"parseguide.htm",sep="/")
                    utils::browseURL(store2, browser=browser)
               }
               if(jrj=="3"){
                    store2 <- paste(storeR1Cs,"parseguidesym.htm",sep="/")
                    utils::browseURL(store2, browser=browser)
               }
          }    #   jrj != "0"
     }    #    jk == 1
     #
     if(verbose) {
          print("", quote = FALSE)
          print("Finished running retrieve.mini", quote = FALSE)
          print("", quote = FALSE)
          print(date(), quote = FALSE)
          print("", quote = FALSE)
     }
     out    
}
