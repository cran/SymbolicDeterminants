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