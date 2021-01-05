    my.html.element <- function(i, j, k=NULL, p)
     {
          # VALUE      Symbolic representation of element of matrix, possibly raised to power
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