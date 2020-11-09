test.in.order <-
function () 
{


initialdets(storage=tempdir(), diagnose = FALSE, verbose = TRUE) 

makedetguide(p=6, storage = tempdir(), diagnose = FALSE, verbose = TRUE) 

confirm.det(x = NULL, p=5, storage = tempdir(), verbose = TRUE) 

parsedetguide(p=4, symmetric = FALSE, storage = tempdir(), diagnose = FALSE, 
    verbose = TRUE) 

parsedetguide(p=4, symmetric = TRUE, storage = tempdir(), diagnose = FALSE, 
    verbose = TRUE) 

size.predictor(p=10, diagnose = FALSE, verbose = TRUE)
print("", quote=FALSE)

print("***********************************************************************************************************", quote=FALSE)
print("This function has run all 5 functions in this package.",quote=FALSE)
print("This was necessary in order that the location of the external storage directory tempdir() ",quote=FALSE)
print("could be made known to all the functions.",quote=FALSE)
print("", quote=FALSE)
 
}
