SymbolicDeterminants
====================

The goal of SymbolicDeterminants is to provide the user with a basic tool for
exploring the structure of matrix

determinants. Rather than calculating the determinant of a numeric matrix,
SymbolicDeterminants produces

the formula for calculating it as a textual function of the elements of the
matrix.

Installation
------------

You can install the released version of SymbolicDeterminants from
<https://CRAN.R-project.org> with:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ r
install.packages("SymbolicDeterminants")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use of the functions included in the package
============================================

The first function to run is *anewdetguide(p,storage)*. This function sets up
the external storage directory, creates the detguides from a 2x2 matrix up to
that of a pxp matrix, and stores the detguides in subdirectories storage/2
through storage/p. 

The vignette Background describes the creation and use of detguides, which are
the lists of indices that guide the definition of the textual output.

The function *confirm.det( )* defines a random matrix of the correct size and
tests its detguide determinant against that obtained from the matrix
eigenvalues. The user can also specify his/her own test matrix.

Note that all lower order detguides will be created in order to enable
calculation of the detguide for p. This can take a long time and may run into
issues of object size. Maximum object size may need to be increased in the
workspace. As each detguide is created (by anew*detguide( )* ), the variable
max.created is defined automatically in the storage directory for ease in
determining what has already been created. It may be advisable to increase p by
one level at a time, certainly if p \> 10.

To obtain a symbolic representation, run
*parsedetguide(p,storage,symmetric=FALSE)* for a given value of p. This is a
listing of individual products that make up that determinant. A listing can be
obtained for an unstructured matrix and for a symmetric matrix. Note that there
are p! products in a determinant, each of which requires 3 or 4 lines of text.
Each line has a line number for ease of identification and discussion. The
output of this function is also stored for further processing, if desired. (It
may be useful to copy the output in its entirety from the text file to Word and
to form columns there to reduce the page count.)

To obtain a table of files created and stored for each value of p, run
*detindex(storage)*. All detguides and symbolic representations are stored in
the subdirectory assigned to the input variable “storage”, including the
smallest ones for consistency. To retrieve one of these files run
*retrieve(p,storage)*. The file is designated interactively from the console.
The value of this function is either a detguide or a display of the symbolic
determinant in a text file. \*retrieve" may be assigned to a name in the R
Workspace for further processing of a detguide, if desired.

In order to anticipate the impact of p on calculation time and storage, some
indication can be found from *predictor(p)*.
