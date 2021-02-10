SymbolicDeterminants
====================

The goal of SymbolicDeterminants is to provide the user with a basic tool for
exploring the structure of matrix determinants. Rather than calculating the 
determinant of a numeric matrix, SymbolicDeterminants produces the formula 
for calculating it as a textual function of the elements of the
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

The package can store a complete detguide if it is not too large; otherwise,
it stores a minidetguide. The functions included process one or the other of
these files. The following descriptions all apply to detguides and the later
ones apply to minidetguides.

The first function to run is *anewdetguide(p,storage)*. This function sets up
the external storage directory, creates the detguides from a 2x2 matrix up to
that of a pxp matrix, and stores the detguides in subdirectories storage/2/
through storage/p/. 

The vignette Background describes the creation and use of detguides, which are
the lists of indices that guide the definition of the textual output.

The function *confirm.det( )* defines a random matrix of the correct size and
tests its detguide determinant against that obtained from the matrix
eigenvalues. The user can also specify their own test matrix.

Note that all lower order detguides will be created in order to enable
calculation of the detguide for p. This can take a long time and may run into
issues of object size. Maximum object size may need to be increased in the
workspace. As each detguide is created (by *anewdetguide( )* ), the variable
max.created is defined automatically in the storage directory for ease in
determining what has already been created. It may be advisable to increase p by
one level at a time, certainly if p \> 10.

To obtain a symbolic representation, run
*parsedetguide(p,storage,browser,symmetric=FALSE)* for a given value of p. This is
a listing of individual products that make up that determinant. A listing can be
obtained for an unstructured matrix and for a symmetric matrix. Note that there
are p! products in a determinant, each of which requires a line of text.
Each line has a line number for ease of identification and discussion. The
output of this function is also stored for further processing, if desired.

To obtain a table of files created and stored for each value of p, run
*detindex(storage,xmini="D")*. All detguides and symbolic representations are stored
in the subdirectory assigned to the input variable “storage”, including the
smallest ones for consistency. To retrieve one of these files run
*retrieve(p,storage,browser)*. The file is designated interactively from the 
console. The value of this function is either a detguide or a display of the 
symbolic determinant in a text file. *retrieve* may be assigned to a name in 
the R workspace for further processing of a detguide, if desired.

In order to anticipate the impact of p on calculation time and storage, some
indication can be found from *predictor(p)*.

Saving storage space by use of minidetguides
============================================

A minidetguide consists only of the products of the determinant that have a
leading plus sign (+). It also omits the first row of the detguide because
this is indicated by the storage directory and is reconstituted when the 
minidetguide is parsed. In that way, some savings are accomplished at the
cost of greater processing time. The greatest saving of storage space occurs
because lower order detguides do not have to be stored when using minidetguides.

As above, the first function to run is *anewminidet(p,storage)*. The minidet
matrices are stored in subdirectories of storage/p/ named R1C1, R1C2, ...,
R1Cp corresponding to the leading element of all the products in that sub-
directory.

The function *confirm.minidet(x,p,storage)* defines a random matrix of the 
correct size and tests its minidetguide determinant against that obtained 
from the matrix eigenvalues. The user can also specify their own test matrix.

To obtain a symbolic representation, run
*parsemini(p, storage, symmetric, browser=NULL, cols=1:p, verbose=TRUE)* for 
a given value of p. This is a listing of individual products that make up 
that determinant. A listing can be obtained for an unstructured matrix or 
for a symmetric matrix. Note that there are p! products in a determinant, 
each of which requires a line of text. Each line has a line number for ease 
of identification and discussion. The output of this function is also stored 
for further processing, if desired. Also, because minidetguides are used for
larger values of p, individual columns (R1Cs) can be specified.

The function *detindex(storage, xmini=c("B","D","M"), pmini=1)* can be used
to obtain a listing of all detindexes, minidetindexes or both along with 
their parsed files, if they exist. To retrieve one of these files run
*retrieve.mini(p,storage,browser)*. The file is designated interactively from the 
console. The value of this function is either a detguide or a display of the 
symbolic determinant in a text file. *retrieve.mini* may be assigned to a name in 
the R workspace for further processing of a minidetguide, if desired. Interactively,
one indicates whether a single R1Cs is wanted or all of them for a given p.
