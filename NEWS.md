SymbolicDeterminants 2.0.0
==========================

-   Added functions to reduce the amount of space used in storing
    detguides and other functions to process, retrieve, and index   
    detguides and minidetguides.

-   Made more functions interactive in order to identify HTML
    browser on non-Windows operating systems.


SymbolicDeterminants 1.3.0
==========================

-   Improved display of symbolic representation by means of HTML and
    use of Web browser

SymbolicDeterminants 1.2.0
===============================

-   Consolidated two functions into one and renamed it in order to ensure
    testing is in proper file order

-   Changed the name of another function for the same reason

-   Changed package build to make all functions visible

-   Made README visible

-   Cleaned up vignette “Background”

SymbolicDeterminants 1.0.2
==========================

-   Removed default name of storage directory in favor of tempdir()

-   tempdir() doesn’t work as expected when one function needs to retrieve data
    from the temporary directory created by another function and the function
    test order isn’t the same as how the package is to be used. I added a
    function called test.in.order() to keep them in line

SymbolicDeterminants 1.0.1
==========================

-   Added a `NEWS.md` file to track changes to the package.

-   First submission of this package
