SymbolicDeterminants 1.2.0.9002
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
