SymbolicDeterminants 1.0.2
==========================

-   Removed default name of storage directory in favor of tempdir()

-   tempdir() doesn’t work as expected when one function needs to retrieve data
    from the temporary directory created by another function and the function
    test order isn’t the same as how the package is to be used.  I added a
    function called test.in.order() to keep them in line

SymbolicDeterminants 1.0.1
==========================

-   Added a `NEWS.md` file to track changes to the package.

-   First submission of this package
