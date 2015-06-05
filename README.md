warnS3
======

warnS3 is intended to give warnings when loading packages that overload S3 methods. Currently it offers the functions

-   `warnS3Methods` Give a warning for masked S3 methods.
-   `library2`, `require2` Same as base functions with additional warning for S3 methods.

Installation
------------

warnS3 is not yet available on CRAN. You can download the latest development version from github with

``` r
## install.packages("devtools")
devtools::install_github("blasern/warnS3")
```

Development
-----------

warnS3 is still under active development. Extensions are planned for `::`, `loadNamespace`, `get0`, `:::` and possibly other functions. If you find bugs or would like to request additional features, please [file an issue](https://github.com/blasern/warnS3/issues).
