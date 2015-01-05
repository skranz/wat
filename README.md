wat
===

R Tools for web-assissted teaching via shiny server

## Installation

```r
# You can set the installation directory of the R packages and store it in lib
lib=.libPaths()[1]

if (!require(devtools))
   install.packages("devtools",lib=lib)
   
with_libpaths(new = lib, install_github(repo="skranz/wat"))
```
