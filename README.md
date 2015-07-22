# sdsmR Package
R Package for Statistical Downscaling

The purposes of this package is to move the functionality of the widely used 
Statistical Downscaling Model (SDSM) tool into the R language. This is useful 
because R as a language already has most of the SDSM functionality built into it, 
and provides many more advanced statistical and graphics options. It also avoids the issues
of SDSM only working on Windows machines, and moves downscaling from a point and 
click interface towards a more programma


## Installation
This package is not yet on CRAN. To install the development version seen here, 
the easiest way is by using the devtools package:

```{r}
# install.packages("devtools")
library("devtools")
devtools::install_github('leerichardson/sdsmR')
```
Sometimes instaling devtools isn't automatic, and you may need to install 
some other dependencies (such as XML or RTools for Windows)to make this work. 
However, I think it will be well worth the initial struggle!
