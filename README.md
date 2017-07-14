# rxylib Shiny

'rxylib Shiny' is a [Shiny](http://shiny.rstudio.com) app providing a graphical user interface for the **R** package ['rxylib'](https://CRAN.R-project.org/package=rxylib). The app allows visualising x-y data from different file formats. For a complete overview see the ['rxylib'](https://CRAN.R-project.org/package=rxylib) manual.

## Impressions

![](img/screenshot.jpg)

## Installation and usage

** Note: This shiny app will just work with the current development version of rxylib.  To install the latest development builds directly from GitHub, run:**

```r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/rxylib@master")
```

The application can be run on a local machine with the following command:

```r
rxylibShiny::run_rxylib()
```

## Related projects 

* [rxylib](https://github.com/R-Lum/rxylib)
* [RLumModel](https://github.com/R-Lum/RLumModel)
* [Luminescence](https://github.com/R-Lum/Luminescence)

## Platform tests status 
[![Build Status](https://travis-ci.org/JohannesFriedrich/rxylibShiny)](https://travis-ci.org/JohannesFriedrich/rxylibShiny)

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
