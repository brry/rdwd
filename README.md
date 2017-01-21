# rdwd
`rdwd` is an [R](https://www.r-project.org/) package to select, download and read climate data from the 
German Weather Service (Deutscher Wetterdienst, DWD).
They provide over 25 thousand datasets with weather observations online at 
<ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate>.

Usage of the package will usually look something like the following:

```R

# download and install the rdwd package (only needed once):
install.packages("berryFunctions") 
berryFunctions::instGit("brry/rdwd")
# install.packages("rdwd") # will replace the previous two lines once package is on CRAN

# load the package into library (needed in every R session):
library(rdwd)

# view package documentation:
?rdwd

# select a dataset (e.g. last year's daily climate data from Potsdam City):
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")

# Actually download that dataset, returning the local storage file name:
file <- dataDWD(link, read=FALSE)

# Read the file from the zip folder:
clim <- readDWD(file)

# Inspect first couple of entries:
head(clim)
```

You can also select datasets with the interactive map.

Further instructions and examples are explained in the package vignette.

```R
vignette("mapDWD") # interactive map
vignette("rdwd")   # package instructions and examples
```

A real-life usage example of the package can be found at
https://github.com/brry/prectemp/blob/master/Code_analysis.R

