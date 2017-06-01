# rdwd
`rdwd` is an [R](https://www.r-project.org/) package to select, download and read climate data from the 
German Weather Service (Deutscher Wetterdienst, DWD).
They provide over 25 thousand datasets with weather observations online at 
<ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate>.


###WARNING:
as of June 1, the FTP server structure has 
[changed](ftp://ftp-cdc.dwd.de/test/CDC/help/Changes_detail_obsgermany_formats_20170601.pdf).
Parts of the package currently do not work correctly anymore.
I'm working on adapting to the changes, please have a bit of patience.
Sorry for any inconveniences.

\

\

`rdwd` is available on CRAN:
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/rdwd)](http://cran.r-project.org/package=rdwd) 
[![downloads](http://cranlogs.r-pkg.org/badges/rdwd)](http://www.r-pkg.org/services)
[![Rdoc](http://www.rdocumentation.org/badges/version/rdwd)](http://www.rdocumentation.org/packages/rdwd)
and has been presented at [FOSDEM](https://fosdem.org/2017/schedule/event/geo_weather/) in Brussels,
featured in Rstudios [data package list](https://www.rstudio.com/rviews/2017/02/17/january-new-data-packages/) 
and written about in [OSOR](https://joinup.ec.europa.eu/community/osor/news/study-german-weather-data-made-easy-rdwd).

Usage of the package usually looks something like the following:

```R

# download and install the rdwd package (only needed once):
install.packages("rdwd")

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

# Inspect the data.frame:
str(clim)
```

You can also select datasets with the [interactive map](https://cran.r-project.org/package=rdwd/vignettes/mapDWD.html).
Further instructions and examples are available in the [package vignette](https://cran.r-project.org/package=rdwd/vignettes/rdwd.html).

```R
vignette("mapDWD") # interactive map, likely faster than CRAN link above
vignette("rdwd")   # package instructions and examples
```

A real-life usage example of the package can be found at
<https://github.com/brry/prectemp/blob/master/Code_analysis.R>

# help
I'm looking for someone to help implement multiple downloads in [dataDWD](https://github.com/brry/rdwd/blob/master/R/dataDWD.R#L160) via e.g. `curl` or `wget`.
Some payment can be arranged.
The requirements are as follows:

* must work cross platform
* is called from R
* has as few dependencies as possible
 

