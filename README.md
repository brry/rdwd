# rdwd
`rdwd` is an [R](https://www.r-project.org/) package to select, download and read climate data from the 
German Weather Service (Deutscher Wetterdienst, DWD).
They provide over 228 thousand datasets with weather observations online at 
<ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate>.

`rdwd` is available on CRAN:
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/rdwd)](https://cran.r-project.org/package=rdwd) 
[![downloads](http://cranlogs.r-pkg.org/badges/rdwd)](https://www.r-pkg.org/services)
[![Rdoc](http://www.rdocumentation.org/badges/version/rdwd)](https://www.rdocumentation.org/packages/rdwd)
!["rdwd dependencies"](https://tinyverse.netlify.com/badge/rdwd)

It has been presented at [FOSDEM 2017](https://fosdem.org/2017/schedule/event/geo_weather/)
and [UseR!2017](https://user2017.sched.com/event/Axr3/rdwd-manage-german-weather-observations) in Brussels,
featured in Rstudios [data package list](https://www.rstudio.com/rviews/2017/02/17/january-new-data-packages/) 
and written about in [OSOR](https://joinup.ec.europa.eu/community/osor/news/study-german-weather-data-made-easy-rdwd).

Usage of the package usually looks something like the following:

```R
# download and install the rdwd package (only needed once):
install.packages("rdwd")
# if wanted, latest development version, incl. vignettes:
remotes::install_github("brry/rdwd", build_opts="--no-manual")

# load the package into library (needed in every R session):
library(rdwd)

# select a dataset (e.g. last year's daily climate data from Potsdam City):
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")

# Actually download that dataset, returning the local storage file name:
file <- dataDWD(link, read=FALSE)

# Read the file from the zip folder:
clim <- readDWD(file, varnames=TRUE)

# Inspect the data.frame:
str(clim)
```

You can also select datasets with the [interactive map](https://cran.r-project.org/package=rdwd/vignettes/mapDWD.html).  
Installation instructions and more examples are available in the [package vignette](https://cran.r-project.org/package=rdwd/vignettes/rdwd.html).  
Long actual-usage examples can be found in the [use cases vignette](https://cran.r-project.org/package=rdwd/vignettes/cases.html).

```R
vignette("mapDWD") # interactive map, likely faster than CRAN link above
vignette("rdwd")   # package instructions and examples
vignette("cases")  # longer use case examples
```


# help
I'm looking for someone to help implement multiple downloads in [dataDWD](https://github.com/brry/rdwd/blob/master/R/dataDWD.R#L176) via e.g. `curl` or `wget`.
The requirements are as follows:

* works cross-platform
* is called from R
* has as few dependencies as possible
* does not fail completely at a single failure, e.g. can be called within `try` 
* optimally enables a progress bar
