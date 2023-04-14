# rdwd
<img src="https://github.com/brry/rdwd/raw/master/misc/hex/hex.png" width="104" height="120">

`rdwd` is an [R](https://www.r-project.org/) package to select, download and read climate data from the 
German Weather Service (Deutscher Wetterdienst, DWD).  
The DWD provides thousands of datasets with weather observations online at 
[opendata.dwd.de](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/).  
Since May 2019, `rdwd` also supports reading the Radolan (binary) raster data at 
[grids_germany](https://opendata.dwd.de/climate_environment/CDC/grids_germany/).

`rdwd` is available on CRAN:
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/rdwd)](https://cran.r-project.org/package=rdwd) 
[![downloads](http://cranlogs.r-pkg.org/badges/rdwd)](https://www.r-pkg.org/services)
[![Rdoc](http://www.rdocumentation.org/badges/version/rdwd)](https://www.rdocumentation.org/packages/rdwd)
!["rdwd dependencies"](https://tinyverse.netlify.com/badge/rdwd)

It has been presented at [FOSDEM 2017](https://archive.fosdem.org/2017/schedule/event/geo_weather/)
and [UseR!2017](https://user2017.sched.com/event/Axr3/rdwd-manage-german-weather-observations) in Brussels and with a 5 Minute [video](https://youtu.be/KOYZPMMgiHo?t=233) at [e-Rum2020](https://milano-r.github.io/erum2020program/lightning-talks.html#rdwd-r-interface-to-german-weather-service-data),
featured in Rstudio's [data package list](https://rviews.rstudio.com/2017/02/17/january-new-data-packages), 
written about in [OSOR](https://joinup.ec.europa.eu/collection/open-source-observatory-osor/news/study-german-weather-data) and used e.g. for
[NDR: Starkregen im Norden](https://story.ndr.de/starkregen-im-norden/index.html).
Development of `rdwd` was triggered 2016 by flash flood research in Braunsbach 
([1](https://www.uni-potsdam.de/en/natriskchange/qualification-program/task-force-braunsbach-flash-flood-2016), [2](https://doi.org/10.1016/j.scitotenv.2018.02.241),
[3](https://doi.org/10.5675/HyWa_2017,3_1),
[4](https://publishup.uni-potsdam.de/frontdoor/index/index/docId/39488)).


```diff
- HELP NEEDED
- with the new 5-minute data (April 2022), the fileIndex etc are getting very big.
- ideas on package size reduction are welcome at  https://github.com/brry/rdwd/issues/35
```

### Documentation

A website with more information, examples, use cases and an interactive map of the DWD stations
can be found at <https://bookdown.org/brry/rdwd>


### Usage

Usage for observational weather data from the measuring stations usually looks something like the following:

```R
# Download and install (once only):
install.packages("rdwd")
# Load the package into library (needed in every R session):
library(rdwd)

# select a dataset (e.g. last year's daily climate data from Potsdam city):
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")

# Actually download that dataset, returning the local storage file name:
file <- dataDWD(link, read=FALSE)
# Read the file from the zip folder:
clim <- readDWD(file, varnames=TRUE) # can happen directly in dataDWD

# Inspect the data.frame:
str(clim)
# Quick time series graphic:
plotDWD(clim, "FM.Windgeschwindigkeit")
```

For data interpolated onto a 1 km raster, including radar data up to the last hour,
see the corresponding [chapter](https://bookdown.org/brry/rdwd/raster-data.html) on the website.


### New to R

If you're new to R, these links might help you to get started:

- [install R & Rstudio](https://github.com/brry/course/#install)
- [brief introduction to R](https://github.com/brry/hour)
- [very large set of slides I use for my courses](https://github.com/brry/course/#slides)

back to `rdwd`:


### Installation

#### Normal
```R
install.packages("rdwd")
```

#### Latest version
```R
rdwd::updateRdwd()
# checks version and (if needed) calls  remotes::install_github("brry/rdwd", build_vignettes=TRUE)
```

#### Full
Suggested (not mandatory) dependencies:  
```R
install.packages("rdwd", dependencies="Suggests") 
```

- `RCurl` for indexFTP and selectDWD(..., current=TRUE)
- `data.table`, `bit64` for readDWD(..., fread=TRUE)
- `raster`, `R.utils`, `ncdf4`, `dwdradar` for readDWD with gridded data
- `readr` for readDWD.stand(..., fast=TRUE)
- `knitr`, `rmarkdown`, `testthat`, `roxygen2`, `devtools`, `remotes`, `XML`, `gsheet` for local testing, development and documentation
- `leaflet`, `OSMscale`, `sp` for interactive/static maps, see [OSMscale installation tips](https://github.com/brry/OSMscale#installation)
- `shiny` for the interactive weather comparison app

Note: on Linux (Ubuntu), install `RCurl` via the terminal (CTRL+ALT+T, note lowercase rcurl):
```
sudo apt install r-cran-rcurl
```
