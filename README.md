# rdwd
Select and download climate data from the DWD (German Weather Service) FTP Server.

This package is in heavy development and not yet on CRAN.
I expect to have run enough tests by late November...

Any feedback is now particularly welcome! File an issue here or send me a message (berry-b@gmx.de).

### Code to install the most recent development version from github:

```R
install.packages("berryFunctions") # rdwd depends on this package
berryFunctions::instGit("brry/rdwd")
# or if you already have devtools:
devtools::install_github("brry/rdwd")

# For full usage, as needed in indexDWD and metaDWD(..., current=TRUE):
install.packages("RCurl") # is only suggested, not mandatory dependency

library(rdwd)
?rdwd
```

### Basic Usage:

```R
link <- selectDWD("Potsdam", res="daily", var="kl", time="recent")
link
file <- dataDWD(link, read=FALSE) # download file
file
clim <- readDWD(file)
head(clim)

# can also be vectorized, and time can be abbreviated:
selectDWD(c("Potsdam","Wuerzburg"), res="hourly", var="sun", time="hist")
selectDWD("Potsdam", res="daily", var="kl", time=c("r","h"), outvec=TRUE)

# station metadata for a given path, with current file index (RCurl required):
m_link <- selectDWD(res="monthly", var="more_precip", time="hist", current=TRUE)
# if no station name or ID is given, the argument meta is set to TRUE
m_link
meta_monthly_rain <- dataDWD(m_link, read=TRUE)
head(meta_monthly_rain)
# or as a one-liner:
head(dataDWD(selectDWD(res="hourly", var="sun", time="r")))

# all (available) files for a certain station (meta files may have more results):
selectDWD(id=c(3467, 5116), meta=T) # that's why the default outvec is FALSE
```
For example Tucheim (5116) is listed in [monthly/more_precip/recent/RR_Monatwerte_Beschreibung_Stationen.txt](ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/more_precip/recent/RR_Monatwerte_Beschreibung_Stationen.txt), but actually has no file there.
Filenames in the last output suggest that the historical time series run until 2006, but the file name may be inaccurate.

### Plotting

```R
png("ExampleGraph.png", width=4, height=3, units="in", res=150)
par(mar=c(4,4,2,0.5), mgp=c(2.7, 0.8, 0), cex=0.8)
plot(clim[,c(2,4)], type="l", xaxt="n", las=1, main="Daily temp Potsdam")
berryFunctions::monthAxis(ym=TRUE)
dev.off()
```
![ExampleGraph](https://github.com/brry/rdwd/blob/master/ExampleGraph.png "Example Graph")

### more details

```R
# metadata:
head(rdwd:::metaIndex)
View(data.frame(sort(unique(rdwd:::metaIndex$Stationsname))))

# files
head(rdwd:::fileIndex)
# If you find this to be outdated (Error in download.file ... : cannot open URL),
# please let me know and I will update it. Meanwhile, use current=TRUE in selectDWD

# recursively list files on the FTP-server:
files <- indexDWD("hourly/sun") # use dir="some_path" to save the output elsewhere
berryFunctions::headtail(files, 5, na=TRUE)
# with other FTP servers, this should also work...
funet <- indexDWD(base="ftp://ftp.funet.fi/pub/standards/RFC/ien")
p <- RCurl::getURL("ftp://ftp.funet.fi/pub/standards/RFC/ien",
                       verbose=T, ftp.use.epsv=TRUE, dirlistonly=TRUE)
```

### Installation troubleshooting

If direct installation from CRAN doesn't work, your R version might be too old. In that case, an update is really recommendable: [r-project.org](http://www.r-project.org/). If you can't update R, try installing from source (github) via `instGit` or devtools as mentioned above. If that's not possible either, here's a manual workaround:
click on **Clone or Download -> Download ZIP** (topright, [link](https://github.com/brry/rdwd/archive/master.zip)), unzip the file to some place, then
```R
setwd("that/path")
dd <- dir("rdwd-master/R", full=T)
dummy <- sapply(dd, source)
```
This creates all R functions as objects in your globalenv workspace (and overwrites existing objects of the same name!).

