# rdwd
Select and download climate data from the DWD (German Weather Service) FTP Server.

This package is in heavy development and not yet on CRAN.
I expect to have run enough tests by late November...

Any feedback is now particularly welcome! File an issue here or send me a message (berry-b@gmx.de).

## Code to install the most recent development version from github:

```R
# Avoid installing devtools with all its dependencies:
install.packages("berryFunctions")
berryFunctions::instGit("brry/rdwd")

# or using devtools:
if(!requireNamespace("devtools", quitly=TRUE)) install.packages("devtools")
devtools::install_github("brry/rdwd")

library(rdwd)
?rdwd
```

## Basic Usage:

```R
link <- selectDWD("Potsdam", res="daily", var="kl", time="recent")
link
climdata <- dataDWD(link, dir=tempdir())
```

## Plotting

```R
png("ExampleGraph.png", width=4, height=3, units="in", res=150)
plot(climdata[,c(2,4)], type="l", xaxt="n", las=1)
berryFunctions::monthAxis(ym=TRUE)
dev.off()
```
![ExampleGraph](https://github.com/brry/rdwd/blob/master/ExampleGraph.png "Example Graph")

## Installation troubleshooting

If direct installation from CRAN doesn't work, your R version might be too old. In that case, an update is really recommendable: [r-project.org](http://www.r-project.org/). If you can't update R, try installing from source (github) via `instGit` or devtools as mentioned above. If that's not possible either, here's a manual workaround:
click on **Download ZIP** (to the right, [link](https://github.com/brry/rdwd/archive/master.zip)), unzip the file to some place, then
```R
setwd("that/path")
dd <- dir("rdwd-master/R", full=T)
dummy <- sapply(dd, source)
```
This creates all R functions as objects in your globalenv workspace (and overwrites existing objects of the same name!).

