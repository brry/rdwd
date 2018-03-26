## ----install, eval=FALSE-------------------------------------------------
#  install.packages("rdwd")
#  # get the latest development version from github:
#  berryFunctions::instGit("brry/rdwd")
#  # For full usage, as needed in indexFTP and metaDWD(..., current=TRUE):
#  install.packages("RCurl") # is only suggested, not mandatory dependency

## ----basics, eval=TRUE---------------------------------------------------
library(rdwd)
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=tempdir(), quiet=TRUE)
# tempdir is only for CRAN vignette checks. In real life, use a real folder.
clim <- readDWD(file)

str(clim)

## ----plot, eval=TRUE, fig.height=3, fig.width=7--------------------------
par(mar=c(4,4,2,0.5), mgp=c(2.7, 0.8, 0), cex=0.8)
plot(clim[,c(2,4)], type="l", xaxt="n", las=1, main="Daily temp Potsdam")
berryFunctions::monthAxis(ym=TRUE)   ;   abline(h=0)
mtext("Source: Deutscher Wetterdienst", adj=-0.1, line=0.5, font=3)

## ----climgraph, eval=TRUE, fig.height=3, fig.width=7, echo=-1------------
par(mar=c(4,4,2,0.5), mgp=c(2.7, 0.8, 0), cex=0.8)
link <- selectDWD("Goettingen", res="monthly", var="kl", per="h")
clim <- dataDWD(link, quiet=TRUE)
clim$month <- substr(clim$MESS_DATUM_BEGINN,5,6)
temp <- tapply(clim$MO_TT, clim$month, mean, na.rm=TRUE)
prec <- tapply(clim$MO_RR, clim$month, mean, na.rm=TRUE)
library(berryFunctions)
headtail(clim[!is.na(clim$MO_TT)&!is.na(clim$MO_RR),])
# as of 2018-03, there are mostly NAs in MO_RR in many stations.
# A message has been sent to DWD.
climateGraph(temp, prec, main="Goettingen 1857:1946")
mtext("Source: Deutscher Wetterdienst", adj=-0.05, line=2.8, font=3)

## ----findID, eval=TRUE---------------------------------------------------
findID("Potsdam")
findID("Koeln", exactmatch=FALSE)

## ----files, eval=FALSE---------------------------------------------------
#  # all files at a given path, with current file index (RCurl required):
#  links <- selectDWD(res="monthly", var="more_precip", per="hist", current=TRUE)

## ----listfiles, eval=FALSE-----------------------------------------------
#  # recursively list files on the FTP-server:
#  files <- indexFTP("hourly/sun") # use dir="some_path" to save the output elsewhere
#  berryFunctions::headtail(files, 5, na=TRUE)
#  
#  # indexFTP uses a folder for resumed indexing after getting banned:
#  gridindex <- indexFTP("radolan","ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly")
#  gridindex <- indexFTP(gridindex,"ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly", sleep=1)
#  
#  # with other FTP servers, this should also work...
#  funet <- indexFTP(base="ftp.funet.fi/pub/standards/w3/TR/xhtml11/", folder="")
#  p <- RCurl::getURL("ftp.funet.fi/pub/standards/w3/TR/xhtml11/",
#                         verbose=T, ftp.use.epsv=TRUE, dirlistonly=TRUE)

## ----select1, eval=FALSE, echo=TRUE--------------------------------------
#  # inputs can be vectorized, and period can be abbreviated:
#  selectDWD(c("Potsdam","Wuerzburg"), res="hourly", var="sun", per="hist")

## ----select2, eval=TRUE, echo=FALSE--------------------------------------
lapply(selectDWD(c("Potsdam","Wuerzburg"), res="hourly", var="sun", per="hist"), function(x) gsub("ical/", "ical/ ", x))

## ----select3, eval=FALSE, echo=TRUE--------------------------------------
#  # Time period can be doubled to get both filenames:
#  selectDWD("Potsdam", res="daily", var="kl", per="rh", outvec=TRUE)

## ----select4, eval=TRUE, echo=FALSE--------------------------------------
gsub("/tages", "/ tages", selectDWD("Potsdam", res="daily", var="kl", per="rh", outvec=TRUE))

## ----select5, eval=TRUE--------------------------------------------------
lapply(selectDWD(id=c(3467,5116)), substr, 58, 1e4)

## ----meta23, eval=TRUE---------------------------------------------------
# All metadata at all folders:
data(metaIndex)
str(metaIndex, vec.len=2)

## ----metaView, eval=FALSE------------------------------------------------
#  View(data.frame(sort(unique(rdwd:::metaIndex$Stationsname)))) # ca 6k entries

## ----meta1, eval=TRUE----------------------------------------------------
# file with station metadata for a given path:
m_link <- selectDWD(res="monthly", var="more_precip", per="hist", meta=TRUE)
substr(m_link, 50, 1e4) # (Monatswerte = monthly values, Beschreibung = description)

## ----meta2, eval=FALSE---------------------------------------------------
#  meta_monthly_rain <- dataDWD(m_link, dir=tdir) # not executed in vignette creation
#  str(meta_monthly_rain)

