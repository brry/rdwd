# rdwd weather app to compare years
# How was the weather in the last weeks, compared to the same time period in the past?
# Berry Boessenkool, berry-b@gmx.de.   April 2023

compare_years <- function(
  kl,             # data.frame with (at least) columns MESS_DATUM and 'column'
  column,         # column name to be used
  day=Sys.Date(), # end day of range
  ndays=30,       # length of range
  cumulated=TRUE, # cumulated line (for rainfall)?
  aggfun=sum,     # aggregating function to be applied like sum, mean, median, 
                  # function(x) quantile(x,probs=0.8,...). Must accept na.rm=TRUE
  fname=deparse(substitute(aggfun)), # function name for label
  ...             # Arguments passed to plot  
  )
{
# checks:
force(fname) # evaluate before aggfun is used and substitute no longer works as intended
# column existence?
# take last day in dataset - may not always be the desired procedure:
day <- pmin(day, max(as.Date(kl$MESS_DATUM)))
range_ends <- which(format(kl$MESS_DATUM, "%m-%d") == format(day, "%m-%d"))
# values of each year:
kl_vals <- sapply(range_ends, function(r){
  rng <- r - ndays:1+1 # range index
  rng <- rng[rng>0] # for first (potentially shorter) year
  values <- kl[rng, column]
  return(values)
})
kl_vals <- t(kl_vals)
kl_vals_plot <- if(cumulated) t(apply(kl_vals, 1, cumsum)) else kl_vals
year <- format(kl$MESS_DATUM[range_ends], "%Y")
rownames(kl_vals) <- year
rownames(kl_vals_plot) <- year
# aggregate:
kl_agg <- apply(kl_vals, 1, aggfun, na.rm=TRUE) # wrap in try?
kl_agg <- data.frame(year=as.numeric(names(kl_agg)), value=kl_agg)
kl_agg$nna <- rowSums(is.na(kl_vals))
kl_agg$value[kl_agg$nna==ndays] <- NA # do not keep 0 for all-NA-years
# visualize:
# time series:
par(mar=c(2.5,3,2,0.5), mgp=c(2,0.7,0), mfrow=c(1,3), oma=c(0,0,2,0), las=1, ...)
plot(day,0, type="n", xlim=day-c(ndays,0), ylim=range(kl_vals_plot, na.rm=TRUE), 
     xlab="", ylab="", main=paste(if(cumulated) "cumulated", "time series"), xaxt="n")
berryFunctions::timeAxis(format="%d.%m.\n")
sapply(year, function(y) lines(day-ndays:1+1, kl_vals_plot[y,],
                               col=ifelse(y==max(year), "salmon", "#00000033"),
                               lwd=ifelse(y==max(year), 4, 2)) )
# aggregated per year:
plot(kl_agg[,1:2], type="l", main=fname)
points(tail(kl_agg[,1:2],1), col="salmon", cex=2, lwd=3)
# histogram
hist(kl_agg$value, xlab="", breaks=20, main="histogram")
abline(v=tail(kl_agg$value,1), col="salmon", lwd=3)
points(median(kl_agg$value, na.rm=TRUE), mean(par("usr")[3:4]), pch=8, cex=1.5, lwd=2)
# main title
title(main=paste0(column," of ",ndays," days before ",day), outer=TRUE, line=0)
# output:
return(invisible(list(day=day, aggfun=aggfun, vals=kl_vals, agg=kl_agg)))
}


if(FALSE){
rdwd::updateRdwd()
library(rdwd)
urls <- selectDWD("Potsdam", "daily", "kl", "hr")
kl <- dataDWD(urls, varnames=TRUE, hr=5, force=6, overwrite=TRUE) ; rm(urls)

compare_year(kl, "RSK.Niederschlagshoehe", ndays=90)
compare_year(kl, "TMK.Lufttemperatur", cumulated=F, ndays=90, aggfun=median)
compare_year(kl, "TXK.Lufttemperatur_Max", cumulated=F, ndays=30, aggfun=median)
compare_year(kl, "TNK.Lufttemperatur_Min", cumulated=F, ndays=90, aggfun=median)
compare_year(kl, "SDK.Sonnenscheindauer", cumulated=T, ndays=60, aggfun=median)

}



# old stuff ----
if(F){
# Rainfall overview:
berryFunctions::logHist(kl$RSK.Niederschlagshoehe) # up to 104 mm/day
mean(kl$RSK.Niederschlagshoehe<=0) # 51% of days with no rainfall
mean(kl$RSK.Niederschlagshoehe<=1) # 71% of days with almost no rainfall
mean(is.na(kl$RSK.Niederschlagshoehe)) # 0% NA!
mean(diff(kl$MESS_DATUM) != 1) # 0 days missing in dataset! Since 1893!

compare_aggregates(kl, "RSK.Niederschlagshoehe")
pdf("Regen_Potsdam_aggregates.pdf", height=5)
pbapply::pbsapply(0:90, function(n) compare_aggregates(kl, "RSK.Niederschlagshoehe", ndays=n))
dev.off()
berryFunctions::openFile("Regen_Potsdam_aggregates.pdf")

pdf("Regen_Potsdam_timeseries.pdf", height=5)
pbapply::pbsapply(1:90, function(n) compare_timeseries(kl, "RSK.Niederschlagshoehe", ndays=n))
dev.off()
berryFunctions::openFile("Regen_Potsdam_timeseries.pdf")
}



# select and create metadata ----

data("metaIndex")
data("DEU")
meta <- metaIndex[metaIndex$res=="daily" & 
                  metaIndex$var=="kl" & 
                  metaIndex$per=="recent" &
                  metaIndex$hasfile, ]


vars <- read.table(stringsAsFactors=FALSE, header=TRUE, sep=":", 
                   strip.white=TRUE, text="
  Abk     : Messgroesse             : Art                     : Einheit
  RSK     : Niederschlag            : Tagessumme              : mm
  RSKF    : Niederschlagsform       : Niederschlagshoehe_ind  : -
  SHK_TAG : Schneehoehe             : Tageswert	              : cm
  TMK     : Lufttemperatur 2m       : Tagesmittel             : °C
  TXK     : Lufttemperatur 2m max   : Tagesmaximum  	         : °C
  TNK     : Lufttemperatur 2m min   : Tagesminimum	           : °C
  TGK     : Temperatur 5cm          : Tagesminimum	           :	°C
  FM      : Windgeschwindigkeit     : Tagesmittel             : m/s
  FX      : Windgeschwindigkeit max : Tagesmaximum Windspitze : m/s
  SDK     : Sonnenscheindauer       : Tagessumme              : h
  NM      : Bedeckungsgrad	         : Tagesmittel             : Achtel
  VPM     : Dampfdruck              : Tagesmittel             : hpa
  PM      : Luftdruck               : Tagesmittel             : hpa
  UPM     : Relative Feuchte	       : Tagesmittel             : %
                   ")
vars$Label <- paste0(vars$Messgroesse, ", ", vars$Art, " (",vars$Abk, ") ", 
                     "  [ ", vars$Einheit," ]")

funs <- read.table(stringsAsFactors=FALSE, header=TRUE, sep=":", strip.white=TRUE, text="
rcode  : deutsch
sum    : Summe
cumsum : Kumulierte Summe
mean   : Mittelwert
median : Median
max    : Maximum
min    : Minimum
")



# Function to download hist+recent dwd data ----

appenv <- new.env()

get_kl_data <- function(stationname, dir=localdir, ...)
{
 # Only load the data if it doesn't already exist in memory:
 klname <- paste0("kl_", gsub(" ", "_", stationname))
 if(exists(klname, envir=appenv))
   return(get(klname, envir=appenv))
 # otherwise do all of the rest
 # ToDo: message to app ("Reading data for ", stationname, ". This may take a few seconds...") ####################
 link <- selectDWD(name=stationname, res="daily", var="kl", per="hr",outvec=TRUE)
 # actually download + read data:
 kl <- dataDWD(link, dir=dir, force=c(FALSE,NA), quiet=FALSE, overwrite=TRUE, fread=TRUE, ...)                            
 names(kl) <- c("hist","recent")
 # merge:
 kl <- rbind(kl$hist, kl$recent[!kl$recent$MESS_DATUM %in% kl$hist$MESS_DATUM,])
 # Check columns:
 ainc <- vars$Abk %in% colnames(kl)
 cina <- colnames(kl) %in% c(vars$Abk,"STATIONS_ID","MESS_DATUM","QN_3","QN_4","eor")
 if(!all(ainc)) warning("The following variables are missing in the dataset for ",
                        stationname,": ", toString(vars$Abk[!ainc]))
 if(!all(cina)) warning("The following variables are not expected in the dataset",
                        " for ", stationname,": ", toString(colnames(kl)[!cina]))
 # change and add column:
 kl$MESS_DATUM <- as.Date(kl$MESS_DATUM)
 kl$day <- format(kl$MESS_DATUM,"%m-%d")
 # Output:
 assign(klname, kl, env=appenv)
 return(invisible(kl))
}

#pdfpng(pbsapply(1:nrow(vars), function(i) plot(kl$MESS_DATUM, kl[,vars$Abk[i]],
#                   type="l", las=1, main=vars$Label[i], xlab="Jahr", ylab="")),
#       "Timeseries", png=FALSE, open=TRUE) # 25 secs as png, faster as pdf
#logHist(kl$RSK, breaks=50, col="cornflowerblue")



# Main plotting function ----

plot_app <- function(station, var, fun, enddate, ndays, histbreaks, type="hist")
{
 if(ndays<2) stop("ndays must be larger than 1")
 kl <- get_kl_data(station)
 dates <- enddate - ndays:1
 # Datum in Betrachtungsperiode?
 inper <- kl$day %in% format(dates, "%m-%d")
 incur <- kl$MESS_DATUM %in% dates
 inper[incur] <- FALSE
 Jahr <- as.numeric(format(kl$MESS_DATUM,"%Y"))
 column <- vars$Abk[vars$Messgroesse==var]
 compfun <- funs$rcode[funs$deutsch==fun]
 if(compfun=="cumsum") type <- "cumsum"
 if(type!="hist") kl$datum08 <- as.Date(paste0("2008-", kl$day))
 #
 if(type=="hist")
 {
 annualvals <- tapply(kl[inper, column], Jahr[inper], get(compfun), na.rm=TRUE)
 hist(annualvals, col="cornflowerblue", las=1, ylab="Anzahl Jahre pro Wertebereich",
      xlab=paste(compfun, "-",vars$Label[vars$Messgroesse==var]), breaks=histbreaks,
      main=paste0(format(dates[1],"%d. %b."), "  -  ", format(dates[ndays],"%d. %b."), ", ", station),
      sub="") # ToDo: INfo about n years since tt, n days missing etc.
 points(get(compfun)(kl[incur, column], na.rm=TRUE), mean(par("usr")[3:4]), type="h", lwd=3, col="red")
 }
 #
 if(type=="ts")  # ToDo: lim0 vs range depending on var
 {
 # line plot time series
 plot(kl[incur,c("datum08",column)], type="n", ylim=lim0(kl[inper|incur, column]), 
      las=1, xlab="Datumsbereich", main=var)
 for(j in unique(Jahr)) lines(kl[inper&Jahr==j,c("datum08",column)], col="grey")
 lines(kl[incur,c("datum08",column)], lwd=3, col="blue")
 }
 #
 if(type=="cumsum")
 {
 kumu <- tapply(kl[inper|incur, column], Jahr[inper|incur], cumsum) # NAs?
 plot(kl$datum08[incur], kumu[[1]], type="n", ylim=range(unlist(kumu)), las=1,
      xlab="Datumsbereich", ylab="Kumulierte Summe", main=var)
 dummy <- lapply(kumu, lines, x=kl$datum08[incur], col="grey")
 lines(kl$datum08[incur], unlist(tail(kumu,1)), lwd=3, col="blue")
 }
}



#plot_app("Potsdam", "Niederschlag", "Summe", Sys.Date(), 30, 30, "cumsum")




server <- function(input, output) {
 
 # Reactive station selection ----
 
 loc_sel <- reactive({
  loc <- "Potsdam"
  if(!is.null(input$location)) loc <- input$location
  if(!is.null(input$plot_click)){
   pdist <- berryFunctions::distance(x=meta$geoLaenge, xref=input$plot_click$x, 
                                     y=meta$geoBreite, yref=input$plot_click$y)
   loc <- meta[which.min(pdist), "Stationsname"]
  }
  loc
 })
 
 output$location <- renderUI({selectInput("location", "Wähle eine Station, oder klicke in der Karte", 
                                          choices=meta$Stationsname, selected=loc_sel())
 })
 

 # App plots ----
 
 output$wetterplot <- renderPlot({
  par(bg="grey96")
  plot_app(loc_sel(), input$var, input$fun, input$enddate, input$ndays, 
           input$histbreaks, type=ifelse(input$tshist, "ts", "hist"))
 })

 output$map <- renderPlot({
  par(mar=rep(0,4))
  raster::plot(DEU, border=8) # see ?DEU
  points(meta$geoLaenge, meta$geoBreite, asp=1.6, pch=3, lwd=1, col="red")
  points(geoBreite~geoLaenge, data=meta[meta$Stationsname==loc_sel(),], cex=3, lwd=2, col="orange")
 })
} # end server



# User interface layout ----

ui <- fixedPage(
 titlePanel("Wetter App"),
 sidebarLayout(
  sidebarPanel(
   a("App", href="https://github.com/brry/rdwd/blob/master/inst/shinyapps/wetter/app.R"),
   "von Berry Boessenkool,", a("berry-b@gmx.de", href="mailto:berry-b@gmx.de"),
   br(),br(),
   "Beschreibung ", 
   "kommt noch.",
   br(),br(),
   uiOutput("location"),
   checkboxInput("tshist", "Zeitreihe statt Histogramm", value=FALSE),
   selectInput("var", "Variable", choices=vars$Messgroesse),
   selectInput("fun", "Funktion", choices=funs$deutsch),
   dateInput("enddate", "Enddatum"),
   column(5.5, numericInput("ndays", "Anzahl Tage", value=30, min=2, max=365, step=1)),
   column(5.5, numericInput("histbreaks", "Histogram breaks", value=30, min=5, max=200, step=5))
     #numericInput("ylim", "y axis limit (NA or empty for automatic)", value=NA, min=0, max=20e3, step=500)
  ),
  # Show plots
  mainPanel(plotOutput("wetterplot"),
            plotOutput("map", click="plot_click"))
  ,
  fluid=FALSE
 )
)
shinyApp(ui = ui, server = server)

