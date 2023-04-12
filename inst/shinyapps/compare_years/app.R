# rdwd weather app to compare years
# How was the weather in the last weeks, compared to the same time period in the past?
# Berry Boessenkool, berry-b@gmx.de.   April 2023

check_package_version <- function(pack, minv)
{
 library(pack, character.only=TRUE)
 V <- suppressWarnings(packageDescription(pack)$Version)
 if(compareVersion(V, minv)<0) stop(pack," version must be >=", minv," but is ", V)
}
check_package_version("rdwd", "1.6.12")
check_package_version("berryFunctions", "1.21.23")
rm(check_package_version)


# meta data ----
{ # code block to get all meta objects during development
data("metaIndex")
meta <- metaIndex[metaIndex$res=="daily" & 
                  metaIndex$var=="kl" & 
                  metaIndex$per=="recent" &
                  metaIndex$hasfile, ]
meta[,c("res","var","per","hasfile")] <- NULL
# rm(metaIndex)

vars <- read.table(stringsAsFactors=FALSE, header=TRUE, sep=":", 
                   strip.white=TRUE, text="
  Abk     : Messgroesse             : Art                     : Einheit
  
  TMK     : Lufttemperatur          : Tagesmittel             : °C
  RSK     : Niederschlag            : Tagessumme              : mm
  FM      : Windgeschwindigkeit     : Tagesmittel             : m/s
  SDK     : Sonnenscheindauer       : Tagessumme              : h
  VPM     : Dampfdruck              : Tagesmittel             : hpa
  PM      : Luftdruck               : Tagesmittel             : hpa
  UPM     : Relative Feuchte	       : Tagesmittel             : %
  RSKF    : Niederschlagsform       : Niederschlagshoehe_ind  : -
  SHK_TAG : Schneehoehe             : Tageswert	              : cm
  TNK     : Lufttemperatur min      : Tagesminimum	           : °C
  TXK     : Lufttemperatur max      : Tagesmaximum  	         : °C
  TGK     : Temperatur 5cm Hoehe    : Tagesminimum	           :	°C
  FX      : Windgeschwindigkeit max : Tagesmaximum Windspitze : m/s
  NM      : Bedeckungsgrad	         : Tagesmittel             : Achtel
                   ")
vars$Label <- paste0(vars$Messgroesse, ", ", vars$Art, " (",vars$Abk, ") ", 
                     "  [ ", vars$Einheit," ]")

funs <- read.table(stringsAsFactors=FALSE, header=TRUE, sep=":", 
strip.white=TRUE, text="rcode  : deutsch
median : Median
mean   : Mittelwert
sum    : Summe
max    : Maximum
min    : Minimum
function(x,...) quantile(x,probs=0.8,...) : Quantil 80%
")

load(system.file("extdata/DEU.rda", package="rdwd"))
} # end meta data

# weather data ----

appenv <- new.env()

get_kl_data <- function(stationname, inapp=TRUE, ...)
{
 # Only load the data if it doesn't already exist in memory:
 klname <- paste0("kl_", gsub(" ", "_", stationname))
 if(exists(klname, envir=appenv))
   return(get(klname, envir=appenv))
 # otherwise do all of the rest
 if(inapp) showNotification(paste0("Reading data for ", stationname, "."),
                            id="downloadstat")
 link <- selectDWD(name=stationname, res="daily", var="kl", per="hr")
 kl <- dataDWD(link, force=c(24*365, 6), hr=5, quiet=inapp, ...)                            
 # Check columns:
 ainc <- vars$Abk %in% colnames(kl)
 if(!all(ainc)) warning("The following variables are missing in the dataset for ",
                        stationname,": ", toString(vars$Abk[!ainc]))
 # Output:
 assign(klname, kl, env=appenv)
 if(inapp) removeNotification("downloadstat")
 return(invisible(kl))
}

if(FALSE){ # Debugging code
kl <- get_kl_data("Neuruppin-Alt Ruppin", inapp=F)
kl <- get_kl_data("Menz", inapp=F)
kl <- kl[,c("MESS_DATUM", "NM")]
link <- selectDWD(id=403, res="daily", var="kl", per="hr")
kl <- dataDWD(link, force=c(24*365, 6), hr=4)   
}


# shiny server ----

server <- function(input, output) {
 
# loc_sel ----
loc_sel <- reactive({ # station selection 
  loc <- "Potsdam"
  if(!is.null(input$location)) loc <- input$location
  if(!is.null(input$map_click)){
   pdist <- berryFunctions::distance(x=meta$geoLaenge, xref=input$map_click$x, 
                                     y=meta$geoBreite, yref=input$map_click$y)
   loc <- meta[which.min(pdist), "Stationsname"]
  }
  loc
})

output$location <- renderUI({
  selectInput("location", "Wähle eine Station, oder klicke in der Karte", 
              choices=meta$Stationsname, selected=loc_sel())
  })


# year_sel ----
year_sel <- reactiveVal(format(Sys.Date(),"%Y"), label="year_sel_val")

observeEvent(input$ts_click, {
 i <- process_kl()
 x <- as.Date(input$ts_click$x, origin="1970-01-01")
 y <- input$ts_click$y
 col <- ncol(i$kl_vals_plot) - as.numeric(round(i$day - x))
 year <- names(which.min(abs(i$kl_vals_plot[,col] - y)))
 year_sel(year) # set
 })

observeEvent(input$agg_click, {
 i <- process_kl()
 pdist <- berryFunctions::distance(x=i$kl_agg$year , xref=input$agg_click$x, 
                                   y=i$kl_agg$value, yref=input$agg_click$y)
 year_sel(i$year[which.min(pdist)])
 })

observeEvent(input$enddate, {
 year_sel(format(input$enddate,"%Y"))
 })

# process_kl ----
process_kl <- reactive({
# inputs:
station   <- loc_sel()
var       <- input$var       # column name representation to be used
day       <- input$enddate   # end day of range
ndays     <- input$ndays     # length of range
cumulated <- input$cumulated # cumulated line (for rainfall, sunshine)?
aggfun    <- input$aggfun    # aggregating function to be applied
# checks:
if(ndays<2) stop("ndays must be larger than 1")
kl <- get_kl_data(station)
column <- vars$Abk[vars$Messgroesse==var]
# take last day in dataset - may not always be the desired procedure:
day <- pmin(day, max(as.Date(kl$MESS_DATUM)))
range_ends <- which(format(kl$MESS_DATUM, "%m-%d") == format(day, "%m-%d"))
# values of each year:
kl_vals <- sapply(range_ends, function(r){
  rng <- r - ndays:1+1 # range index
  rng[rng<=0] <- NA # for first (potentially shorter) year
  values <- kl[rng, column]
  values
})

kl_vals <- t(kl_vals)
kl_vals_plot <- if(cumulated) t(apply(kl_vals, 1, cumsum)) else kl_vals
year <- format(kl$MESS_DATUM[range_ends], "%Y")
rownames(kl_vals) <- year
rownames(kl_vals_plot) <- year
# aggregate:
aggfun2 <- funs$rcode[funs$deutsch==aggfun]
kl_agg <- apply(kl_vals, 1, aggfun2, na.rm=TRUE) # wrap in try?
kl_agg <- data.frame(year=as.numeric(names(kl_agg)), value=kl_agg)
kl_agg$nna <- rowSums(is.na(kl_vals))
kl_agg$value[kl_agg$nna==ncol(kl_agg)] <- NA # do not keep 0 for all-NA-years
# output
# if(station=="Menz" & var=="Bedeckungsgrad") browser()
return(list(station=station, var=var, day=day, ndays=ndays, cumulated=cumulated, 
            aggfun=aggfun, kl_vals_plot=kl_vals_plot, kl_agg=kl_agg, year=year))
})



# plot functions ----

plot_1_ts <- function()
{
i <- process_kl()
rng <- format(i$day-c(i$ndays,1)+1, "%d.%m.")
rng <- paste(rng, collapse=" - ")
ylim <- suppressWarnings(range(i$kl_vals_plot, finite=TRUE))
if(all(!is.finite(ylim))) ylim <- c(0,1)
par(mar=c(2.5,2.5,2,0.2), mgp=c(2,0.7,0), las=1)
plot(i$day,0, type="n", xlim=i$day-c(i$ndays,1)+1, ylim=ylim, 
     main=paste0(vars[vars$Messgroesse==i$var,"Label"],", ",rng), 
     xlab="", ylab="", xaxt="n")
berryFunctions::timeAxis(format="%d.%m.\n")
sapply(i$year, function(y) lines(i$day-i$ndays:1+1, i$kl_vals_plot[y,],
                         col=berryFunctions::addAlpha("steelblue",0.2), lwd=2) )
selyear <- year_sel()
if(!is.logical(selyear))
lines(i$day-i$ndays:1+1, i$kl_vals_plot[selyear,], col="salmon", lwd=4)
# dd <- try(lines(i$day-i$ndays:1+1, i$kl_vals_plot[selyear,], col="salmon", lwd=4))
# if(inherits(dd,"try-error")) browser()
legend("topleft", legend=selyear, text.col="salmon", bty="n")
# if(i$station=="Neuruppin-Alt Ruppin" & i$var=="Sonnenscheindauer") browser()
# box("outer")
}

plot_2_agg <- function()
{
i <- process_kl()
ylim <- suppressWarnings(range(i$kl_agg[,2], finite=TRUE))
if(all(!is.finite(ylim))) ylim <- c(0,1)
par(mar=c(2.5,3,1,0.2), mgp=c(2,0.7,0), las=1)
plot(i$kl_agg[,1:2], type="l", xlab="", ylab=i$aggfun, ylim=ylim, col="steelblue")
points(i$kl_agg[year_sel(),1:2], col="salmon", cex=2, lwd=3)
sna <- sum(i$kl_agg$nna)
pna <- sna/length(i$kl_vals_plot)
legend("topleft", paste0(sna," Fehlwerte (",round(pna*100),"%)"), bty="n")
# box("outer")
}

plot_3_hist <- function()
{
i <- process_kl()
values <- i$kl_agg$value
names(values) <- rownames(i$kl_agg)
allNA <- all(is.na(values))
if(allNA) values <- 0
par(mar=c(3,3,2,0.2), mgp=c(1.8,0.7,0), las=1)
hist(values, breaks=25, col=berryFunctions::addAlpha("steelblue",0.4), main="",
     ylab="Anzahl Jahre pro Wertebereich", xlab=i$aggfun)
if(allNA) return()
abline(v=values[year_sel()], col="salmon", lwd=4)
points(median(values, na.rm=TRUE), mean(par("usr")[3:4]), pch=8, cex=1.5, lwd=2)
# box("outer")
}


# App plots ----

output$plot_1_ts <- renderPlot(plot_1_ts())
output$plot_2_agg <- renderPlot(plot_2_agg())
output$plot_3_hist <- renderPlot(plot_3_hist())

output$map <- renderPlot({
  par(mar=rep(0,4), bg="grey96")
  raster::plot(DEU, border=8) # see ?DEU
  points(meta$geoLaenge, meta$geoBreite, asp=1.6, pch=3, lwd=1, col="steelblue")
  points(geoBreite~geoLaenge, data=meta[meta$Stationsname==loc_sel(),], cex=3, 
         lwd=2, col="salmon")
 })
} # end server


# UI layout ----
ui <- fixedPage( # UserInterface
 titlePanel("Wetter Jahresvergleich"),
 sidebarLayout(
  sidebarPanel(
   uiOutput("location"),
   selectInput("var", "Variable", choices=vars$Messgroesse),
   checkboxInput("cumulated", "kumulierte Werte", value=FALSE),
   selectInput("aggfun", "Funktion", choices=funs$deutsch),
   div(style="display:inline-block",dateInput("enddate", "Enddatum")),
   div(style="display:inline-block",numericInput("ndays", "Anzahl Tage", 
                                                 value=60, min=2, max=365, step=1)),
   plotOutput("map", click="map_click", height="350px"),
   "Für Sonne/Regen Funktion 'Summe' verwenden.", br(),
   "App von",a("Berry", href="mailto:berry-b@gmx.de"), "Boessenkool, ",
   a("Info", href="https://bookdown.org/brry/rdwd"), "/ ",
   a("Quellcode", 
     href="https://github.com/brry/rdwd/blob/master/inst/shinyapps/compare_years/app.R"),
  ),
  # Show plots
  mainPanel(plotOutput("plot_1_ts"  , height="250px", click="ts_click"),
            plotOutput("plot_2_agg" , height="250px", click="agg_click"),
            plotOutput("plot_3_hist", height="250px")
            ), 
  fluid=TRUE
 )
) # end ui
shinyApp(ui=ui, server=server)
