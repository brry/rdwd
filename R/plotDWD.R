#' @title Quickly plot time series
#' @description plot rdwd time series from data.frames
#' @return Nothing
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2021
#' @seealso [readDWD()]
#' @keywords hplot
#' @importFrom berryFunctions monthAxis tstop
#' @export
#' @examples
#' link <- selectDWD("Potsdam", res="daily", var="kl", per="r")
#' clim <- dataDWD(link, dir=locdir(), varnames=TRUE)
#' plotDWD(clim, "TMK.Lufttemperatur", line0=TRUE, main="Potsdam")
#'
#' @param x         Data.frame, e.g. from [readDWD.data]
#' @param cn        Column name (charstring)
#' @param monthaxis Draw nice axis? DEFAULT: TRUE
#' @param line0     Draw horizontal line at 0? DEFAULT: FALSE
#' @param xlab      X axis label. DEFAULT: ""
#' @param ylab      Y axis label. DEFAULT: cn
#' @param main      Plot title. DEFAULT: ""
#' @param type      [graphics::plot] type. DEFAULT: "l"
#' @param lwd       Line width. DEFAULT: 3
#' @param col       Line color. DEFAULT: "blue"
#' @param las       Label axis style. DEFAULT: 1 (all upright)
#' @param mar       Plot margins. DEFAULT: c(2.6, 3.1, 2.5, 0.5)
#' @param mgp       Margin placement. DEFAULT: c(1.9, 0.7, 0)
#' @param keeppar   Keep `las, mar and mgp` as set with [par], 
#'                  so later points are added in the right location?
#'                  DEFAULT: TRUE
#' @param \dots     Further arguments passed to [graphics::plot]
#'
plotDWD <- function(
x,
cn,
monthaxis=TRUE,
line0=FALSE,
xlab="",
ylab=cn,
main="",
type="l",
lwd=3,
col="blue",
las=1,
mar=c(2.6, 3.1, 2.5, 0.5),
mgp=c(1.9, 0.7, 0),
keeppar=TRUE,
...
)
{
# Check column names:
if(!cn %in% colnames(x)) tstop("Column '",cn,"' is not in '", 
  deparse(substitute(x)),"', which has the columns:\n", toString(colnames(x)))
if(!"MESS_DATUM" %in% colnames(x)) tstop(deparse(substitute(x)),
                    " should contain the column 'MESS_DATUM'.")

# set graphical parameters:
op <- par(mar=mar, mgp=mgp, las=las)
if(!keeppar) on.exit(par(op), add=TRUE)

# Actual plot:
plot(x[,c("MESS_DATUM",cn)], type=type, lwd=lwd, col=col, 
     xaxt=if(monthaxis) "n" else "s", 
     xlab=xlab, ylab=ylab, main=main, panel.first=if(line0) abline(h=0), ...)
if(monthaxis) berryFunctions::monthAxis()
}
