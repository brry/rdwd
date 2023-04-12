#' @title Launch interactive weather analysis app
#' @description Launch interactive analysis of weather period comparison 
#'              for different RDWD stations.
#'              The R session is blocked during usage, close the app to re-enable
#'              console usage.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, July 2018 + April 2023
#' @seealso [shiny::runApp()], [rdwd]
#' @keywords iplot
#' @importFrom berryFunctions tstop
#' @export
#' @examples
#' # app()
#' @param \dots Arguments passed to [shiny::runApp()]
#'
app <- function(...)
{
checkSuggestedPackage("shiny", "rdwd::app")
appDir <- system.file("shinyapps/compare_years", package="rdwd")
if(appDir=="") tstop("Could not find directory 'shinyapps/compare_years'. Try re-installing 'rdwd'.")
shiny::runApp(appDir, ...)
}
