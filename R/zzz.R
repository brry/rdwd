#' @title global quiet option for rdwd
#' @description global quiet option. The default `rdwdquiet()` is `FALSE`.\cr
#' Just write the following in your code and all subsequent calls will be quiet:\cr
#' `options(rdwdquiet=TRUE)`
#' @export
rdwdquiet <- function()
{
cv <- getOption("rdwdquiet", default=FALSE) # current value
if(!(isTRUE(cv)|isFALSE(cv))) stop("options('rdwdquiet') must be TRUE or FALSE, not '",
                                 toString(cv), "'.")
cv
}



.onLoad <- function(libname, pkgname)
{
options(rdwdquiet=FALSE)
invisible(NULL)
}

