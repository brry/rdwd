try(installB::checkOutdated("rdwd"), silent=TRUE)
try(installB::loadAndMessage("rdwd"), silent=TRUE)
if(grepl("MacBook",Sys.info()["nodename"])) options(rdwdlocdir="/Users/berry/Desktop/DWDdata")

crancheck <- function() 
 {
 message("Submitting to rhub...")
 rhub::check_for_cran(env_vars=c(`_R_CHECK_FORCE_SUGGESTS_`="false"), show_status=FALSE)
 message("Submitting to win-builder...")
 devtools::check_win_devel(quiet=TRUE)
 message("Done. Results will come in by email.")
 }
