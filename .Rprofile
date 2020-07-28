try(installB::checkOutdated("rdwd"), silent=TRUE)
try(installB::loadAndMessage("rdwd"), silent=TRUE)

rhubcheck <- function() rhub::check_for_cran(env_vars=c(`_R_CHECK_FORCE_SUGGESTS_`="false"), 
                                             show_status=FALSE)
