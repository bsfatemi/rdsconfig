#' System functions
#'
#' @param erp erp system uuid
#'
#' @import data.table
#'
#' @name cfg_systems
NULL

#' @describeIn cfg_systems TBD
#' @export
erpSystemInfo <- function(erp) {
  ..check_uuid(erp)
  as.list(erpSysIndex()[sys_uuid == erp])
}

#' @describeIn cfg_systems listing (index)
#' @export
lookup_erp_uuid <- function(erp = NULL){
  erpSysIndex()[short_name == erp, sys_uuid]
}
