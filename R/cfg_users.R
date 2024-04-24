#' Users
#'
#' @param usr user uuid
#' @param .app TBD
#' @param .api TBD
#' @param extra_cols A logical indicating if new function columns should be returned
#'
#' @name cfg_users
NULL


#' @describeIn cfg_users TBD
#' @export
userOrg <- function(usr, extra_cols = FALSE) {
  ..check_uuid(usr)
  org <- userIndex()[get("useruuid") == usr, get("orguuid")]
  orgInfo(org, extra_cols)
}


#' @describeIn cfg_users TBD
#' @export
userScope <- function(usr, .app = "polaris") {
  ..check_uuid(usr)
  org <- userIndex()[get("useruuid") == usr, get("orguuid")]
  orgScope(org, .app)
}


#' @describeIn cfg_users TBD
#' @export
userAuth <- function(usr, .api = "twil") {
  ..check_uuid(usr)
  OID <- userIndex()[get("useruuid") == usr, get("orguuid")]
  orgAuth(OID, .api)
}


#' @describeIn cfg_users TBD
#' @export
userAccount <- function(usr) {
  ..check_uuid(usr)
  userTable <- userIndex()
  usr_row <- userTable[get("useruuid") == usr]
  nrow(usr_row) || stop("user not found")
  list("useruuid" = unlist(usr), "username" = usr_row[, get("username")])
}


#' @describeIn cfg_users TBD
#' @export
userIntegrations <- function(usr) {
  ..check_uuid(usr)
  org <- userIndex()[get("useruuid") == usr, get("orguuid")]
  orgIntegrations(org)
}


#' @describeIn cfg_users TBD
#' @export
userModsEnabled <- function(usr) {
  ..check_uuid(usr)
  pl <- userIntegrations(usr)
  pl_info_cat <- erpSystemInfo(pl$erp_uuid)
  pl_info_cat
}


