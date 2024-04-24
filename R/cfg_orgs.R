#' org functions
#'
#' @param name org name
#' @param org org guid
#' @param .app TBD
#' @param .api default is 'twil'
#' @param store store_short
#' @param extra_cols A logical indicating if new function columns should be returned
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' lookupOrgGuid(name = "amuse")
#' }
#'
#' @name cfg_orgs
NULL


#' @describeIn cfg_orgs look up orgs guid given its' shortname. Returns NA if shortname not found
#' @export
lookupOrgGuid <- function(name) {
  setkey(orgIndex(), short_name)[(name), org_uuid]
}


#' @describeIn cfg_orgs look up orgs twil credentials given org Id
#' @export
orgAuth <- function(org, .api) {
  ..check_uuid(org)
  auth <- org_auth(org, .api)
  return(auth)
}


#' @describeIn cfg_orgs look up orgs twil credentials given org Id
#' @export
orgScope <- function(org, .app) {
  polaris_config <- get_polaris_config()[org_uuid == org, !"org_uuid"]
  polaris_admins <- get_polaris_admins()[org_uuid == org, .(phone_num)]

  # We only handle Polaris scope in our databases.
  if (.app == "polaris" && nrow(polaris_config) == 1) {
    scope <- list(
      admin = polaris_admins$phone_num,
      usage = list(
        sms_srvc = polaris_config$sms_service,
        opt_policy = polaris_config$optins_policy,
        util_cap = polaris_config$util_capacity,
        reset_md = polaris_config$reset_day,
        mms_enable = polaris_config$mms_enabled,
        mms_ratio = polaris_config$mms_ratio,
        beta_tester = polaris_config$beta_tester,
        custom_upload = polaris_config$custom_upload,
        emailing_enabled = polaris_config$emailing_enabled
      ),
      ai_mods = list(keyopps = polaris_config$keyopps),
      addons = list(
        abandon_cart = polaris_config$abandon_cart,
        loyalty = polaris_config$loyalty
      )
    )
  }
  # Fail if scope was not found.
  length(scope) > 0 || stop("Scope not found for provided org uuid and .app")
  return(scope)
}


#' @describeIn cfg_orgs look up orgs twil credentials given org Id
orgInfo <- function(org, extra_cols = FALSE) {
  info <- org_info(org, extra_cols)
  length(info) > 0 || stop("Org info not found for provided uuid")
  return(info)
}

#' @describeIn cfg_orgs look up orgs twil credentials given org Id
org_info <- function(org, extra_cols = FALSE) {
  org_index <- orgIndex(extra_cols)[org_uuid == org]
  org_stores <- get_stores()
  org_stores <- org_stores[org_stores$org_uuid == org, ]
  if (nrow(org_index) == 1 && nrow(org_stores) > 0) {
    res <- as.list(org_index)
    ll <- as.list(org_stores$store_uuid)
    names(ll) <- org_stores$short_name
    res$stores <- ll
    res
  }
}


#' @describeIn cfg_orgs look up orgs twil credentials given org Id
orgIntegrations <- function(org) {
  integrations <- org_integrations(org)
  length(integrations) > 0 || stop("Org integrations not found for provided uuid")
  return(integrations)
}

#' @describeIn cfg_orgs orgs
#' @export
org_integrations <- function(org) {
  res <- orgIndexFull()[org_uuid == org, .N, .(org_uuid, store_pos)]
  if (nrow(res) == 0) stop("org not found")
  list(org_uuid = res$org_uuid[1], erp_uuid = lookup_erp_uuid(res$store_pos[1]))
}



#' @describeIn cfg_orgs orgs
#' @export
orgShortName <- function(org = NULL) {
  orgIndex()[org_uuid == ..check_uuid(org), short_name]
}


#' @describeIn cfg_orgs orgs
#' @export
orgTimeZone <- function(org = NULL) {
  orgIndex()[org_uuid == ..check_uuid(org), tz]
}


#' @describeIn cfg_orgs orgs
#' @export
get_store_id <- function(org, store) {
  store_id <- get_store_id_from_db(org, store)
  length(store_id) > 0 || stop("Store not found for provided org uuid")
  return(store_id)
}

#' @describeIn cfg_orgs orgs
get_store_id_from_db <- function(org, store) {
  stores <- get_stores()
  stores[org_uuid == org & facility == store, store_uuid]
}


#' @describeIn cfg_orgs orgs
#' @export
get_org_stores <- function(org = NULL) {
  stores <- get_stores()
  if (is.null(org))
    return(stores[])
  return(stores[org_uuid == org])
}

#' @describeIn cfg_orgs org and store index
#' @export
orgIndexFull <- function() {
  setcolorder(
    orgIndex()[
      (curr_client), .(
        org_short = short_name,
        org_full = full_name,
        org_markets = market_areas,
        org_tz = tz
      ), key = org_uuid ][
        setnames(
          setDT(get_stores(), key = "org_uuid")[, !4],
          old = 3:4,
          new = c("store_short", "store_pos")
        ),
        nomatch = NULL
      ],
    c(1, 6, 2, 7))[]
}


