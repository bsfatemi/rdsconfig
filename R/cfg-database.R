#' Functions that Query the Database
#'
#' @param extra_cols A logical indicating if new function columns should be returned
#' @param org org guid
#' @param store store uuid
#' @param .api API to use for auth
#'
#' @import data.table
#' @importFrom stringr str_glue
#' @importFrom DBI dbGetQuery dbReadTable
#' @importFrom rpgconn dbc dbd
#'
#' @name cfg-database
NULL


#' @describeIn cfg-database users
#' @export
userIndex <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))
  qry <- "SELECT username, user_uuid useruuid, org_uuid orguuid FROM hca_users"
  as.data.table(DBI::dbGetQuery(conn, qry))
}


#' @describeIn cfg-database users
#' @export
orgIndex <- function(extra_cols = FALSE) {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))

  extra_cols_str <- ""
  if (extra_cols) {
    extra_cols_str <- ", language"
  }
  as.data.table(DBI::dbGetQuery(conn, stringr::str_glue("
    SELECT
      LHS.org_uuid AS org_uuid, short_name, full_name, org_tier, market_areas, tz, has_deliver,
      has_retail, agency, in_population in_pop, current_client curr_client{extra_cols_str}
    FROM (
      (
        SELECT
          org_uuid, short_name, full_name, tier org_tier, market_areas, timezone tz,
          has_delivery has_deliver, has_retail, agency{extra_cols_str}
        FROM org_info
      ) LHS
      INNER JOIN
      (SELECT org_uuid, current_client, in_population FROM org_pipelines_info) RHS
      ON (LHS.org_uuid = RHS.org_uuid)
    )
  ")))
}


#' @describeIn cfg-database get polaris config
#' @export
get_polaris_config <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))

  setDT(DBI::dbGetQuery(conn, stringr::str_glue(
    "
    SELECT
      org_uuid, sms_service, optins_policy, util_capacity, reset_day, mms_enabled, mms_ratio, keyopps,
      abandon_cart, loyalty, beta_tester, custom_upload, emailing_enabled
    FROM org_polaris_config
    "
  )))[]
}

#' @describeIn cfg-database get polaris admins
#' @export
get_polaris_admins <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))

  setDT(DBI::dbGetQuery(conn, stringr::str_glue(
    "SELECT org_uuid, phone_num FROM org_polaris_admins"
  )))[]
}

#' @describeIn cfg-database look up orgs twil credentials given org Id
org_auth <- function(org, .api) {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))
  res <- DBI::dbGetQuery(conn, stringr::str_glue(
    "
    SELECT messaging_profile, username, key, token
    FROM org_credentials_telnyx
    WHERE org_uuid = '{org}'
    "
  ))
  # We only handle Telnyx auths in our databases.
  if (.api == "telnyx" && nrow(res) == 1) {
    list(
      key = res$key,
      mp = res$messaging_profile,
      token = res$token,
      usr = res$username
    )
  }
}

#' @describeIn cfg-database Gets all Stores set in our database.
#' @export
get_stores <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))
  qry <- "SELECT org_uuid, store_uuid, short_name, full_name, main_pos FROM org_stores"
  DT <- setDT(DBI::dbGetQuery(conn, qry))[]
  setnames(DT, "short_name", "facility")
  DT[]
}


#' @describeIn cfg-database pipelines index
#' @export
plIndex <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))
  setDT(DBI::dbGetQuery(
    conn,
    "
    SELECT LHS.org_uuid AS org_uuid, short_name,
    is_active, customers, orders, order_lines, pos_data
    FROM (
      (SELECT org_uuid, short_name FROM org_info) LHS
      INNER JOIN
      (
        SELECT
          org_uuid, is_active,
          consolidated_customers customers,
          consolidated_orders orders,
          consolidated_order_lines order_lines,
          pos_data
        FROM org_pipelines_info
      ) RHS
      ON (LHS.org_uuid = RHS.org_uuid)
    )
    "))[]
}

#' @describeIn cfg-database pipelines index for appdata
#' @export
plAppDataIndex <- function() {
  conn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(conn))
  setDT(DBI::dbGetQuery(
    conn,
    "
    SELECT
      LHS.org_uuid AS org_uuid, short_name, current_client, is_active, in_population,
      polaris_is_active, polaris_incl_no_orders, polaris_manual_pl, sirius_is_active,
      greenscreens_is_active, pos_optins, pos_data
    FROM (
      (SELECT org_uuid, short_name FROM org_info) LHS
      INNER JOIN
      (
        SELECT
          org_uuid, current_client, is_active, in_population, polaris_is_active,
          polaris_incl_no_orders, polaris_manual_pl, sirius_is_active, greenscreens_is_active,
          pos_optins, pos_data
        FROM org_pipelines_info
      ) RHS
      ON (LHS.org_uuid = RHS.org_uuid)
    )
    "))[]
}

#' @describeIn cfg-database get index of brands
#' @export
get_brands_index <- function() {
  conn <- rpgconn::dbc(db = "integrated")
  on.exit(rpgconn::dbd(conn))
  setDT(DBI::dbReadTable(conn, "brands_master_index"), key = "brand_name")[]
}


#' @describeIn cfg-database pos system index
#' @export
erpSysIndex <- function() {
  cn <- rpgconn::dbc(db = "hcaconfig")
  on.exit(rpgconn::dbd(cn))
  as.data.table(DBI::dbReadTable(cn, "pos_sys_index"))
}


#' @describeIn cfg-database get list of products for a retail location
get_product_skus <- function(org, store) {
  cn <- rpgconn::dbc(db = "appdata")
  on.exit(rpgconn::dbd(cn))
  qry <- stringr::str_glue(
    "SELECT product_sku
      FROM vindex_product_velocity_daily
      WHERE org_uuid = '{org}'
      AND store_uuid = '{store}'"
  )
  dbGetQuery(cn, qry)$product_sku
}
