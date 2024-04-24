#' Utilities
#'
#' @param id uuid to check format
#' @param n sample size
#' @param resample replace each?
#' @param ... additional params to function sample
#'
#' @importFrom stringr str_split str_count str_detect
#'
#' @name utils
NULL

#' @describeIn utils check format for valid uuid
#' @export
..check_uuid <- function(id) {
  pat <- "[0-9a-zA-Z]{8}\\-([0-9a-zA-Z\\-]{4}){3}"
  (sum(unlist(lapply(stringr::str_split(id, "-"), stringr::str_count))) == 32 &&
      stringr::str_detect(id, pat)) || stop("invalid uuid argument")
  invisible(id)
}


#' @describeIn utils testing functions
#' @export
sampleOrgs <- function(n = 5, resample = FALSE, ...) {
  sample(orgIndex()[, get("org_uuid")], size = n, replace = resample, ...)
}

#' @describeIn utils testing functions
#' @export
sampleUsers <- function(n = 5, resample = FALSE, ...) {
  sample(userIndex()[, get("useruuid")], size = n, replace = resample, ...)
}


#' @describeIn utils testing functions
#' @export
sampleLocation <- function() {
  oid <- sampleOrgs(1)
  sid <- get_org_stores(oid)$store_uuid
  list(oid = oid, sid = sample(sid, 1))
}

#' @describeIn utils testing functions
#' @export
sampleLocationSku <- function() {
  ll <- sampleLocation()
  get_product_skus(ll$oid, ll$sid)
}

