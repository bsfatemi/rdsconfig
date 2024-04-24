test_that("All orgs have uuid & name v2", {
  ODT <- orgIndex()
  expect_true(ODT[is.na(org_uuid) | is.na(short_name), .N] == 0)
  expect_true(
    all(unique(ODT[, sapply(.SD, class)]) %in% c("character", "integer", "logical"))
  )
})


test_that("checking for expected columns in orgIndex table v2", {
  ODT <- orgIndex()

  cols <- c(
    "org_uuid",
    "short_name",
    "full_name",
    "org_tier",
    "market_areas",
    "tz",
    "has_deliver",
    "has_retail"
  )
  expect_no_error(ODT[, cols, with = FALSE])

})

test_that("checking for NA values across all columns in orgIndex table v2", {
  ODT <- orgIndex()
  cols <- c("short_name", "full_name", "org_tier",
            "market_areas", "tz", "has_retail", "has_deliver")
  expect_true(ODT[, all(!sapply(.SD, is.na)), .SDcols = cols])
})

test_that("test org tzs v2", {
  ODT <- orgIndex()
  expect_true(ODT[, all(unique(tz) %in% OlsonNames())])
})


test_that("testing org_integrations lookup", {
  uuid <- "66fba771-0880-4dbc-9dbb-3a463fb4a21f"
  orgll <- userOrg(uuid)
  orgid <- orgll$org_uuid
  expect_equal(orgid, "174ba423-593e-4431-9e4d-a912cdac3362")
  out <- org_integrations(orgid)

  expect_true(is.list(out))
  expect_true(length(out) == 2)
  expect_named(out, c("org_uuid", "erp_uuid"))
})
