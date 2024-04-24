test_that("Testing org query functions", {

  expect_no_error(lookupOrgGuid(name = "medithrive"))

  oid <- lookupOrgGuid(name = "medithrive")

  expect_equal(oid, ..check_uuid(oid))
  expect_true(is.data.table(orgIndex()))

  expect_equal(orgShortName(org = oid), "medithrive")

  expect_equal(orgTimeZone(org = oid), "America/Los_Angeles")

  expect_true(nrow(get_org_stores(org = oid)) == 1)

  sid <- get_org_stores(org = oid)$store_uuid

  expect_equal(get_store_id(org = oid, store = "main"), sid)

  cols <- c("org_uuid", "store_uuid", "org_short", "store_short", "org_full",
            "org_markets", "org_tz", "store_pos")
  expect_equal(names(orgIndexFull()), cols)

  orgScope(org = oid, .app = "polaris")


})
