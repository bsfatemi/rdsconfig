test_that("Testing index functions v2", {
  expect_equal(
    names(orgIndex()),
    c(
      "org_uuid", "short_name", "full_name", "org_tier", "market_areas", "tz", "has_deliver",
      "has_retail", "agency", "in_pop", "curr_client"
    )
  )

  expect_equal(
    names(userIndex()),
    c("username", "useruuid", "orguuid")
  )
})


test_that("testing userAuth", {
  skip_on_ci()
  uid <- userIndex()[1, useruuid]
  expect_true(is.character(uid) & length(uid) == 1)
})

test_that("testing user* functions", {

  uids <- userIndex()
  lapply(uids[c(1, nrow(uids))]$useruuid, function(uid) {
    ll <- userAccount(usr = uid)
    expect_length(ll, 2)
    expect_vector(ll)
    expect_named(ll, expected = c("useruuid", "username"))

    ll <- userIntegrations(usr = uid)
    expect_length(ll, 2)
    expect_vector(ll)
    expect_named(ll, expected = c("org_uuid", "erp_uuid"))

    ll <- userModsEnabled(usr = uid)
    expect_vector(ll)
    expect_length(ll, 11)
    expect_named(
      ll, c(
        "sys_uuid", "type", "short_name", "full_name", "irb_class", "has_products", "has_brands",
        "has_inventory", "has_logistics", "has_optins", "html_url"
      )
    )

    ll <- userOrg(usr = uid)
    expect_true(all(
      c(
        "org_uuid", "short_name", "full_name", "org_tier", "market_areas", "tz", "has_deliver",
        "has_retail", "agency", "in_pop", "curr_client", "stores"
      ) %in% names(ll)
    ))

    ll <- userScope(usr = uid, .app = "polaris")
    expect_identical(names(ll), c("admin", "usage", "ai_mods", "addons"))
  })
})


test_that("testing getStoreID", {
  uuid <- "579c39f7-fe97-4803-ab70-6fe45dfe0e77" # hightimes
  store_id <- get_store_id(uuid, store = "broadway")
  expect_vector(store_id)
})
