test_that("testing cfg-users", {

  UINDEX <- userIndex()
  expect_true(nrow(UINDEX) > 0)
  expect_true(length(UINDEX) == 3)

  uid <- UINDEX[1, useruuid]

  ll <- userOrg(usr = uid)

  expect_length(ll, 12)

  ll <- userScope(usr = uid, .app = "polaris")
  expect_length(ll, 4)

  expect_named(userAccount(usr = uid), c("useruuid", "username"))
  expect_named(userIntegrations(usr = uid), c("org_uuid", "erp_uuid"))
})
