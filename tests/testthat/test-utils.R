test_that("testing sample functions", {
  orgs <- sampleOrgs(5)
  usrs <- sampleUsers(5)
  expect_length(orgs, 5)
  expect_length(usrs, 5)
  expect_error(..check_uuid(orgs))
  expect_error(..check_uuid(usrs))
  expect_identical(sapply(orgs, ..check_uuid, USE.NAMES = FALSE), orgs)
  expect_identical(sapply(usrs, ..check_uuid, USE.NAMES = FALSE), usrs)
})


