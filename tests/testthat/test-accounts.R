test_that("id syntax and validity", {
  ix <- userIndex()
  right <- ix[1, get("useruuid")]
  obj <- userAccount(usr = right)
  expect_true(is.list(obj))
  expect_length(obj, 2)
  right_last <- ix[nrow(ix), get("useruuid")]
  obj_last <- userAccount(usr = right_last)
  expect_true(is.list(obj_last))
  expect_length(obj_last, 2)
  expect_error(userAccount(usr = "blah"), "invalid uuid argument")
  expect_error(userAccount(usr = ix[1, get("orguuid")]), "user not found")
})

test_that("testing orguuid:shortname uniqueness v2", {
  dt <- userIndex()
  len <- dt[, .N, by = c("useruuid", "orguuid", "username")][N > 1]
  expect_true(nrow(len) == 0)
})
