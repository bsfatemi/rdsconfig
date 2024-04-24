test_that("get brands index", {
  DT <- get_brands_index()
  expect_true(nrow(DT) > 1)
})
