test_that("day 012 - part a function achieves expected result", {
  x <- load_test_data("02a")

  expect_equal(f02a(x), 8)
})

