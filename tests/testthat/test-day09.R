test_that("day 09 - part 1 function achieves expected result", {
  x <- load_test_data("09")

  expect_equal(f09a(x), 114)
})

test_that("day 09 - part 2 function achieves expected result", {
  x <- load_test_data("09")

  expect_equal(f09b(x), 2)
})


