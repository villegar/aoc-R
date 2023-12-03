test_that("day 03 - part a function achieves expected result", {
  x <- load_test_data("03a")

  expect_equal(f03a(x), 4361)
})

test_that("day 03 - part b function achieves expected result", {
  x <- load_test_data("03a") # same as in part a

  expect_equal(f03b(x), 467835)
})


