test_that("day 02 - part a function achieves expected result", {
  x <- load_test_data("02a")

  expect_equal(f02a(x), 8)
})

test_that("day 02 - part b function achieves expected result", {
  x <- load_test_data("02a") # same as in part a

  expect_equal(f02b(x), 2286)
})


