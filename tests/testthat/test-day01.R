test_that("day 01 - part a function achieves expected result", {
  x <- load_test_data("01a")

  expect_equal(f01a(x), 142)
})

test_that("day 01 - part b function achieves expected result", {
  # x <- load_test_data("01b")
  #
  # expect_equal(f01a(x), 281)
})
