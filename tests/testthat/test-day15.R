test_that("day 15 - part 1 function achieves expected result", {
  x <- load_test_data("15")

  expect_equal(f15a(x), 1320)
})

test_that("day 15 - part 2 function achieves expected result", {
  x <- load_test_data("15")

  expect_equal(f15b(x), 145)
})


