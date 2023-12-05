test_that("day 04 - part 1 function achieves expected result", {
  x <- load_test_data("04a")

  expect_equal(f04a(x), 13)
})

test_that("day 04 - part 2 function achieves expected result", {
  x <- load_test_data("04a") # same as in part 1

  expect_equal(f04b(x), 30)
})


