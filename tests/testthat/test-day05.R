test_that("day 05 - part 1 function achieves expected result", {
  x <- load_test_data("05")

  expect_equal(f05a(x), 35)
})

test_that("day 05 - part 2 function achieves expected result", {
  x <- load_test_data("05")

  expect_equal(f05b(x), 46)
})


