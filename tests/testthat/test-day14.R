test_that("day 14 - part 1 function achieves expected result", {
  x <- load_test_data("14")

  expect_equal(f14a(x), 136)
})

test_that("day 14 - part 2 function achieves expected result", {
  x <- load_test_data("14")

  expect_equal(f14b(x), 64)
})


