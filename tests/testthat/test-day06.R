test_that("day 06 - part 1 function achieves expected result", {
  x <- load_test_data("06")

  expect_equal(f06a(x), 288)
})

test_that("day 06 - part 2 function achieves expected result", {
  x <- load_test_data("06")

  expect_equal(f06b(x), 71503)
})


