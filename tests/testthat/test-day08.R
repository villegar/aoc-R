test_that("day 08 - part 1 function achieves expected result", {
  x <- load_test_data("08a")

  expect_equal(f08a(x), 2)
})

test_that("day 08 - part 2 function achieves expected result", {
  x <- load_test_data("08b")

  expect_equal(f08b(x), 6)
})


