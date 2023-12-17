test_that("day 10 - part 1 function achieves expected result", {
  x <- load_test_data("10a")

  expect_equal(f10a(x), 4)
})

test_that("day 10 - part 2 function achieves expected result", {
  x <- load_test_data("10b")

  expect_equal(f10b(x), 4)
})


