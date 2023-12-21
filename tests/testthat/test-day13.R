test_that("day 13 - part 1 function achieves expected result", {
  x <- load_test_data("13")

  expect_equal(f13a(x), 405)
})

test_that("day 13 - part 2 function achieves expected result", {
  x <- load_test_data("13")

  expect_equal(f13b(x), 400)
})


