test_that("day 11 - part 1 function achieves expected result", {
  x <- load_test_data("11")

  expect_equal(f11a(x), 374)
})

test_that("day 11 - part 2 function achieves expected result", {
  x <- load_test_data("11")

  expect_equal(f11b(x), 82000210)
})


