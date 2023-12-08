test_that("day 07 - part 1 function achieves expected result", {
  x <- load_test_data("07")

  expect_equal(f07a(x), 6440)
})

test_that("day 07 - part 2 function achieves expected result", {
  x <- load_test_data("07")

  expect_equal(f07b(x), 5905)
})


