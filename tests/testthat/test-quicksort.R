test_that("quicksort sorts", {
  set.seed(123)
  x <- sample.int(100)
  expect_equal(quicksort(x), sort(x))
})
