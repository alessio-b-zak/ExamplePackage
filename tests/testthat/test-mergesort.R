
test_that("mergesort sorts", {
  set.seed(123)
  x <- sample.int(100)
  expect_equal(mergesort(x), sort(x))
})
