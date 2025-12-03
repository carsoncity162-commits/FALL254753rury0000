test_that("my ncurve function works", {
  results <- myncurve(0,3,0)
  expect_equal(results$mu, 0)
  expect_equal(results$sigma, 3)
  expect_equal(results$probability, 0.5, tolerance = 1e-4)
})
