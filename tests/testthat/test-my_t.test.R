test_that("my_t.test works mathematically", {
  test_data <- c(0, 0, 0, 0, 1)
  test_mu <- mean(test_data)
  test_val <- my_t.test(test_data, "two.sided", test_mu)
  expect_equal(test_val$test_stat, 0)
  expect_equal(test_val$df, 4)
  expect_equal(test_val$alternative, "two.sided")
  expect_equal(test_val$p_val, 1)
})
