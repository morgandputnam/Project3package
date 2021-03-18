test_that("my_lm works mathematically", {
  test_x <- c(5, 23, 10, 8, 17, 25, 19, 23, 2, 11)
  test_y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  test_val <- my_lm(test_y ~ test_x, data.frame("x" = test_x, "y" = test_y))
  expect_equal(test_val["Estimate"]["x"], as.double(NA))
})
