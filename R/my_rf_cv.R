#' random forest cross validation function for the palmerpenguins penguins data
#'
#' This functions determines the cross validation error for modeling body_mass_g
#'   using bill_depth_mm, bill_length_mm, and flipper_length_mm with random trees.
#'
#' @param k Numeric representing the number of folds to use in cross validation.
#'
#' @return a numeric representing the cross validation error.
#'
#' @export
my_rf_cv <- function(k) {
  penguins <- my_penguins
  penguin_df <- penguins[-c(1, 2, 7, 8)] # drop unwanted fields from penguin
  penguin_df <- penguin_df[-c(4, 272),] # these two rows are NA

  # random grouping
  fold <- sample(rep(1:k, length = nrow(penguin_df)))
  folds <- data.frame(penguin_df, "fold" = fold)

  for (i in 1:k) {
    test1 <- folds %>% filter(fold == i)
    train1 <- folds %>% filter(fold != i)

    MODEL <- randomForest(body_mass_g ~ bill_depth_mm +
                            bill_length_mm + flipper_length_mm,
                          data = train1, ntree = 100)

    PREDICTIONS <- predict(MODEL, test1[,-c(4,5)])

    cv_error <- mean((PREDICTIONS - test1$body_mass_g)^2)
  }

  return(cv_error)
}
