#' k nearest neighbors cross validation function
#'
#' This function predicts categories based on the k nearest neighbors and uses
#'   cross validation to indicate efficacy.
#'
#' @param train A data frame to predict classifications with.
#' @param cl The true classifications of each observation in \code{train}.
#' @param k_nn A numeric representing the number of neighbors to use.
#' @param k_cv A numeric representing the number of folds to use in cross validation.
#'
#' @return A list with two elements:
#'   \code{"class"}: the predicted class of each element of \code{train},
#'   \code{"cv_error"}: the cross validation error for the predictions.
#'
#' @examples
#' # I have spent an hour on this and can't get a working example
#'
#' @keywords prediction
#'
#' @export
#' @import class
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  folds <- data.frame(train, "fold" = fold)
  labeled_folds <- data.frame(folds, "cl" = cl)

  predictions <- list()
  cv_errors <- numeric(k_cv)

  for (i in 1:k_cv) {
    train2 <- (folds %>% filter(fold != i))[,-ncol(folds)]
    train2_names <- (labeled_folds %>% filter(fold != i))[,ncol(labeled_folds)]
    test2 <- (folds %>% filter(fold == i))[,-ncol(folds)]
    test2_names <- (labeled_folds %>% filter(fold == i))[,ncol(labeled_folds)]

    predictions[[i]] <- knn(train2, test2,
                            train2_names, k_nn)
    incorrect <- 0
    for (j in 1:length(predictions[[i]])) {
      if (test2_names[j] != predictions[[i]][j]) {
        incorrect <- incorrect + 1
      }
    }
    cv_errors[i] <- incorrect / length(predictions[[i]])
  }

  full <- knn(folds, folds, cl, k_nn)

  return(list("class" = full, "cv_error" = mean(cv_errors)))
}
