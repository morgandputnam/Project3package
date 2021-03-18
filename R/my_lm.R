#' Linear model function
#'
#' This function calculates a regression model.
#'
#' @param formula The formula object to be tested
#' @param data a numeric data frame representing a data set with the variables in
#'   \code{formula}.
#'
#' @return a list representing the linear model for \code{data} with the given
#'   \code{formula} containing:
#'   Estimate: the estimated coefficients for \code{formula},
#'   Std. Error: the average error of the predicted model,
#'   t value: a numeric representing the t-value of each estimated coefficient,
#'   Pr(>|t|) the probability of a more extreme estimated coefficient.
#'
#' @examples
#' y <- c(2, 5, 6, 8, 9)
#' x <- c(1, 2, 3, 4, 5)
#' example.df <- data.frame("y" = y, "x" = x)
#' my_lm(y ~ x, example.df)
#' my_lm(x ~ y, example.df)
#'
#' @keywords prediction
#'
#' @export
#' @importFrom stats model.frame model.matrix model.response predict pt sd
my_lm <- function(formula, data) {
  x <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  y <- model.response(frame)
  # Estimated coefficients
  beta_hats <- solve(t(x) %*% x) %*% t(x) %*% y
  d_f <- nrow(data) - nrow(beta_hats)
  variance_hat <- sum((y - (x %*% beta_hats))^2) / d_f
  # Std. Error
  s_e <- diag(sqrt(variance_hat * solve(t(x) %*% x)))
  # t values
  t_vals <- beta_hats / s_e
  # Pr(>|t|)
  p_vals <- 2 * pt(abs(t_vals), d_f, lower.tail = FALSE)

  # put values into a matrix and convert to a table
  results <- matrix(c(beta_hats, s_e, t_vals, p_vals), ncol = 4)
  colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(results) <- rownames(beta_hats)
  return(as.table(results))
}
