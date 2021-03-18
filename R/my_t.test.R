#' t-test function
#'
#' This function runs a t-test on a set of data.
#'
#' @param x Numeric vector representing a data set to conduct a t-test on.
#' @param alternative The alternative to be tested with: either \code{"greater"},
#'   \code{"less"}, or \code{"two.sided"}.
#' @param mu A numeric representing the assumed mean of what \code{x} is a sample of.
#'
#' @return A list with 4 elements:
#'   test-stat: a numeric representing the hypothesized t-score of sample \code{x},
#'   df: a numeric representing the degrees of freedom of \code{x},
#'     alternative: the passed in \code{alternative},
#'   p-val: a numeric representing the probability of a sample as extreme as
#'     \code{x} occurring.
#'
#' @examples
#'  my_t.test(c(1, 2, 3, 4, 5), "two.sided", 0)
#'  my_t.test(x = 1:50, "greater", 10)
#'
#' @keywords inference
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # throw an error if invalid alternative
  if (alternative != "two.sided" &
      alternative != "less" & alternative != "greater") {
    stop("alternative must be \"two.sided\", \"less\", or \"greater\"")
  }
  s_e <- sd(x) / sqrt(length(x))
  d_f <- length(x) - 1
  t_score <- (mean(x) - mu) / s_e
  # p_val dependent on alternative
  if (alternative == "two.sided") {
    p_val <- 2 * pt(abs(t_score), d_f, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- pt(t_score, d_f, lower.tail = TRUE)
  } else {
    p_val <- pt(t_score, d_f, lower.tail = FALSE)
  }
  results <- list("test_stat" = t_score,
                  "df" = d_f,
                  "alternative" = alternative,
                  "p_val" = p_val)
  return(results)
}
