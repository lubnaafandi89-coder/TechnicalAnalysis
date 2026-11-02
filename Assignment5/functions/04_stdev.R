# Population standard deviation, following the assignment's division by n.
stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0L) stop("data must be a non-empty numeric vector")
  if (any(!is.finite(data))) stop("data must contain only finite numbers")
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  sqrt(variance)
}
