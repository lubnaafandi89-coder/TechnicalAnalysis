# Linear regression window specified by the Assignment 5 pseudocode.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || length(regressionSource) == 0L || any(!is.finite(regressionSource))) {
    stop("regressionSource must be a non-empty finite numeric vector")
  }
  n <- length(regressionSource)
  if (length(regressionLength) != 1L || !is.finite(regressionLength) ||
      regressionLength < 2 || regressionLength != as.integer(regressionLength)) {
    stop("regressionLength must be an integer of at least 2")
  }
  if (regressionLength > n) stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (length(regressionOffset) != 1L || !is.finite(regressionOffset) ||
      regressionOffset < 0 || regressionOffset != as.integer(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }
  if (regressionOffset >= regressionLength) stop("regressionOffset must be less than regressionLength")
  # Offset moves the end of the regression window back from the latest value.
  end_index <- n - as.integer(regressionOffset)
  start_index <- max(1L, end_index - as.integer(regressionLength) + 1L)
  source_subset <- regressionSource[start_index:end_index]
  if (length(source_subset) < 2L) stop("At least two observations are required for linear regression")
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  list(slope = slope, intercept = intercept, predicted_values = predicted_values,
       source_indices = start_index:end_index)
}
