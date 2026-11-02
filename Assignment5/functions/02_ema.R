# Exponential Moving Average (EMA) using the assignment's recursive formula.
ema <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0L) stop("data must be a non-empty numeric vector")
  if (length(period) != 1L || !is.finite(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  if (any(!is.finite(data))) stop("data must contain only finite numbers")
  multiplier <- 2 / (as.integer(period) + 1)
  values <- numeric(length(data))
  values[1] <- data[1]
  if (length(data) > 1L) {
    for (i in 2:length(data)) values[i] <- (data[i] - values[i - 1L]) * multiplier + values[i - 1L]
  }
  values
}
