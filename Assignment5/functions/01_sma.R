# Simple Moving Average (SMA) using base R only.
sma <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0L) stop("data must be a non-empty numeric vector")
  if (length(period) != 1L || !is.finite(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  period <- as.integer(period)
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  if (any(!is.finite(data))) stop("data must contain only finite numbers")
  out <- numeric(length(data) - period + 1L)
  for (i in seq_along(out)) out[i] <- sum(data[i:(i + period - 1L)]) / period
  out
}
