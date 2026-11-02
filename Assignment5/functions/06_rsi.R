# Relative Strength Index (RSI) with Wilder smoothing, using base R only.
rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) < 2L || any(!is.finite(data))) {
    stop("data must be a finite numeric vector with at least two values")
  }
  if (length(period) != 1L || !is.finite(period) || period < 1 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  period <- as.integer(period)
  if (length(data) <= period) stop("data must contain more observations than period")
  changes <- diff(data)
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  rsi_values <- rep(NA_real_, length(data))
  avg_gain <- sum(gains[seq_len(period)]) / period
  avg_loss <- sum(losses[seq_len(period)]) / period
  calculate_rsi <- function(gain, loss) {
    if (loss == 0 && gain == 0) return(50)
    if (loss == 0) return(100)
    if (gain == 0) return(0)
    100 - (100 / (1 + gain / loss))
  }
  rsi_values[period + 1L] <- calculate_rsi(avg_gain, avg_loss)
  if (length(data) > period + 1L) {
    for (i in (period + 2L):length(data)) {
      change_index <- i - 1L
      avg_gain <- (avg_gain * (period - 1) + gains[change_index]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[change_index]) / period
      rsi_values[i] <- calculate_rsi(avg_gain, avg_loss)
    }
  }
  rsi_values
}
