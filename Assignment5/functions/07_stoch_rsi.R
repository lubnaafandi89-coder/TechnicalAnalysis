# Stochastic RSI: rolling normalization followed by SMA-smoothed %K and %D.
# Uses rsi() and sma() from 06_rsi.R and 01_sma.R.
stoch_rsi <- function(data, period, k_period, d_period) {
  valid_period <- function(x) length(x) == 1L && is.finite(x) && x >= 1 && x == as.integer(x)
  if (!valid_period(period) || !valid_period(k_period) || !valid_period(d_period)) {
    stop("period, k_period, and d_period must be positive integers")
  }
  rsi_values <- rsi(data, as.integer(period))
  period <- as.integer(period)
  k_period <- as.integer(k_period)
  d_period <- as.integer(d_period)
  raw_k <- rep(NA_real_, length(rsi_values))
  first_valid <- which(is.finite(rsi_values))[1]
  if (is.na(first_valid)) stop("Not enough data to calculate RSI")
  if (length(rsi_values) - first_valid + 1L >= period) {
    for (end_idx in seq.int(first_valid + period - 1L, length(rsi_values))) {
      window <- rsi_values[(end_idx - period + 1L):end_idx]
      low <- min(window)
      high <- max(window)
      if (high == low) raw_k[end_idx] <- 0 else raw_k[end_idx] <- (rsi_values[end_idx] - low) / (high - low)
    }
  }
  k_line <- rep(NA_real_, length(raw_k))
  raw_indices <- which(is.finite(raw_k))
  if (length(raw_indices) >= k_period) {
    smoothed_k <- sma(raw_k[raw_indices], k_period)
    k_line[raw_indices[k_period]:tail(raw_indices, 1L)] <- smoothed_k
  }
  d_line <- rep(NA_real_, length(k_line))
  valid_k <- which(is.finite(k_line))
  if (length(valid_k) >= d_period) {
    smoothed_d <- sma(k_line[valid_k], d_period)
    d_line[valid_k[d_period]:tail(valid_k, 1L)] <- smoothed_d
  }
  list(k_line = k_line, d_line = d_line)
}
