# Moving Average Convergence Divergence (MACD), using ema() from 02_ema.R.
macd <- function(data, short_period, long_period, signal_period) {
  periods <- c(short_period, long_period, signal_period)
  if (length(periods) != 3L || any(!is.finite(periods)) || any(periods < 1) ||
      any(periods != as.integer(periods))) stop("All periods must be positive integers")
  if (short_period >= long_period) stop("short_period must be less than long_period")
  short_ema <- ema(data, as.integer(short_period))
  long_ema <- ema(data, as.integer(long_period))
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, as.integer(signal_period))
  list(macd_line = macd_line, signal_line = signal_line,
       histogram = macd_line - signal_line)
}
