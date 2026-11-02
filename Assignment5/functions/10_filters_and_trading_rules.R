# Date/indicator filters and an SMA crossover position rule, using base R only.
filter_stocks <- function(stock_data, start_date = NULL, end_date = NULL,
                          close_above_sma = FALSE, min_volume = NULL) {
  required <- c("Date", "Close", "Volume")
  if (!all(required %in% names(stock_data))) stop("stock_data must contain Date, Close, and Volume")
  keep <- rep(TRUE, nrow(stock_data))
  if (!is.null(start_date)) keep <- keep & as.Date(stock_data$Date) >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & as.Date(stock_data$Date) <= as.Date(end_date)
  if (close_above_sma) {
    if (!"SMA20_R" %in% names(stock_data)) stop("SMA20_R is required when close_above_sma = TRUE")
    keep <- keep & is.finite(stock_data$SMA20_R) & stock_data$Close > stock_data$SMA20_R
  }
  if (!is.null(min_volume)) {
    if (length(min_volume) != 1L || !is.finite(min_volume) || min_volume < 0) stop("min_volume must be non-negative")
    keep <- keep & is.finite(stock_data$Volume) & stock_data$Volume >= min_volume
  }
  stock_data[keep, , drop = FALSE]
}

apply_sma_trading_rule <- function(stock_data) {
  if (!all(c("SMA20_R", "SMA50_R") %in% names(stock_data))) {
    stop("stock_data must contain SMA20_R and SMA50_R")
  }
  events <- crossover(stock_data$SMA20_R, stock_data$SMA50_R)
  position <- character(nrow(stock_data))
  state <- "Flat"
  for (i in seq_len(nrow(stock_data))) {
    if (events[i] == "Up") state <- "Long"
    if (events[i] == "Down") state <- "Flat"
    position[i] <- state
  }
  stock_data$SMA_Cross_Event <- events
  stock_data$Position <- position
  stock_data
}
