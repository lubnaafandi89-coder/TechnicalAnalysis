# Run this file from the Assignment5 folder in RStudio.
# It uses only base R for Assignment 5 indicator calculations.
source("functions/01_sma.R")
source("functions/02_ema.R")
source("functions/03_macd.R")
source("functions/04_stdev.R")
source("functions/05_linreg.R")
source("functions/06_rsi.R")
source("functions/07_stoch_rsi.R")
source("functions/08_crossover.R")
source("functions/09_crossunder.R")
source("functions/10_filters_and_trading_rules.R")

data_dir <- file.path("..", "Assignment2", "outputs")
output_dir <- "outputs"
if (!dir.exists(data_dir)) stop("Cannot find ../Assignment2/outputs. Keep Assignment2 and Assignment5 side by side.")
if (!dir.exists(output_dir)) dir.create(output_dir)

align_sma <- function(x, period) c(rep(NA_real_, period - 1L), sma(x, period))
symbols <- c("NVDA", "AMZN", "TSLA")
summary_rows <- list()

for (symbol in symbols) {
  input_file <- file.path(data_dir, paste0(symbol, "_data.csv"))
  if (!file.exists(input_file)) stop("Missing input file: ", input_file)
  stock <- read.csv(input_file, stringsAsFactors = FALSE)
  required <- c("Date", "Open", "High", "Low", "Close", "Volume")
  if (!all(required %in% names(stock))) stop(symbol, " CSV is missing required OHLCV columns")
  stock$Date <- as.Date(stock$Date)
  stock <- stock[order(stock$Date), , drop = FALSE]
  close <- as.numeric(stock$Close)

  stock$SMA20_R <- align_sma(close, 20L)
  stock$SMA50_R <- align_sma(close, 50L)
  stock$EMA12_R <- ema(close, 12L)
  stock$EMA26_R <- ema(close, 26L)
  macd_result <- macd(close, 12L, 26L, 9L)
  stock$MACD <- macd_result$macd_line
  stock$MACD_Signal <- macd_result$signal_line
  stock$MACD_Histogram <- macd_result$histogram
  stock$RSI14 <- rsi(close, 14L)
  stoch_result <- stoch_rsi(close, 14L, 14L, 3L)
  stock$StochRSI_K <- stoch_result$k_line
  stock$StochRSI_D <- stoch_result$d_line
  stock$Stdev20 <- c(rep(NA_real_, 19L), vapply(20:nrow(stock), function(i) stdev(close[(i - 19L):i]), numeric(1)))
  regression <- lapply(seq_len(nrow(stock)), function(i) {
    if (i < 20L) return(NA_real_)
    fit <- linreg(close[seq_len(i)], 20L, 0L)
    tail(fit$predicted_values, 1L)
  })
  stock$LinReg20 <- unlist(regression, use.names = FALSE)
  stock$Crossunder_20_50 <- crossunder(stock$SMA20_R, stock$SMA50_R)
  stock <- apply_sma_trading_rule(stock)
  stock$Symbol <- symbol

  # Example filter: observations in the latest 90 calendar days with close above SMA20.
  last_date <- max(stock$Date, na.rm = TRUE)
  filtered <- filter_stocks(stock, start_date = last_date - 89, end_date = last_date,
                            close_above_sma = TRUE)
  write.csv(stock, file.path(output_dir, paste0(symbol, "_technical_analysis.csv")), row.names = FALSE)
  write.csv(filtered, file.path(output_dir, paste0(symbol, "_filtered_signals.csv")), row.names = FALSE)

  summary_rows[[symbol]] <- data.frame(
    Symbol = symbol,
    Rows = nrow(stock),
    Latest_Date = as.character(tail(stock$Date, 1L)),
    Latest_Close = tail(stock$Close, 1L),
    Latest_SMA20 = tail(stock$SMA20_R, 1L),
    Latest_RSI14 = tail(stock$RSI14, 1L),
    Latest_MACD = tail(stock$MACD, 1L),
    Latest_Position = tail(stock$Position, 1L),
    Filtered_Rows_Last_90_Days = nrow(filtered),
    stringsAsFactors = FALSE
  )
}

summary_table <- do.call(rbind, summary_rows)
write.csv(summary_table, file.path(output_dir, "assignment5_summary.csv"), row.names = FALSE)
print(summary_table)
cat("Assignment 5 calculations completed. Output files are in Assignment5/outputs.\n")
