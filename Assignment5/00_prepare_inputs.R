# Retrieve OHLCV data for the Assignment 5 portfolio.
# quantmod is used only for Yahoo Finance data import; indicators use base R.
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}

symbols <- c("NVDA", "AMZN", "TSLA")
data_dir <- file.path("..", "Assignment2", "outputs")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

for (symbol in symbols) {
  message("Downloading daily data for ", symbol)
  prices <- quantmod::getSymbols(symbol, src = "yahoo", from = "2023-01-01",
                                 auto.assign = FALSE)
  output <- data.frame(
    Date = as.Date(zoo::index(prices)),
    Open = as.numeric(quantmod::Op(prices)),
    High = as.numeric(quantmod::Hi(prices)),
    Low = as.numeric(quantmod::Lo(prices)),
    Close = as.numeric(quantmod::Cl(prices)),
    Volume = as.numeric(quantmod::Vo(prices))
  )
  write.csv(output, file.path(data_dir, paste0(symbol, "_data.csv")), row.names = FALSE)
}
cat("OHLCV input files saved in Assignment2/outputs.\n")
