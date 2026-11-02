# Assignment 2: Technical Analysis Using R
# Student: Lubna Afandi

if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
library(quantmod)

# Read stock symbols from portfolio.txt
symbols <- trimws(readLines("portfolio.txt", warn = FALSE))
symbols <- symbols[nzchar(symbols)]

if (length(symbols) == 0) {
  stop("No stock symbols were found in portfolio.txt.")
}

# Return the most frequent value
calculate_mode <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Download stock data from Yahoo Finance
download_stock_data <- function(symbol) {
  tryCatch(
    getSymbols(
      symbol,
      src = "yahoo",
      from = as.Date("2024-01-01"),
      auto.assign = FALSE
    ),
    error = function(e) {
      message("Could not download ", symbol, ": ", conditionMessage(e))
      NULL
    }
  )
}

results <- data.frame(
  Symbol = character(),
  Observations = integer(),
  Mean = numeric(),
  Mode = numeric(),
  Median = numeric(),
  Standard_Deviation = numeric()
)

for (symbol in symbols) {
  stock_data <- download_stock_data(symbol)

  if (is.null(stock_data)) {
    next
  }

  adjusted_prices <- as.numeric(Ad(stock_data))
  valid_prices <- adjusted_prices[is.finite(adjusted_prices)]

  if (length(valid_prices) == 0) {
    next
  }

  results <- rbind(
    results,
    data.frame(
      Symbol = symbol,
      Observations = length(valid_prices),
      Mean = mean(valid_prices),
      Mode = calculate_mode(valid_prices),
      Median = median(valid_prices),
      Standard_Deviation = if (length(valid_prices) > 1) {
        sd(valid_prices)
      } else {
        NA_real_
      }
    )
  )

  # Plot adjusted closing price and 20-day moving average
  moving_average_20 <- SMA(Ad(stock_data), n = 20)

  plot(
    index(Ad(stock_data)),
    adjusted_prices,
    type = "l",
    col = "darkgreen",
    xlab = "Date",
    ylab = "Adjusted closing price (USD)",
    main = paste(symbol, "Adjusted Closing Price")
  )

  lines(
    index(moving_average_20),
    as.numeric(moving_average_20),
    col = "blue",
    lwd = 2
  )

  legend(
    "topleft",
    legend = c("Adjusted price", "20-day moving average"),
    col = c("darkgreen", "blue"),
    lty = 1,
    bty = "n"
  )
}

print(results)

if (nrow(results) > 0) {
  write.csv(results, "summary_statistics.csv", row.names = FALSE)
}
