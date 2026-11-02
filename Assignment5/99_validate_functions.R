# Run from the Assignment5 folder. These are compact deterministic checks of
# known examples and the required output shapes for all nine indicator functions.
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

stopifnot(isTRUE(all.equal(sma(1:5, 3), c(2, 3, 4))))
stopifnot(isTRUE(all.equal(ema(1:3, 3), c(1, 1.5, 2.25))))
macd_example <- macd(1:10, 3, 5, 2)
stopifnot(length(macd_example$macd_line) == 10L,
          length(macd_example$signal_line) == 10L,
          isTRUE(all.equal(macd_example$histogram,
                           macd_example$macd_line - macd_example$signal_line)))
stopifnot(isTRUE(all.equal(stdev(c(2, 4, 4, 4, 5, 5, 7, 9)), 2)))
fit <- linreg(c(2, 4, 6, 8, 10), 3, 0)
stopifnot(isTRUE(all.equal(fit$slope, 2)), isTRUE(all.equal(fit$intercept, 4)),
          isTRUE(all.equal(fit$predicted_values, c(6, 8, 10))))
rsi_example <- rsi(c(1, 2, 3, 4), 2)
stopifnot(is.na(rsi_example[1]), is.na(rsi_example[2]),
          isTRUE(all.equal(rsi_example[3:4], c(100, 100))))
stoch_example <- stoch_rsi(c(1, 2, 3, 4, 5), 2, 1, 1)
stopifnot(length(stoch_example$k_line) == 5L,
          length(stoch_example$d_line) == 5L,
          all(is.finite(stoch_example$k_line[4:5])),
          all(is.finite(stoch_example$d_line[4:5])))
stopifnot(identical(crossover(c(1, 2, 1), c(2, 1, 2)), c("None", "Up", "Down")))
stopifnot(identical(crossunder(c(2, 1, 2), c(1, 2, 1)), c("None", "True", "False")))

sample_stock <- data.frame(
  Date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04")),
  Close = c(10, 12, 9, 10), Volume = c(100, 200, 150, 180),
  SMA20_R = c(9, 11, 9, 10), SMA50_R = c(10, 10, 10, 11)
)
filtered <- filter_stocks(sample_stock, start_date = "2026-01-02",
                           end_date = "2026-01-04", close_above_sma = TRUE)
stopifnot(nrow(filtered) == 1L, filtered$Date[1] == as.Date("2026-01-02"))
ruled <- apply_sma_trading_rule(sample_stock)
stopifnot(identical(ruled$SMA_Cross_Event, c("None", "Up", "Down", "None")),
          identical(ruled$Position, c("Flat", "Long", "Flat", "Flat")))

cat("All Assignment 5 indicator checks passed.\n")
