# Assignment 5: Technical Analysis Using R

**Student:** Lubna Afandi  
**Course:** BDA400 - Data Science Tools and Techniques  
**Repository:** [TechnicalAnalysis](https://github.com/lubnaafandi89-coder/TechnicalAnalysis)

## Contents

- `functions/01_sma.R` through `functions/09_crossunder.R`: one base-R script per required indicator.
- `functions/10_filters_and_trading_rules.R`: example date/price filter and SMA crossover position rule.
- `00_prepare_inputs.R`: downloads daily OHLCV inputs for NVDA, AMZN, and TSLA. quantmod is used only for data import.
- `00_run_assignment5.R`: applies the base-R functions to those inputs and writes analysis CSV outputs.
- `99_validate_functions.R`: optional function validation script.
- `outputs/`: destination for the generated technical-analysis, filtered-signal, and summary CSV files.
- `Afandi_Lubna_CA_BDA400_A05.pdf`: project overview and run guide.

## Input data

Run `source("00_prepare_inputs.R")` once to retrieve daily OHLCV data for NVDA, AMZN, and TSLA into `../Assignment2/outputs/`. It uses quantmod only to import market data. The indicator implementations use base R. Each CSV contains `Date`, `Open`, `High`, `Low`, `Close`, and `Volume`. Keep the `Assignment2` and `Assignment5` folders side by side in the repository.

All indicator calculations use base R; no technical-analysis package is used.

## Run in RStudio

1. Open the repository project in RStudio and set the working directory to the `Assignment5` folder.
2. Run `source("00_prepare_inputs.R")` to create the three OHLCV input CSVs.
3. Run `source("99_validate_functions.R")` to check the indicator functions.
4. Run `source("00_run_assignment5.R")`.
5. Review the summary printed in the Console and the CSV files created under `Assignment5/outputs/`.

## Calculation notes

- SMA returns complete rolling windows; the runner pads the preceding rows with `NA` to preserve dates.
- EMA follows the recursive smoothing formula and seeds its first value with the first observation.
- `stdev()` uses population standard deviation (division by *n*), as specified in the assignment.
- RSI uses Wilder smoothing; StochRSI normalizes RSI against rolling extrema and smooths %K and %D.
- Crossover/crossunder functions return aligned signals while preserving unavailable comparisons.
- The example filter retains the latest 90 calendar days where close is above SMA20. The trading rule enters a long position at an upward SMA20/SMA50 cross and returns to flat at a downward cross.
