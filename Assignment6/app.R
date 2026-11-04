# Assignment 6: Technical Analysis Using R - Visualization Stage
# Student: Lubna Afandi
# Course: CA-BDA400 - Data Science Tools and Techniques
# Data source: Yahoo Finance through the quantmod package
#
# First-time setup only: run this command in the R Console if needed:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR"))
#
# Section 1: Load packages required for data retrieval, indicators, charts, and Shiny.
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# Section 2: Build the dashboard interface and its interactive controls.
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #F5F7F4; color: #263B36; }
      .main-header { color: #1E5148; font-weight: 700; }
      .well { background-color: #E8F0ED; border-color: #C8D8D1; }
      .control-label { color: #244F46; }
      h4 { color: #244F46; }
    "))
  ),

  titlePanel(
    div("Lubna's Technical Analysis Dashboard", class = "main-header")
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "symbol", "Select portfolio stock:",
        choices = c("NVIDIA (NVDA)" = "NVDA",
                    "Amazon (AMZN)" = "AMZN",
                    "Tesla (TSLA)" = "TSLA")
      ),

      dateRangeInput(
        "date_range", "Select date range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        min = Sys.Date() - (5 * 365),
        max = Sys.Date()
      ),

      selectInput(
        "time_frame", "Select time frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),

      selectInput(
        "chart_type", "Select price chart:",
        choices = c("Line", "Area", "Candlestick")
      ),

      checkboxGroupInput(
        "indicators", "Show indicators:",
        choices = c("20-period moving average" = "SMA20",
                    "50-period moving average" = "SMA50",
                    "Relative Strength Index (14)" = "RSI",
                    "Moving Average Convergence Divergence" = "MACD"),
        selected = c("SMA20", "SMA50")
      ),

      numericInput("short_ma", "Short moving-average period:", 20, min = 2, max = 200),
      numericInput("long_ma", "Long moving-average period:", 50, min = 3, max = 300),

      helpText("For crossover signals, the short period must be smaller than the long period.")
    ),

    mainPanel(
      h4(textOutput("signal_summary")),
      plotOutput("price_chart", height = "460px"),

      conditionalPanel(
        "input.indicators.indexOf('RSI') >= 0",
        h4("Relative Strength Index"),
        plotOutput("rsi_chart", height = "230px")
      ),

      conditionalPanel(
        "input.indicators.indexOf('MACD') >= 0",
        h4("Moving Average Convergence Divergence"),
        plotOutput("macd_chart", height = "230px")
      )
    )
  )
)

# Section 3: Retrieve and prepare Yahoo Finance data based on dashboard selections.
server <- function(input, output, session) {

  stock_data <- reactive({
    req(input$symbol, input$date_range)

    # Request additional historical prices so long-period indicators can be calculated.
    fetch_from <- input$date_range[1] - 1825
    fetch_to <- input$date_range[2] + 1

    tryCatch(
      getSymbols(
        input$symbol,
        src = "yahoo",
        from = fetch_from,
        to = fetch_to,
        auto.assign = FALSE
      ),
      error = function(e) NULL
    )
  })

  analysis_data <- reactive({
    # Validate the crossover settings before calculating signals.
    validate(
      need(input$short_ma < input$long_ma,
           "The short moving-average period must be smaller than the long period.")
    )

    x <- stock_data()
    validate(
      need(!is.null(x),
           "Stock data could not be downloaded. Check the internet connection and try again.")
    )

    # Aggregate daily prices when the user selects weekly or monthly observations.
    if (input$time_frame == "Weekly") {
      x <- to.weekly(x, indexAt = "lastof", OHLC = TRUE)
    } else if (input$time_frame == "Monthly") {
      x <- to.monthly(x, indexAt = "lastof", OHLC = TRUE)
    }

    # Convert the selected OHLCV series into a data frame for plotting and calculations.
    d <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(Op(x)),
      High = as.numeric(Hi(x)),
      Low = as.numeric(Lo(x)),
      Close = as.numeric(Cl(x)),
      Volume = as.numeric(Vo(x))
    )

    # Calculate moving averages, RSI, and MACD from closing prices.
    d$SMA20 <- as.numeric(SMA(d$Close, n = 20))
    d$SMA50 <- as.numeric(SMA(d$Close, n = 50))
    d$ShortMA <- as.numeric(SMA(d$Close, n = input$short_ma))
    d$LongMA <- as.numeric(SMA(d$Close, n = input$long_ma))
    d$RSI <- as.numeric(RSI(d$Close, n = 14))

    macd_values <- MACD(d$Close, nFast = 12, nSlow = 26, nSig = 9)
    d$MACD <- as.numeric(macd_values[, 1])
    d$MACDSignal <- as.numeric(macd_values[, 2])
    d$MACDHistogram <- d$MACD - d$MACDSignal

    # Generate Buy and Sell signals when the short average crosses the long average.
    # Label other valid observations as Hold.
    previous_short <- c(NA, head(d$ShortMA, -1))
    previous_long <- c(NA, head(d$LongMA, -1))

    d$Signal <- ifelse(
      is.na(d$ShortMA) | is.na(d$LongMA) |
        is.na(previous_short) | is.na(previous_long),
      NA,
      ifelse(
        d$ShortMA > d$LongMA & previous_short <= previous_long, "Buy",
        ifelse(
          d$ShortMA < d$LongMA & previous_short >= previous_long, "Sell",
          "Hold"
        )
      )
    )

    d
  })

  # Limit calculations to the date interval selected by the user.
  visible_data <- reactive({
    d <- analysis_data()
    d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
  })

  # Section 4: Show the latest calculated Buy, Sell, or Hold status.
  output$signal_summary <- renderText({
    d <- visible_data()
    validate(need(nrow(d) > 0, "No data is available for the selected dates."))

    valid_signals <- d$Signal[!is.na(d$Signal)]
    if (length(valid_signals) == 0) {
      return("Latest trading signal: Not enough data to calculate a signal.")
    }

    paste(input$symbol, "- latest trading signal:", tail(valid_signals, 1))
  })

  # Render the selected price chart and overlay the enabled moving averages.
  # Buy and Sell crossover points are labeled directly on the chart.
  output$price_chart <- renderPlot({
    d <- visible_data()
    validate(need(nrow(d) > 0, "No data is available for the selected dates."))

    p <- ggplot(d, aes(x = Date))

    if (input$chart_type == "Candlestick") {
      candle_width <- if (input$time_frame == "Daily") 0.35 else
        if (input$time_frame == "Weekly") 2 else 7

      p <- p +
        geom_segment(aes(y = Low, yend = High), color = "#52635D") +
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "#52635D"
        ) +
        scale_fill_manual(
          values = c("TRUE" = "#1E8B72", "FALSE" = "#C56A55"),
          guide = "none"
        )
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "#79B5A3", alpha = 0.55)
    } else {
      p <- p + geom_line(aes(y = Close), color = "#1E5148", linewidth = 0.85)
    }

    if ("SMA20" %in% input$indicators) {
      p <- p + geom_line(aes(y = SMA20, color = "20-period MA"), linewidth = 0.85)
    }

    if ("SMA50" %in% input$indicators) {
      p <- p + geom_line(aes(y = SMA50, color = "50-period MA"), linewidth = 0.85)
    }

    signal_points <- d[!is.na(d$Signal) & d$Signal %in% c("Buy", "Sell"), ]

    p +
      geom_point(
        data = signal_points,
        aes(y = Close, color = Signal),
        size = 2.6
      ) +
      geom_text(
        data = signal_points,
        aes(y = Close, label = Signal, color = Signal),
        vjust = -0.9,
        size = 3.5,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c("20-period MA" = "#C69A28",
                   "50-period MA" = "#3B7B70",
                   "Buy" = "#16815F",
                   "Sell" = "#B94D49")
      ) +
      labs(
        title = paste(input$symbol, "Price and Crossover Signals"),
        x = "Date",
        y = "Price (USD)",
        color = "Chart key"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(color = "#244F46", face = "bold"),
        legend.title = element_text(color = "#244F46")
      )
  })

  # Display the RSI oscillator with common 30 and 70 reference levels.
  output$rsi_chart <- renderPlot({
    d <- visible_data()
    validate(need(nrow(d) > 0, "No data is available for the selected dates."))

    ggplot(d, aes(x = Date, y = RSI)) +
      geom_line(color = "#3B7B70", linewidth = 0.85) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed",
                 color = "#C69A28") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Date", y = "RSI (14)") +
      theme_minimal(base_size = 11)
  })

  # Display the MACD line, signal line, and histogram as a separate indicator panel.
  output$macd_chart <- renderPlot({
    d <- visible_data()
    validate(need(nrow(d) > 0, "No data is available for the selected dates."))

    ggplot(d, aes(x = Date)) +
      geom_col(
        aes(y = MACDHistogram, fill = MACDHistogram >= 0),
        show.legend = FALSE
      ) +
      geom_line(aes(y = MACD), color = "#1E5148", linewidth = 0.85) +
      geom_line(aes(y = MACDSignal), color = "#C69A28", linewidth = 0.85) +
      scale_fill_manual(values = c("TRUE" = "#79B5A3", "FALSE" = "#D58B76")) +
      labs(x = "Date", y = "MACD") +
      theme_minimal(base_size = 11)
  })
}

# Section 5: Start the interactive Shiny application.
shinyApp(ui = ui, server = server)

