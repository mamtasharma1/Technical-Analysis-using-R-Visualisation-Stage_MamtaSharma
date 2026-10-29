# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R - Visualization Phase
# Portfolio dashboard built with Shiny, ggplot2, quantmod, and TTR.
#
# How to run:
# 1. Install R and RStudio.
# 2. Open this file in RStudio.
# 3. Run: shiny::runApp()
#    (or click "Run App" in RStudio).
#
# Note: Stock prices are downloaded from Yahoo Finance when the app runs.
# This project is for educational demonstration, not financial advice.

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(xts)
library(scales)

# Convert downloaded OHLC data to a regular data frame and calculate indicators.
prepare_stock_data <- function(symbol, start_date, end_date, timeframe) {
  raw <- tryCatch(
    suppressWarnings(
      getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = start_date,
        to = end_date + 1,
        auto.assign = FALSE
      )
    ),
    error = function(e) {
      stop(
        paste0(
          "Could not download data for ", symbol, ". Check the ticker, date range, ",
          "and internet connection. Details: ", conditionMessage(e)
        )
      )
    }
  )

  if (NROW(raw) < 2) {
    stop("Yahoo Finance returned too little data for this symbol and date range.")
  }

  # Adjusted close is used for indicators to account for stock splits/dividends.
  stock_df <- data.frame(
    Date = as.Date(index(raw)),
    Open = as.numeric(Op(raw)),
    High = as.numeric(Hi(raw)),
    Low = as.numeric(Lo(raw)),
    Close = as.numeric(Cl(raw)),
    Adjusted = as.numeric(Ad(raw)),
    Volume = as.numeric(Vo(raw))
  )
  stock_df <- stock_df[complete.cases(stock_df[, c("Date", "Open", "High", "Low", "Close", "Adjusted")]), ]

  # Aggregate observations when weekly or monthly views are selected.
  if (timeframe == "Weekly") {
    stock_xts <- xts(stock_df[, c("Open", "High", "Low", "Close", "Adjusted", "Volume")],
                     order.by = stock_df$Date)
    aggregated <- to.weekly(stock_xts, indexAt = "lastof", OHLC = FALSE)
    stock_df <- data.frame(
      Date = as.Date(index(aggregated)),
      Open = as.numeric(aggregated$Open),
      High = as.numeric(aggregated$High),
      Low = as.numeric(aggregated$Low),
      Close = as.numeric(aggregated$Close),
      Adjusted = as.numeric(aggregated$Adjusted),
      Volume = as.numeric(aggregated$Volume)
    )
  } else if (timeframe == "Monthly") {
    stock_xts <- xts(stock_df[, c("Open", "High", "Low", "Close", "Adjusted", "Volume")],
                     order.by = stock_df$Date)
    aggregated <- to.monthly(stock_xts, indexAt = "lastof", OHLC = FALSE)
    stock_df <- data.frame(
      Date = as.Date(index(aggregated)),
      Open = as.numeric(aggregated$Open),
      High = as.numeric(aggregated$High),
      Low = as.numeric(aggregated$Low),
      Close = as.numeric(aggregated$Close),
      Adjusted = as.numeric(aggregated$Adjusted),
      Volume = as.numeric(aggregated$Volume)
    )
  }

  stock_df <- stock_df[order(stock_df$Date), ]
  rownames(stock_df) <- NULL

  # Technical indicators: short/long simple moving averages, RSI, and MACD.
  stock_df$MA20 <- as.numeric(SMA(stock_df$Adjusted, n = 20))
  stock_df$MA50 <- as.numeric(SMA(stock_df$Adjusted, n = 50))
  stock_df$RSI14 <- as.numeric(RSI(stock_df$Adjusted, n = 14))
  macd_values <- MACD(stock_df$Adjusted, nFast = 12, nSlow = 26, nSig = 9,
                      maType = "EMA", percent = FALSE)
  stock_df$MACD <- as.numeric(macd_values[, 1])
  stock_df$MACDSignal <- as.numeric(macd_values[, 2])
  stock_df$MACDHistogram <- stock_df$MACD - stock_df$MACDSignal

  # Generate a signal only on the day a moving-average crossover occurs.
  # If no crossover occurs, the row is labelled Hold.
  stock_df$Signal <- "Hold"
  valid <- !is.na(stock_df$MA20) & !is.na(stock_df$MA50)
  above <- stock_df$MA20 > stock_df$MA50
  prior_above <- c(FALSE, head(above, -1))
  prior_valid <- c(FALSE, head(valid, -1))
  buy_rows <- valid & prior_valid & above & !prior_above
  sell_rows <- valid & prior_valid & !above & prior_above
  stock_df$Signal[buy_rows] <- "Buy"
  stock_df$Signal[sell_rows] <- "Sell"

  stock_df
}

# User interface: ticker, dates, chart style, timeframe, and indicator toggles.
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock ticker:", value = "AAPL"),
      dateRangeInput(
        "date_range", "Select date range:",
        start = Sys.Date() - 365, end = Sys.Date(),
        min = "2000-01-01", max = Sys.Date()
      ),
      selectInput(
        "time_frame", "Select time frame:",
        choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"
      ),
      selectInput(
        "chart_type", "Price chart type:",
        choices = c("Line", "Area", "Candlestick"), selected = "Line"
      ),
      checkboxGroupInput(
        "indicators", "Technical indicators:",
        choices = c("Moving Average 20" = "MA20",
                    "Moving Average 50" = "MA50",
                    "RSI (14)" = "RSI",
                    "MACD (12, 26, 9)" = "MACD"),
        selected = c("MA20", "MA50", "RSI", "MACD")
      ),
      helpText("Data source: Yahoo Finance. Use a valid market ticker such as AAPL, MSFT, or SHOP."),
      helpText("Buy/Sell markers show moving-average crossovers. Hold means no new crossover on that observation.")
    ),
    mainPanel(
      uiOutput("summary"),
      h3("Stock price and trading signals"),
      plotOutput("stock_chart", height = "450px"),
      conditionalPanel(
        condition = "input.indicators.indexOf('RSI') >= 0",
        h3("Relative Strength Index (RSI)"),
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.indicators.indexOf('MACD') >= 0",
        h3("Moving Average Convergence Divergence (MACD)"),
        plotOutput("macd_chart", height = "250px")
      ),
      h3("Recent observations"),
      tableOutput("recent_data"),
      h3("Signal summary"),
      tableOutput("signal_summary")
    )
  )
)

server <- function(input, output, session) {

  stock_data <- reactive({
    req(input$symbol, input$date_range)
    validate(
      need(nchar(trimws(input$symbol)) > 0, "Please enter a stock ticker."),
      need(input$date_range[1] <= input$date_range[2], "Start date must be before end date.")
    )
    tryCatch(
      prepare_stock_data(
        toupper(trimws(input$symbol)),
        as.Date(input$date_range[1]),
        as.Date(input$date_range[2]),
        input$time_frame
      ),
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
      }
    )
  })

  output$summary <- renderUI({
    df <- stock_data()
    latest <- tail(df, 1)
    tagList(
      p(strong("Ticker: "), toupper(trimws(input$symbol)),
        " | ", strong("Latest observation: "), format(latest$Date, "%Y-%m-%d"),
        " | ", strong("Adjusted close: "),
        dollar(latest$Adjusted, accuracy = 0.01)),
      p("Charts and indicators update when the inputs are changed.")
    )
  })

  output$stock_chart <- renderPlot({
    df <- stock_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Adjusted), alpha = 0.25) +
        geom_line(aes(y = Adjusted), linewidth = 0.55)
    } else if (input$chart_type == "Candlestick") {
      # Green candles represent a close >= open; red candles represent a close < open.
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      p <- ggplot(df, aes(x = Date)) +
        geom_segment(aes(y = Low, yend = High, xend = Date),
                     linewidth = 0.45, color = "grey35") +
        geom_rect(aes(
          xmin = Date - 0.35, xmax = Date + 0.35,
          ymin = pmin(Open, Close), ymax = pmax(Open, Close),
          fill = Direction
        ), color = "grey30", linewidth = 0.25) +
        scale_fill_manual(values = c("Up" = "#238b45", "Down" = "#cb181d")) +
        labs(fill = "Direction")
    } else {
      p <- p + geom_line(aes(y = Adjusted), linewidth = 0.65)
    }

    if ("MA20" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA20, color = "MA 20"), linewidth = 0.65, na.rm = TRUE)
    }
    if ("MA50" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA50, color = "MA 50"), linewidth = 0.65, na.rm = TRUE)
    }

    # Annotate crossover events on the price chart.
    signal_points <- df[df$Signal %in% c("Buy", "Sell"), ]
    if (nrow(signal_points) > 0) {
      p <- p +
        geom_point(data = signal_points,
                   aes(y = Adjusted, color = Signal, shape = Signal), size = 2.8) +
        geom_text(data = signal_points,
                  aes(y = Adjusted, label = Signal, color = Signal),
                  vjust = -0.9, size = 3.2, show.legend = FALSE)
    }

    p + scale_color_manual(
      values = c("MA 20" = "#2171b5", "MA 50" = "#f16913",
                 "Buy" = "#238b45", "Sell" = "#cb181d"),
      breaks = c("MA 20", "MA 50", "Buy", "Sell")
    ) +
      labs(
        title = paste(toupper(trimws(input$symbol)), "price chart"),
        x = "Date", y = "Price (USD)", color = "Series"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$indicators)
    df <- stock_data()
    ggplot(df, aes(x = Date, y = RSI14)) +
      geom_line(linewidth = 0.65) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed",
                 color = "grey45") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(title = "RSI (14) — reference levels at 30 and 70",
           x = "Date", y = "RSI") +
      theme_minimal(base_size = 12)
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$indicators)
    df <- stock_data()
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACDHistogram), alpha = 0.35) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.65, na.rm = TRUE) +
      geom_line(aes(y = MACDSignal, color = "Signal line"), linewidth = 0.65, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = c("MACD" = "#2171b5", "Signal line" = "#f16913")) +
      labs(title = "MACD (12, 26, 9)", x = "Date", y = "MACD", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$recent_data <- renderTable({
    df <- stock_data()
    display <- tail(df[, c("Date", "Open", "High", "Low", "Close",
                           "Adjusted", "MA20", "MA50", "RSI14", "Signal")], 10)
    display$Date <- format(display$Date, "%Y-%m-%d")
    numeric_cols <- c("Open", "High", "Low", "Close", "Adjusted", "MA20", "MA50", "RSI14")
    display[numeric_cols] <- lapply(display[numeric_cols], function(x) round(x, 2))
    display
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$signal_summary <- renderTable({
    df <- stock_data()
    counts <- table(factor(df$Signal, levels = c("Buy", "Sell", "Hold")))
    data.frame(
      Signal = names(counts),
      Count = as.integer(counts),
      Explanation = c(
        "Short MA crosses above long MA",
        "Short MA crosses below long MA",
        "No new crossover on this observation"
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
