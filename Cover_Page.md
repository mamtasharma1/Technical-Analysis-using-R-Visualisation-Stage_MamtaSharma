# BDA400 — Data Science Tools and Techniques

## Assignment 6: Technical Analysis using R — Visualization Phase

**Student name:** [Enter your full name]  
**Course:** BDA400 — Data Science Tools and Techniques  
**Assignment:** Assignment 6 — Visualization Phase  
**Submission date:** [Enter submission date]  
**Assignment repository URL:** [Paste your public GitHub repository URL here]

### Project description

This project implements an interactive stock portfolio dashboard using R Shiny. The application retrieves historical stock prices from Yahoo Finance and allows users to select a ticker, date range, time interval, and price-chart style. It visualizes stock prices using line, area, or candlestick charts and includes optional 20-period and 50-period simple moving averages, RSI, and MACD. A moving-average crossover rule generates Buy and Sell markers; observations without a new crossover are labelled Hold. The dashboard also displays recent observations and a signal count summary.

### Files included

- `app.R` — complete Shiny dashboard source code.
- `README.md` — setup, run, and submission notes.

### How to run

Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp()` from the project directory. The application requires internet access to download market data from Yahoo Finance.

**Important:** Replace the placeholders above with your own details and add your real GitHub repository URL before submitting. The dashboard is for educational purposes and is not investment advice.
