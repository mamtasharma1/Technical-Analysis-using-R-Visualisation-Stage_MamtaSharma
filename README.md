# BDA400 Assignment 6 — Technical Analysis using R

## Overview

An interactive R Shiny stock dashboard built to meet the Assignment 6 visualization-phase requirements. Data is fetched from Yahoo Finance through `quantmod`.

## Features

- User-entered stock ticker (default: AAPL)
- User-selected date range
- Daily, weekly, and monthly views
- Line, area, and candlestick price charts
- Optional 20-period and 50-period simple moving averages
- RSI (14), including reference lines at 30 and 70
- MACD (12, 26, 9), signal line, and histogram
- Moving-average crossover Buy/Sell annotations and Hold labels for observations without a new crossover
- Recent-observation table and signal summary
- Basic validation and download error messages

## Requirements and setup

1. Install R and preferably RStudio.
2. Open `app.R`. The script checks for required packages and installs any missing packages.
3. Ensure the computer has internet access so Yahoo Finance data can be downloaded.
4. Run the app from RStudio using **Run App**, or run the following in the R console from this folder:

   ```r
   shiny::runApp()
   ```

## Trading-rule interpretation

The 20-period simple moving average is treated as the short-term average and the 50-period simple moving average as the long-term average. A Buy signal is produced when the short average crosses from at/below to above the long average. A Sell signal is produced when it crosses from at/above to below the long average. Otherwise, the observation is labelled Hold. Moving averages require enough observations to become available, so the earliest rows may not produce crossover signals.

## Submission checklist

- [ ] Added the name and submission details to `Cover_Page.md`.
- [ ] Created a public GitHub repository for the assignment.
- [ ] Uploaded `app.R`, `README.md`, and the completed cover page.

## Limitations

This is an educational technical-analysis demonstration. It does not place trades, account for transaction costs, or guarantee future performance. Yahoo Finance availability and ticker coverage can change. Weekly and monthly views aggregate OHLC values before calculating indicators.
