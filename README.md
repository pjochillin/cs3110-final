# Stock Analysis Tool

## Description
This OCaml project provides a graphical user interface (GUI) for analyzing stock data. Users can enter a stock ticker, and the application will display various financial indicators and recent news related to the stock. This tool is designed to assist investors and financial analysts in making informed decisions based on technical analysis and current events.

## Features
- **Stock Graphs**: Visual representation of the stock's price movement over time.
- **Stock Information Sources**: Gathers information for each ticker based on user selection:
  - AlphaVantage
  - Polygon.io
  - Twelve Data
  - APIStocks
- **Technical Indicators**: Generates graphs for:
  - Relative Strength Index (RSI)
  - Average True Range (ATR)
  - On-Balance Volume (OBV)
  - Moving Average Convergence Divergence (MACD)
  - Commodity Channel Index (CCI)
  - Bollinger Bands
  - Stochastic Oscillator
- **Stock News**: Fetches and displays the latest news articles related to the stock.

## Installation
1. **Clone the repository**:
   ```bash
   git clone [URL to the repository]
   cd [repository name]
   ```

2. **Install dependencies (Mac)**:
   ```bash
   sudo apt install cmake pkg-config
   brew install plplot
   opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf
   ```
   2. **Install dependencies (Windows)**:
   ```bash
   opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf
   ```

## Usage
To run the application, execute:
```bash
dune build
dune exec bin/main.exe
```
Once the GUI is open, enter a stock ticker in the input field and press the "Get Data" button to view the stock's data and related news.

## Authors
- Joshua Ochalek (jo447)
- Arnav Tevatia (at846)
- Krish Mehra (km937)
