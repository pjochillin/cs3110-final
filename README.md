Stock Analysis Tool

Description
This OCaml project provides a graphical user interface (GUI) for analyzing stock data. Users can enter a stock ticker, and the application will display various financial indicators and recent news related to the stock. This tool aims to assist investors and financial analysts in making informed decisions based on technical analysis and current events.

Features
Stock Graphs: Visual representation of the stock's price movement over time.
Technical Indicators: Generates graphs for:
Relative Strength Index (RSI)
Average True Range (ATR)
On-Balance Volume (OBV)
Moving Average Convergence Divergence (MACD)
Commodity Channel Index (CCI)
Bollinger Bands
Stochastic Oscillator
Stock News: Fetches and displays the latest news articles related to the stock.

Installation
Clone the repository:
bash
Copy code
git clone [URL to the repository]
cd [repository name]

Install dependencies:
On Mac : brew install plplot
opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf
dune build

To run the application, execute:
dune exec bin/main.exe

Once the GUI is open, enter a stock ticker in the input field and press the "Get Data" button to view the stock's data and related news.

Authors
Joshua Ochalek (jo447), Arnav Tevatia (at846), Krish Mehra (km937)
