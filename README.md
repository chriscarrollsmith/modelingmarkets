# wallstreetpettingzoo
Financial and economic analysis and data visualization

By Christopher C. Smith

INTRODUCTION

Wall Street Petting Zoo is a weekly investing podcast co-hosted by Robert Coburn and Christopher Carroll Smith. Wall Street Petting Zoo is available through YouTube, Spotify, Apple Podcasts, Stitcher, and Podbean. The Wall Street Petting Zoo Github repository will compile code for financial/economic analysis and data visualization. Code will be created primarily in R and Python. Code is copyrighted by Christopher Carroll Smith, but is free to use with proper acknowledgements under terms of the Apache 2.0 license. Sample output is included for each script.

SCRIPTS FOUND IN THIS REPOSITORY

10yr-yield-forecaster.Rmd - Uses multiple regression models trained with five-fold cross-validation to forecast the 10-year US Treasury yield and S&P 500 P/E ratio from date and PCE inflation rate.

C19SectorReturns.R - Charts stock market returns by equal weight S&P 500 sector since the beginning of the Covid-19 pandemic using data from Yahoo Finance.

DownFriday-DownMonday.R - Tests Jeffrey Hirsch's "Down Friday + Down Monday" indicator to see if DF+DM day pairs are more commonly associated with market inflection points than other day-pairs. Uses split-adjusted $SPY price data from Yahoo Finance and tests for robustness against 24 different model specifications. Effect size is positive in all model specifications. Effect size is larger on shorter time frames (5-10 days) and appears weaker when analysis is extended to 15-20 days. Measured magnitude of excess inflection is <1% of index price in all specifications. "Excess inflection" associated with the DF+DM signal is proportionally largest immediately after the signal appears. 3 days after the signal, magnitude of inflection is 18-43% larger than the magnitude of inflection of the average non-DF+DM day-pair. It hits a minimum 18 days after the signal, when magnitude of inflection is only 0.5-13% larger than the magnitude of inflection of the average non-DF+DM day pair.

em-us-bond-spread.R - Script to chart the ratio between the VWOB (emerging markets) and GOVT (United States) sovereign bond ETFs. Since these two ETFs have similar duration, the ratio between them makes a useful proxy for the relative cheapness or expensiveness of US vs. emerging markets sovereign bonds.

historicalvolume.R - Pulls stock price and historical volume data from Yahoo Finance via the tidyquant library and constructs a profile of historical volume. Inspired by TradingView's "volume profile," this charting technique seeks to identify price levels where a lot of historical volume was located. The theory is that if a price had a lot of historical volume, then there will still be buy and sell orders located there that could make it a critical turning point for the stock's price action.

multi-asset-model.R - Constructs simple forecasting models for several different US and global stock and bond index ETFs. Uses supervised machine learning to select model inputs, and multiple linear regression to forecast returns. Sharpe ratio is calculated based on forecasted returns and observed standard deviation from model when forecasting out of sample on a test set. (Sharpe ratios must be used with caution because error is systematically biased rather than random.)

stockmarketseasonality.R - Script to determine the seasonal pattern of returns for any ticker over the last 20 years (or for the lifetime of the ticker, if shorter than that). Just change the ticker and type variables and execute the script to get the chart you want.

trendlinefinder.R - Scrapes Yahoo! Finance data and charts it using tidyquant functions. Finds and draws all major support and resistance trend lines.

tsadata.R - Scrapes TSA traveler throughput data from tsa.gov and charts 2020 throughput against 2019 throughput to examine the impact of the Covid-19 (coronavirus, SARS-COV-2) pandemic.
