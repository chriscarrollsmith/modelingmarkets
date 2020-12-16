# wallstreetpettingzoo
Financial and economic analysis and data visualization

INTRODUCTION

Wall Street Petting Zoo is a weekly investing podcast co-hosted by Robert Coburn and Christopher Carroll Smith. Wall Street Petting Zoo is available through YouTube, Spotify, Apple Podcasts, Stitcher, and Podbean. The Wall Street Petting Zoo Github repository will compile code for financial/economic analysis and data visualization. Code will be created primarily in R and Python. Code is copyrighted by Christopher Carroll Smith, but is free to use with proper acknowledgements under terms of the Apache 2.0 license.

SCRIPTS FOUND IN THIS REPOSITORY

C19SectorReturns.R - Charts stock market returns by equal weight S&P 500 sector since the beginning of the Covid-19 pandemic using data from Yahoo Finance.

DownFriday-DownMonday.R - Tests Jeffrey Hirsch's "Down Friday + Down Monday" indicator to see if DF+DM day pairs are more commonly associated with market inflection points than other day pairs. Uses split-adjusted $SPY price data from Yahoo Finance and tests for robustness against 24 different model specifications. Effect size is positive in all model specifications. Effect size is larger on shorter time frames (5-10 years) and appears weaker when analysis is extended to 15-20 years. Measured magnitude of excess inflection is <1% of index price in all specifications.

ICUutilization.R - Accesses healthdata.gov API and downloads a JSON file with today's state-by-state ICU bed utilization data. Creates a column chart from this data.

tsadata.R - Scrapes TSA traveler throughput data from tsa.gov and charts 2020 throughput against 2019 throughput to examine the impact of the Covid-19 (coronavirus, SARS-COV-2) pandemic.
