# --------------------------------------
# This script calls the public APIs of crypto currency echanges for an infinite amount of time.

# Input: 3 arguments: 
  # - currency: BTCEUR / ETHEUR / XRPEUR / LTCEUR / BCHEUR
  # - Time to wait between 2 API calls
  # - output file name (.R format)

# Output:
  # A 'ticker' file with the prices of the currency: last, bid, ask...

# Run the script doing: 
# wget https://raw.githubusercontent.com/holtzy/Crypto-Arbitrage/master/FUNCTIONS/Run_API_public_market_harvest.R
# Rscript Run_API_public_market_harvest.R BTCEUR 10 ticked_data_currency_date.Rdata
# --------------------------------------


# Get the arguments
args = commandArgs(trailingOnly=TRUE)
currency=args[1]        # Must be one of BTCEUR or ETHEUR
my_sleep_time=args[2]
output=args[3]

# Library requested:
library(RCurl)          # To request data from an API
library(RJSONIO)        # To transform a JSON format in a data frame.
library(dplyr)

# Source the functions to request each API
source("https://raw.githubusercontent.com/holtzy/Crypto-Arbitrage/master/FUNCTIONS/Public_Market_Functions.R")

#Initialize an emtpy result table
Ticker=as.data.frame(matrix(NA, 0, 12))
names(Ticker) = c("time", "platform", "ask", "bid", "last", "open", "low", "high", "volume", "volumeQuote", "timestamp", "symbol")
for(i in c(3:12)){Ticker[,i]=as.numeric(as.character(Ticker$ask))}

# Run the analysis
num=0
while(TRUE){
  num=num+1
  cat("---------")
  cat(num)
  time=Sys.time()
  
  if(currency %in% c("BTCEUR", "ETHEUR", "LTCEUR")){
    cat("harvest coinbase")
    tmp=try(get_coinbase(time, currency))
    if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  }
  
  cat(" / kraken")
  tmp=try(get_kraken(time, currency))
  if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  
  cat(" / bitstamp")
  tmp=try(get_bitstamp(time, currency))
  if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  
  if(currency %in% c("BTCEUR", "ETHEUR", "BCHEUR")){
    print(" / cex.io")
    tmp=try(get_cex(time, currency))
    if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  }
  
  # Every ten loop I save the file
  if (num %% 100 == 0){
      cat("--------- SAVE -------- ")
      save(Ticker, file=output)
  }
  Sys.sleep(my_sleep_time)
  
}
