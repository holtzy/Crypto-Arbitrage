# This script call the API of plateforms during an infinite amount of time.
# It build a huge 'ticker' data frame

# 3 arguments: 
  # - currency: BTCEUR / ETHEUR / XRPEUR / LTCEUR / BCHEUR
  # - Time to wait during 2 API calls
  # - output file (.R format)

# This script can be run locally or on the cluster doing:
# -- Cluster
# 
# qsub -b y -cwd -e tmp_BTCEUR.e -o tmp_BTCEUR.o -l vf=10G,h_vmem=10G   "/clusterdata/apps/R-3.2.3/bin/Rscript  /ibscratch/wrayvisscher/Yan_Holtz/X_CRYPTO/Run_API_public_market_harvest.R BTCEUR 1 /ibscratch/wrayvisscher/Yan_Holtz/X_CRYPTO/PublicMarket_BTCEUR.R"

# -- Locally
# cd ~/Dropbox/CRYPTO/4_DATASET
# Rscript ../3_FUNCTIONS/Run_API_public_market_harvest.R BTCEUR 10 ticked_data_BTC_26_jun.Rdata
# Rscript ../3_FUNCTIONS/Run_API_public_market_harvest.R ETHEUR 10 ticked_data_ETH_26_jun.Rdata
# Rscript ../3_FUNCTIONS/Run_API_public_market_harvest.R XRPEUR 10 ticked_data_XRP_26_jun.Rdata
# Rscript ../3_FUNCTIONS/Run_API_public_market_harvest.R LTCEUR 10 ticked_data_LTC_26_jun.Rdata
# Rscript ../3_FUNCTIONS/Run_API_public_market_harvest.R BCHEUR 10 ticked_data_BCH_26_jun.Rdata



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
  if (num %% 10 == 0){
      cat("--------- SAVE -------- ")
      save(Ticker, file=output)
  }
  Sys.sleep(my_sleep_time)
  
}
