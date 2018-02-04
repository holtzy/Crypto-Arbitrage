source("https://raw.githubusercontent.com/holtzy/Cryp-To/master/FUNCTIONS/Public_Market_Functions.R")

#Initialize an emtpy result table
Ticker=as.data.frame(matrix(NA, 0, 12))
names(Ticker) = c("time", "platform", "ask", "bid", "last", "open", "low", "high", "volume", "volumeQuote", "timestamp", "symbol")

# Run the analysis
num=0
while(TRUE){
  
  # Keep a trace of where we are in this loop
  num=num+1
  cat("Let's start the iteration number: ",num, " at ", Sys.time() , "\n")
  
  # BITCOIN (BTC)
  time=Sys.time()
  tmp=try(get_coinbase(time, "BTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_kraken(time, "BTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitstamp(time, "BTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitfinex(time, "BTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_cex(time, "BTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  
  # ETHERUM (ETH)
  time=Sys.time()
  tmp=try(get_coinbase(time, "ETHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_kraken(time, "ETHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitstamp(time, "ETHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_cex(time, "ETHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  
  # LITECOIN (LTC)
  time=Sys.time()
  tmp=try(get_coinbase(time, "LTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_kraken(time, "LTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitstamp(time, "LTCEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }

  # BITCOINCASH (BCH)
  time=Sys.time()
  tmp=try(get_coinbase(time, "BCHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_kraken(time, "BCHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitstamp(time, "BCHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_cex(time, "BCHEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  
  # RIPPLE (XRP)
  time=Sys.time()
  tmp=try(get_kraken(time, "XRPEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_bitstamp(time, "XRPEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }
  tmp=try(get_cex(time, "XRPEUR")) ; if(is.data.frame(tmp)){ Ticker=rbind(Ticker, tmp) }

  # Every ten loop I save the file
  if (num %% 1 == 0){  save(Ticker, file="../DATA/public_ticker_harvest.Rdata")  }

}
