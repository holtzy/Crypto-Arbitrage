# A script by Yan Holtz - yan.holtz.data@gmail.com

# This file displays a set of functions allowing to call the public APIs of several cryptocurrency platforms
# All APIs have their particularity. Each function call the APIs to retrieve the 'ticker' of each platform

# OUTPUT: time / platform / ask / bid / last / open / low / high / volume / columeQuote / timestamp / currency
# INPUT: is time (found using Sys.time()) AND currency

# libraries
library(RJSONIO)        # To transform a JSON format in a data frame.
library(dplyr)
library(digest)         # For the hmac function that allows to create the signature
library(RCurl)          # Pour récupérer les données en appelant des URLS
require(jsonlite)       # To go from JSON to data frame




# KRAKEN: currency= BTCEUR / ETHEUR / XRPEUR / LTCEUR / BCHEUR
get_kraken=function(time, currency){
  adress=paste("https://api.kraken.com/0/public/Ticker?pair=", currency, sep="" )
  tmp=getURLContent(adress) %>% fromJSON() 
  tmp=tmp$result[[1]]
  result = data.frame(time=time, platform="Kraken", ask=tmp$a[1], bid=tmp$b[1], last=tmp$c[1], open=tmp$o, low=tmp$l[1], high=tmp$h[1], volume=tmp$v[1], volumeQuote=NA, timestamp=NA, symbol=currency)
  return(result)
}


# BITSTAMP: currency= BTCEUR / ETHEUR / XRPEUR / LTCEUR / BCHEUR
# Note that the ticker is cached during 10 seconds if I understood well.
get_bitstamp=function(time, currency){
  clean_currency=tolower(currency)
  adress=paste("https://www.bitstamp.net/api/v2/ticker/",clean_currency, sep="")
  tmp=getURLContent(adress) %>% fromJSON()
  result = data.frame(time=time, platform="Bitstamp", ask=tmp[8], bid=tmp[4], last=tmp[2], open=tmp[9], low=tmp[7], high=tmp[1], volume=tmp[6], volumeQuote=NA, timestamp=NA, symbol=currency)
  return(result)
}


# COINBASE: currency= BTCEUR / ETHEUR / BCHEUR
get_coinbase=function(time, currency){
  a=substr(currency, 1, 3)
  b=substr(currency, 4, 6)
  clean_currency=paste(a,b,sep="-")
  a=fromJSON(getURLContent(paste("https://api.coinbase.com/v2/prices/",clean_currency,"/sell", sep="")))$data[3] %>% as.numeric()
  b=fromJSON(getURLContent(paste("https://api.coinbase.com/v2/prices/",clean_currency,"/buy", sep="")))$data[3] %>% as.numeric()
  c=fromJSON(getURLContent(paste("https://api.coinbase.com/v2/prices/",clean_currency,"/spot", sep="")))$data[3] %>% as.numeric()
  result = data.frame(time=time, platform="coinbase", ask=b, bid=a, last=c, open=NA, low=NA, high=NA, volume=NA, volumeQuote=NA, timestamp=NA, symbol=currency)
  return(result)
}




# CEX.IO: currency= BTCEUR / ETHEUR / BCHEUR
get_cex=function(time, currency){
  cur1=substr(currency,1,3)
  cur2=substr(currency,4,6)
  adress=paste("https://cex.io/api/ticker/",cur1,"/",cur2, sep="")
  tmp=fromJSON(getURLContent(adress))
  result = data.frame(time=time, platform="Cex", ask=tmp$ask, bid=tmp$bid, last=tmp$last, open=NA, low=tmp$low, high=tmp$high, volume=tmp$volume, volumeQuote=NA, timestamp=tmp$timestamp, symbol=currency)
  return(result)
}



# BITFINEX: currency= BTCEUR / 
get_bitfinex=function(time, currency){
  adress=paste("https://api.bitfinex.com/v2/ticker/t", currency, sep="")
  tmp=fromJSON(getURLContent(adress))
  result = data.frame(time=time, platform="Bitfinex", ask=tmp[3], bid=tmp[1], last=tmp[7], open=NA, low=tmp[10], high=tmp[9], volume=tmp[8], volumeQuote=NA, timestamp=NA, symbol=currency)
  return(result)
}
#get_bitfinex(Sys.time(), "BTCEUR")




# ----- I do not use these following platforms anymore ------

# HITBTC
get_hitbtc=function(time){
  tmp=fromJSON(getURLContent("https://api.hitbtc.com/api/2/public/ticker/BTCEUR")) %>% as.data.frame()
  result = data.frame(time=time, platform="HitBTC", ask=tmp[1,], bid=tmp[2,], last=tmp[3,], open=tmp[4,], low=tmp[5,], high=tmp[6,], volume=tmp[7,], volumeQuote=tmp[8,], timestamp=tmp[9,], symbol="BTCEUR")
  return(result)
}


# BITMARKET
get_bitmarket=function(time, currency){
  adress=paste("https://www.bitmarket.net/json/",currency,"/ticker.json", sep="")
  tmp=fromJSON(getURLContent(adress))
  result = data.frame(time=time, platform="Bitmarket", ask=tmp[1], bid=tmp[2], last=tmp[3], open=NA, low=tmp[4], high=tmp[5], volume=tmp[7], volumeQuote=NA, timestamp=NA, symbol=currency)
  return(result)
}

