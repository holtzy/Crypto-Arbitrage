# This script provides several functions allowing to call the private API part of CEX.IO
# You need to have several objects in your environment to make them work (credential, tokens...)

# to access the Cex API you need your passwords:
#customer_id="yyxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#key="yyxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#secret="yyxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# A few libraries are needed
library(tidyverse)
library(digest)       # For the hmac function that allows to create the signature
library(RCurl)        # Pour récupérer les données en appelant des URLS
require(jsonlite)      # To go from JSON to data frame


# A function that returns my Cex balance
get_my_balance_cex=function(){
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret, object = paste0( nonce, customer_id, key), algo = "sha256"))
  post_data <- paste0("key=", key, "&signature=", signature, "&nonce=", nonce)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = "https://cex.io/api/balance/", binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}
#get_my_balance_cex()




# A function that returns open orders
get_open_orders_cex=function(){
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret, object = paste0( nonce, customer_id, key), algo = "sha256"))
  post_data <- paste0("key=", key, "&signature=", signature, "&nonce=", nonce)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = "https://cex.io/api/open_orders/BTC/USD", binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}
#get_open_orders_cex()



# EUR --> CRYPTO | A function that buy crypto with euros | amount is given in EURO
from_euro_to_crypto_cex = function( amount, currency ){
  clean_currency=paste(substr(currency,1,3), "/", substr(currency,4,6), sep="")
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret, object = paste0( nonce, customer_id, key), algo = "sha256"))
  post_data <- paste0("key=", key, "&signature=", signature, "&nonce=", nonce, "&type=buy&amount=",amount,"&order_type=market")
  curl <- getCurlHandle()
  query_result_json=NULL
  while( is.null(query_result_json) ){
    try( query_result_json <- rawToChar(getURLContent(curl = curl, url = paste("https://cex.io/api/place_order/", clean_currency, sep=""), binary = TRUE, postfields = post_data)))
  }
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}
#from_euro_to_crypto_cex(60, "BCHEUR")






# CRYPTO --> EUR | A function that sell CRYPTO for euros | amount is given in CRYPTO
from_crypto_to_euro_cex = function( amount, currency ){
  clean_currency=paste(substr(currency,1,3), "/", substr(currency,4,6), sep="")
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret, object = paste0( nonce, customer_id, key), algo = "sha256"))
  post_data <- paste0("key=", key, "&signature=", signature, "&nonce=", nonce, "&type=sell&amount=",amount,"&order_type=market")
  curl <- getCurlHandle()
  query_result_json=NULL
  while( is.null(query_result_json) ){
    try( query_result_json <- rawToChar(getURLContent(curl = curl, url = paste("https://cex.io/api/place_order/", clean_currency, sep=""), binary = TRUE, postfields = post_data)) )
  }
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}
#from_crypto_to_euro_cex(0.02, "BCHEUR")



# A function that returns my previous transactions
get_my_transactions_ddstamp=function(){
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret, object = paste0( nonce, customer_id, key), algo = "sha256"))
  post_data <- paste0("key=", key, "&signature=", signature, "&nonce=", nonce)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = "https://www.bitstamp.net/api/v2/user_transactions/", binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}



