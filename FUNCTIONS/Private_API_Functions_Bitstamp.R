# This script provides several functions allowing to call the private API part of BITSTAMP
# You need to have several objects in your environment to make them work (credential, tokens...)

# Some library are necessary to make these function work
library(dplyr)
library(digest)       # For the hmac function that allows to create the signature
library(RCurl)        # Pour récupérer les données en appelant des URLS
library(jsonlite)      # To go from JSON to data frame

# to access the Bitstamp API, you need to have the following passwords in your environment:
#key_bitstamp="yyxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#secret_bitstamp="vfxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#customer_id_bitstamp="zxxxxxx"
  

# A function that returns my Bitstamp balance
get_my_balance_bitstamp=function(){
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret_bitstamp, object = paste0( nonce, customer_id_bitstamp, key_bitstamp), algo = "sha256"))
  post_data <- paste0("key=", key_bitstamp, "&signature=", signature, "&nonce=", nonce)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = "https://www.bitstamp.net/api/v2/balance/", binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json,  flatten=TRUE) %>% data.frame()
  result=result[ , c("bch_available", "btc_available", "eth_available", "eur_available", "ltc_available", "xrp_available")]
  return(result)
}

# A function that returns my previous transactions
get_my_transactions_bitstamp=function(){
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret_bitstamp, object = paste0( nonce, customer_id_bitstamp, key_bitstamp), algo = "sha256"))
  post_data <- paste0("key=", key_bitstamp, "&signature=", signature, "&nonce=", nonce)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = "https://www.bitstamp.net/api/v2/user_transactions/", binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}

# EUR --> CRYPTO | A function that buy crypto with euros. I have to buy at least 5 euros. The amount must be provided in crypto
from_euro_to_crypto_bitstamp = function( amount, currency ){
  amount=round(amount, 8)
  clean_currency=tolower(currency)
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret_bitstamp, object = paste0( nonce, customer_id_bitstamp, key_bitstamp), algo = "sha256"))
  post_data <- paste0("key=", key_bitstamp, "&signature=", signature, "&nonce=", nonce, "&amount=", amount)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = paste("https://www.bitstamp.net/api/v2/buy/market/", clean_currency, "/", sep=""), binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}

# CRYPTO --> EUR | A function that sell crypto to get euros. I have to buy at least 5 euros. I give the amount in crypto
from_crypto_to_euro_bitstamp = function( amount, currency ){
  amount=round(amount, 8)
  clean_currency=tolower(currency)
  nonce=as.character(as.numeric(Sys.time()) * 1000000)
  signature = toupper( hmac(key = secret_bitstamp, object = paste0( nonce, customer_id_bitstamp, key_bitstamp), algo = "sha256"))
  post_data <- paste0("key=", key_bitstamp, "&signature=", signature, "&nonce=", nonce, "&amount=", amount)
  curl <- getCurlHandle()
  query_result_json <- rawToChar(getURLContent(curl = curl, url = paste("https://www.bitstamp.net/api/v2/sell/market/", clean_currency, "/", sep=""), binary = TRUE, postfields = post_data))
  result=fromJSON(query_result_json) %>% as.data.frame()
  return(result)
}



# Example of utilization
#get_bitstamp(Sys.time(), "BCHEUR")
#get_my_balance_bitstamp()
#from_euro_to_crypto_bitstamp(0.047, "BCHEUR" )
#from_crypto_to_euro_bitstamp(0.1, "BCHEUR" )


