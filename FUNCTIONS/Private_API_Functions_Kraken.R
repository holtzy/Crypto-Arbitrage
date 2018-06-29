# This script provides several functions allowing to call the private API part of KRAKEN
# You need to have several objects in your environment to make them work (credential, tokens...)

# Some library are necessary to make these function work
library(dplyr)
library(digest)       # For the hmac function that allows to create the signature
library(RCurl)        # Pour récupérer les données en appelant des URLS
require(jsonlite)      # To go from JSON to data frame


# to access the Bitstamp API, you need to have the following passwords in your environment:
#key_kraken="yyxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#secret_kraken="vfxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#customer_id_kraken="zxxxxxx"
  

# A function that return my current balance:
get_my_balance_kraken=function(){
    url="https://api.kraken.com/0/private/Balance"
    nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
    post_data <- paste0("nonce=", nonce)
    method_path <- gsub("^.*?kraken.com", "", url)
    sign <- hmac(key = base64Decode(secret_kraken, mode = "raw"), object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
    httpheader <- c(`API-Key` = key_kraken, `API-Sign` = base64Encode(sign))
    curl <- getCurlHandle()
    # Next step often bug. I have to try until it works.
    query_result_json=NULL
    while( is.null(query_result_json) ){
      try(query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, postfields = post_data, httpheader = httpheader)))
      cat("kraken try again\n")
    }
    result <- fromJSON(query_result_json)$result %>% as.data.frame()
    return(result)
}


# A function that return my trade history:
# Looks like my sell fee is 0.25%
get_my_trade_history_kraken=function(){
  url="https://api.kraken.com/0/private/TradesHistory"
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret_kraken, mode = "raw"), object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key_kraken, `API-Sign` = base64Encode(sign))
  curl <- getCurlHandle()
  query_result_json=NULL
  while( is.null(query_result_json) ){
    try(query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, postfields = post_data, httpheader = httpheader)))
    cat("kraken try again\n")
  }
  tmp <- fromJSON(query_result_json)$result %>% unlist()
  result=matrix(tmp[-length(tmp)], ncol=11, byrow = TRUE)
  colnames(result)=gsub(".*\\.","", names(tmp)[1:11])
  return(result)
}

# EUR --> CRYPTO | A function that buy crypto with euros. It looks like there is no limit for the transaction. I give the amount in crypto.
from_euro_to_crypto_kraken = function( amount, currency ){
  url="https://api.kraken.com/0/private/AddOrder"
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce, "&pair=", currency, "&type=buy&ordertype=market&volume=", amount)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret_kraken, mode = "raw"), object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key_kraken, `API-Sign` = base64Encode(sign))
  curl <- getCurlHandle()
  query_result_json=NULL
  while( is.null(query_result_json) ){
    try(query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, postfields = post_data, httpheader = httpheader)))
    cat("kraken try again")
  }
  result <- fromJSON(query_result_json)$result %>% as.data.frame()
  return(result)
}

# CRYPTO --> EUR | A function that sell crypto for euros. It looks like there is no limit for the transaction. I give the amount in crypto.
from_crypto_to_euro_kraken = function( amount, currency ){
  url="https://api.kraken.com/0/private/AddOrder"
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce, "&pair=", currency, "&type=sell&ordertype=market&volume=", amount)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret_kraken, mode = "raw"), object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key_kraken, `API-Sign` = base64Encode(sign))
  curl <- getCurlHandle()
  query_result_json=NULL
  while( is.null(query_result_json) ){
    try(query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, postfields = post_data, httpheader = httpheader)))
    cat("kraken try again\n")
  }
  result <- fromJSON(query_result_json)$result %>% as.data.frame()
  return(result)
}


# Example of utilization
#get_kraken(Sys.time(), "BCHEUR")
#get_my_balance_kraken()
#from_euro_to_crypto_kraken( 0.0033, "BCHEUR" )
#from_crypto_to_euro_kraken( 0.0094922300, "BCHEUR" )

