# This script allows to make arbitrage between Bitstamp and KRAKEN. Each time unit it does:
#     - get Bitstamp and Kraken BCH Ticker
#     - calculate the difference in %
#     - If i/ the difference is big enough ii/ it is interesting to make a transfert to balance accounts --> make the transaction
#     - produce a report file allowing to study the behaviour of the bot

# arguments: 
# - Currency, can be BTCEUR / ETHEUR / XRPEUR / LTCEUR / BCHEUR
# - quantity to trade (in euro). must be at least 5
# - threshold 
# - threshold that I accept for rebalancing data
# - output file (.R format)

# Example to run the script
# Rscript Run_Arbitrage_Bitstamp_Kraken.R "BCHEUR" 5.2  0.7  0.6 arbitrage_bitstamp_kraken_BCHEUR_14May.Rdata 




# 0 ------ INITIALISATION

# Get the arguments
args = commandArgs(trailingOnly=TRUE)
currency=args[1]
euro_to_trade=as.numeric(args[2])
thres=as.numeric(args[3])
thres_rebalance=as.numeric(args[4])
output=args[5]


# Library requested:
library(RJSONIO)        # To transform a JSON format in a data frame.
library(dplyr)
library(digest)         # For the hmac function that allows to create the signature
library(RCurl)          # Pour récupérer les données en appelant des URLS
require(jsonlite)       # To go from JSON to data frame

# And source the functions I've written
# public API
source("https://raw.githubusercontent.com/holtzy/Cryp-To/master/FUNCTIONS/Public_Market_Functions.R")
# private API for Bitstamp
source("https://raw.githubusercontent.com/holtzy/Cryp-To/master/FUNCTIONS/Private_API_Functions_Bitstamp.R")
# private API for Kraken
source("https://raw.githubusercontent.com/holtzy/Cryp-To/master/FUNCTIONS/Private_API_Functions_Kraken.R")





# 0 ------ CODES
# This script needs to access you code to the Bitstamp and Cex private APIs.
# You need to create 6 objects for the script to work: key_kraken / secret_kraken / customer_id_kraken / key_bitstamp / secret_bitstamp / customer_id_bitstamp
# For my personal usage, I source this object on a file that is not public:
source("access.R")








# 1 ------ GET INITIAL BALANCE & INITIALIZE OUTPUTS

# I need to know the balance in each platform to know if I can trade or not.

cat("--- Start initialization \n")

# Get platform1 balance:
initial_balance_bitstamp=get_my_balance_bitstamp()
init_crypto_plat1 = as.numeric(as.character(initial_balance_bitstamp[ 1, "bch_available"] ))
init_euro_plat1 = as.numeric( as.character( initial_balance_bitstamp[ 1, "eur_available"] ))

# Get platform2 balance:
initial_balance_kraken=get_my_balance_kraken()
init_crypto_plat2 = as.numeric( as.character( initial_balance_kraken[ 1, "BCH"] ))
init_euro_plat2 = as.numeric( as.character( initial_balance_kraken[ 1, "ZEUR"] ))

# Initialize outputs. I will have one line per iteration of the loop. If I do a transaction , some column will be filled with the appropriate information.
bilan=as.data.frame(matrix(NA, 0, 25))
names(bilan) = c(
  "time", "last_plat1", "ask_plat1", "bid_plat1", "last_plat2", "ask_plat2", "bid_plat2", "diff_side1", "diff_side2", "transaction", "rebalance", "real_bitstamp",
  "thres", "thres_rebalance", "euro_to_trade", "crypto_to_trade",
  "euro_plat1", "crypto_plat1", "euro_plat2", "crypto_plat2",
  "total_euro", "total_crypto", "total", "total_without_arbitrage", "id"
  )

cat("--- Initialization step OK \n\n")








# 2 ------ START THE LOOP

cat("--- Start loop \n")
num=0

while(TRUE){
  num=num+1
  time=Sys.time()
  real_bitstamp=NA
  cat(paste("\n\n--------- Call # ", num, " | Time: ", time, " ----------\n"))
  
  # ---- Step0: initialize my current amount of money for the start of the loop
  if( exists("current_crypto_plat1")==FALSE ){
    current_crypto_plat1=init_crypto_plat1
    current_crypto_plat2=init_crypto_plat2
    current_euro_plat1=init_euro_plat1
    current_euro_plat2=init_euro_plat2
    cat("Step0 : ok\n")
  }
  
  # ---- Step1: recover price of both platforms
  tmp=try(get_bitstamp(time, currency))
  if(is.data.frame(tmp)){ 
    ask_plat1 = as.numeric( as.character( tmp$ask))  
    bid_plat1 = as.numeric( as.character( tmp$bid))
    price_plat1 = as.numeric( as.character( tmp$last))
  }
  tmp=try(get_kraken(time, currency))
  if(is.data.frame(tmp)){ 
    ask_plat2 = as.numeric( as.character( tmp$ask))  
    bid_plat2 = as.numeric( as.character( tmp$bid))
    price_plat2 = as.numeric( as.character( tmp$last))
  }
  cat( paste("    prices: ", ask_plat1, "/", bid_plat1, " - ", ask_plat2, "/", bid_plat2, "\n", sep=""))
  cat("Step1: ok\n")
  
  # ---- Step2: calculate difference between both plateform? Side1 = platform1 is more expensive. So I buy on plat2 and sell on plat1
  diff_side1 = (bid_plat1 - ask_plat2) / mean( c(bid_plat1,ask_plat2) ) * 100
  diff_side2 = (bid_plat2 - ask_plat1) / mean( c(bid_plat2,ask_plat1) ) * 100
  cat( paste( "    relative diff side1 / side2: ", round(diff_side1,3), " / ", round(diff_side2,3), "\n", sep=""))
  cat("Step2: ok\n")
  
  
  # ---- Step3: calcule the equivalence crypto / euro --> we need that to trade the good amount
  crypto_to_trade=( euro_to_trade /  min(bid_plat1, bid_plat2) )
  cat("Step3: ok\n")
  
  
  # ---- Step4: if there is a significant difference + I have the money, I make a transaction + I check my balance. 
  trade_side1 = diff_side1 > thres & current_crypto_plat1>crypto_to_trade & current_euro_plat2>euro_to_trade
  trade_side2 = diff_side2 > thres & current_crypto_plat2>crypto_to_trade & current_euro_plat1>euro_to_trade
  # I can also make a transaction to rebalance my fundings! If I have less than one third of the crypto in a plateform, I re-balance
  tot_crypto = current_crypto_plat1 + current_crypto_plat2
  rebalance_side1 = current_crypto_plat2<tot_crypto/3  & diff_side1 > thres_rebalance & current_crypto_plat1>crypto_to_trade & current_euro_plat2>euro_to_trade
  rebalance_side2 = current_crypto_plat1<tot_crypto/3  & diff_side2 > thres_rebalance & current_crypto_plat2>crypto_to_trade & current_euro_plat1>euro_to_trade
  
  if( trade_side1==TRUE | trade_side2==TRUE | rebalance_side1==TRUE | rebalance_side2==TRUE ){
    
    cat("I make a transaction\n")
    transaction="yes"
    if( rebalance_side1==TRUE | rebalance_side2==TRUE ){ rebalance="yes" }

    # -- 4.1 if side1 --> plateform1 > plateform2 --> I buy crypto on plateform2, and I sell crypto on plateform1
    if( trade_side1==TRUE | rebalance_side1==TRUE){
      cat("    option 1 trade\n")
      tmp=try( from_crypto_to_euro_bitstamp( crypto_to_trade, currency) )
      if(is.data.frame(tmp) & ncol(tmp)==5){ cat("    bitstamp transaction ok\n") ; real_bitstamp=as.numeric(as.character(tmp$price)) }else{ cat("    bitstamp transaction FAILED\n")}
      tmp=try( from_euro_to_crypto_kraken( crypto_to_trade, currency) )
      if(is.data.frame(tmp)){ cat("    kraken transaction ok\n") }else{ cat("    kraken transaction FAILED\n")}
    }
    
    # -- 4.2 if side 2 --> plateform1 < plateform2 --> I buy crypto on plateform1, and I sell crypto on plateform2
    if( trade_side2==TRUE | rebalance_side2==TRUE){
      cat("    option 2 trade\n")
      tmp=try( from_euro_to_crypto_bitstamp( crypto_to_trade, currency) )
      if(is.data.frame(tmp) & ncol(tmp)==5){ cat("    bitstamp transaction ok\n") ; real_bitstamp=as.numeric(as.character(tmp$price)) }else{ cat("    bitstamp transaction FAILED\n")}
      tmp=try( from_crypto_to_euro_kraken( crypto_to_trade, currency) )
      if(is.data.frame(tmp)){ cat("    kraken transaction ok\n") }else{ cat("    kraken transaction FAILED\n")}
    }

    # -- 4.3 And in any case I check how much money I have now
    tmp=try(get_my_balance_bitstamp())
    if(is.data.frame(tmp)){
      current_crypto_plat1 = as.numeric( as.character( tmp[1, "bch_available"] ))
      current_euro_plat1 = as.numeric( as.character( tmp[1, "eur_available"] ))
      cat("    Bitstamp balance recovered\n")
    }else{ cat("    Bitstamp balance NOT recovered\n") }
    tmp=try(get_my_balance_kraken())
    if( class(tmp)!="try-error" ){
      current_crypto_plat2 = as.numeric( as.character( tmp[1, "BCH"] ))
      current_euro_plat2 = as.numeric( as.character( tmp[1, "ZEUR"] ))
      cat("    Kraken balance recovered\n")
    }else{ cat("    Kraken balance NOT recovered\n") }
    
    
        
  # ---- Step 4: If no significative difference, then I don't do anything, and NA to the bilan table  
  }else{
    cat("I don't do anything\n")
    transaction="no"
    rebalance="no"
  }

  # ---- Step5: make a summary data frame for this time unit and add it to the final output data frame
  total_euro=current_euro_plat1 + current_euro_plat2
  total_crypto=current_crypto_plat1 + current_crypto_plat2
  total= total_euro + current_crypto_plat1*price_plat1 + current_crypto_plat2*price_plat2
  total_without_arbitrage= init_euro_plat1 + init_euro_plat2 + init_crypto_plat1*price_plat1 + init_crypto_plat2*price_plat2
  bilan[num,]=c( time, price_plat1, ask_plat1, bid_plat1, price_plat2, ask_plat2, bid_plat2, diff_side1, diff_side2, transaction, rebalance, real_bitstamp, thres, thres_rebalance, euro_to_trade, crypto_to_trade, current_euro_plat1, current_crypto_plat1, current_euro_plat2, current_crypto_plat2, total_euro, total_crypto, total, total_without_arbitrage, num)

  # ---- Step6: Once in a while, I save the summary data frame
  if (num %% 2 == 0){
    cat("Save the bilan file")
    save(bilan, file=output)
  }
  
  # ---- I wait a little bit between each occurence.
  Sys.sleep(5)

}




























