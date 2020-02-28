source("main_func.R")

bitfinex_func <- function(finish_table, lim_or_mark, data_market_price){
  # заполнение курсов binance
  get_price_bitfinex <- function(Name_crypt1, Name_crypt2, Name_crypt3, table_Crypto, lim_or_mark, data_market_price){
    # преобразуем list в data.frame
    data <- as.data.frame(do.call(rbind, data_market_price), ncol=11)
    data[c(12:17)]<- NULL

    switch(lim_or_mark, 
           lim={
             # заполняем строчку курсов для limit price
             # при прямой --------------------------------------------------
             table_Crypto[1,2] <- as.numeric(data[data[,1]==Name_crypt1,2]) # 2-Bid
             table_Crypto[1,3] <- as.numeric(data[data[,1]==Name_crypt2,2])
             table_Crypto[1,4] <- 1/(as.numeric(data[data[,1]==Name_crypt3,4])) # 4-Ask
             table_Crypto[1,10] <- 1/(as.numeric(data[data[,1]==Name_crypt1,4]))
             table_Crypto[1,11] <- as.numeric(data[data[,1]==Name_crypt3,2])
             table_Crypto[1,12] <- 1/(as.numeric(data[data[,1]==Name_crypt2,4]))
             # при обратной ------------------------------------------------
             table_Crypto[1,6] <- as.numeric(data[data[,1]==Name_crypt3,2])
             table_Crypto[1,7] <- 1/(as.numeric(data[data[,1]==Name_crypt2,4]))
             table_Crypto[1,8] <- 1/(as.numeric(data[data[,1]==Name_crypt1,4]))
             table_Crypto[1,14] <- as.numeric(data[data[,1]==Name_crypt2,2])
             table_Crypto[1,15] <- 1/(as.numeric(data[data[,1]==Name_crypt3,4]))
             table_Crypto[1,16] <- as.numeric(data[data[,1]==Name_crypt1,2])
           },
           mark={
             # заполняем строчку курсов для market price
             # при прямой --------------------------------------------------
             table_Crypto[1,2] <- as.numeric(data[data[,1]==Name_crypt1,8]) # 8-Last
             table_Crypto[1,3] <- as.numeric(data[data[,1]==Name_crypt2,8])
             table_Crypto[1,4] <- 1/(as.numeric(data[data[,1]==Name_crypt3,8]))
             table_Crypto[1,10] <- 1/(as.numeric(data[data[,1]==Name_crypt1,8]))
             table_Crypto[1,11] <- as.numeric(data[data[,1]==Name_crypt3,8])
             table_Crypto[1,12] <- 1/(as.numeric(data[data[,1]==Name_crypt2,8]))
             # при обратной ------------------------------------------------
             table_Crypto[1,6] <- as.numeric(data[data[,1]==Name_crypt3,8])
             table_Crypto[1,7] <- 1/(as.numeric(data[data[,1]==Name_crypt2,8]))
             table_Crypto[1,8] <- 1/(as.numeric(data[data[,1]==Name_crypt1,8]))
             table_Crypto[1,14] <- as.numeric(data[data[,1]==Name_crypt2,8])
             table_Crypto[1,15] <- 1/(as.numeric(data[data[,1]==Name_crypt3,8]))
             table_Crypto[1,16] <- as.numeric(data[data[,1]==Name_crypt1,8])
           })
    # ------------------
    return(table_Crypto)
  }
  
  # -----------------------------------------------------------------------------------
  # узнать номер криптовалютной пары для первых 3 параметров в нижних функциях
  #what_num_crypt<-as.data.frame.raw(data_market_price)
  #View(what_num_crypt)
  
  # заполняем значение курса
  table_BTG_USDT <- get_price_bitfinex("tBTGBTC", "tBTCUSD", "tBTGUSD", table_BTG_USDT, lim_or_mark, data_market_price)
  table_DASH_USDT <- get_price_bitfinex("tDSHBTC", "tBTCUSD", "tDSHUSD", table_DASH_USDT, lim_or_mark, data_market_price)
  table_EOS_USDT <- get_price_bitfinex("tEOSBTC", "tBTCUSD", "tEOSUSD", table_EOS_USDT, lim_or_mark, data_market_price) 
  table_EOS_ETH <- get_price_bitfinex("tEOSETH", "tETHBTC", "tEOSBTC", table_EOS_ETH, lim_or_mark, data_market_price)
  table_EOS_EUR <- get_price_bitfinex("tEOSBTC", "tBTCEUR", "tEOSEUR", table_EOS_EUR, lim_or_mark, data_market_price) 
  table_ETC_USDT <- get_price_bitfinex("tETCBTC", "tBTCUSD", "tETCUSD", table_ETC_USDT, lim_or_mark, data_market_price)
  table_ETH_USDT <- get_price_bitfinex("tETHBTC", "tBTCUSD", "tETHUSD", table_ETH_USDT, lim_or_mark, data_market_price)
  table_ETH_EUR <- get_price_bitfinex("tETHBTC", "tBTCEUR", "tETHEUR", table_ETH_EUR, lim_or_mark, data_market_price) 
  table_MIOTA_USDT <- get_price_bitfinex("tIOTBTC", "tBTCUSD", "tIOTUSD", table_MIOTA_USDT, lim_or_mark, data_market_price)
  table_MIOTA_ETH <- get_price_bitfinex("tIOTETH", "tETHBTC", "tIOTBTC", table_MIOTA_ETH, lim_or_mark, data_market_price)
  table_MIOTA_EUR <- get_price_bitfinex("tIOTBTC", "tBTCEUR", "tIOTEUR", table_MIOTA_EUR, lim_or_mark, data_market_price) 
  table_LTC_USDT <- get_price_bitfinex("tLTCBTC", "tBTCUSD", "tLTCUSD", table_LTC_USDT, lim_or_mark, data_market_price) 
  table_NEO_USDT <- get_price_bitfinex("tNEOBTC", "tBTCUSD", "tNEOUSD", table_NEO_USDT, lim_or_mark, data_market_price) 
  table_NEO_ETH <- get_price_bitfinex("tNEOETH", "tETHBTC", "tNEOBTC", table_NEO_ETH, lim_or_mark, data_market_price) 
  table_NEO_EUR <- get_price_bitfinex("tNEOBTC", "tBTCEUR", "tNEOEUR", table_NEO_EUR, lim_or_mark, data_market_price) 
  table_QTUM_USDT <- get_price_bitfinex("tQTMBTC", "tBTCUSD", "tQTMUSD", table_QTUM_USDT, lim_or_mark, data_market_price)
  table_QTUM_ETH <- get_price_bitfinex("tQTMETH", "tETHBTC", "tQTMBTC", table_QTUM_ETH, lim_or_mark, data_market_price) 
  table_TRX_USDT <- get_price_bitfinex("tTRXBTC", "tBTCUSD", "tTRXUSD", table_TRX_USDT, lim_or_mark, data_market_price) 
  table_TRX_ETH <- get_price_bitfinex("tTRXETH", "tETHBTC", "tTRXBTC", table_TRX_ETH, lim_or_mark, data_market_price) 
  table_XLM_USDT <- get_price_bitfinex("tXLMBTC", "tBTCUSD", "tXLMUSD", table_XLM_USDT, lim_or_mark, data_market_price)
  table_XLM_ETH <- get_price_bitfinex("tXLMETH", "tETHBTC", "tXLMBTC", table_XLM_ETH, lim_or_mark, data_market_price) 
  table_XLM_EUR <- get_price_bitfinex("tXLMBTC", "tBTCEUR", "tXLMEUR", table_XLM_EUR, lim_or_mark, data_market_price)
  table_XMR_USDT <- get_price_bitfinex("tXMRBTC", "tBTCUSD", "tXMRUSD", table_XMR_USDT, lim_or_mark, data_market_price) 
  table_XRP_USDT <- get_price_bitfinex("tXRPBTC", "tBTCUSD", "tXRPUSD", table_XRP_USDT, lim_or_mark, data_market_price) 
  table_ZEC_USDT <- get_price_bitfinex("tZECBTC", "tBTCUSD", "tZECUSD", table_ZEC_USDT, lim_or_mark, data_market_price)
  
  # -----------------------------------------------------------------------------------
  # выполняем расчет и заполняем строчки
  # https://api.bitfinex.com/v1/symbols_details - тут смотрим делители, а именно колличество знаков после запятой (del1.1 - minimum_order_size первой пары, del2.1 - minimum_order_size второй пары, del3.1 - minimum_order_size третьей пары) ... https://api.bitfinex.com/v2/tickers?symbols=ALL - тут смотрим (del1.2 - BID_PRICE первой пары, del2.2 - BID_PRICE второй пары, del3.2 - BID_PRICE третьей пары)
  table_BTG_USDT <- find_balance(table_BTG_USDT, BTG, BTC, bitfinex_fee, 1, 7, 3, 1, 1, 3)
  table_DASH_USDT <- find_balance(table_DASH_USDT, DASH, BTC, bitfinex_fee, 2, 6, 3, 1, 2, 2)
  table_EOS_USDT <- find_balance(table_EOS_USDT, EOS, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 4)
  table_EOS_ETH <- find_balance(table_EOS_ETH, EOS, ETH, bitfinex_fee, 0, 6, 2, 6, 0, 8)
  table_EOS_EUR <- find_balance(table_EOS_EUR, EOS, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 4)
  table_ETC_USDT <- find_balance(table_ETC_USDT, ETC, BTC, bitfinex_fee, 1, 7, 3, 1, 1, 3)
  table_ETH_USDT <- find_balance(table_ETH_USDT, ETH, BTC, bitfinex_fee, 2, 6, 3, 1, 2, 2)
  table_ETH_EUR <- find_balance(table_ETH_EUR, ETH, BTC, bitfinex_fee, 2, 6, 3, 1, 2, 2)
  table_MIOTA_USDT <- find_balance(table_MIOTA_USDT, MIOTA, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 5)
  table_MIOTA_ETH <- find_balance(table_MIOTA_ETH, MIOTA, ETH, bitfinex_fee, 0, 7, 2, 6, 0, 8)
  table_MIOTA_EUR <- find_balance(table_MIOTA_EUR, MIOTA, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 5)
  table_LTC_USDT <- find_balance(table_LTC_USDT, LTC, BTC, bitfinex_fee, 1, 7, 3, 1, 1, 3)
  table_NEO_USDT <- find_balance(table_NEO_USDT, NEO, BTC, bitfinex_fee, 1, 7, 3, 1, 1, 3)
  table_NEO_ETH <- find_balance(table_NEO_ETH, NEO, ETH, bitfinex_fee, 1, 6, 2, 6, 1, 7)
  table_NEO_EUR <- find_balance(table_NEO_EUR, NEO, BTC, bitfinex_fee, 1, 7, 3, 1, 1, 3)
  table_QTUM_USDT <- find_balance(table_QTUM_USDT, QTUM, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 4)
  table_QTUM_ETH <- find_balance(table_QTUM_ETH, QTUM, ETH, bitfinex_fee, 0, 6, 2, 6, 0, 8)
  table_TRX_USDT <- find_balance(table_TRX_USDT, TRX, BTC, bitfinex_fee, 0, 7, 3, 1, 0, 6)
  table_TRX_ETH <- find_balance(table_TRX_ETH, TRX, ETH, bitfinex_fee, 0, 8, 2, 6, 0, 7)
  table_XLM_USDT <- find_balance(table_XLM_USDT, XLM, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 5)
  table_XLM_ETH <- find_balance(table_XLM_ETH, XLM, ETH, bitfinex_fee, 0, 8, 2, 6, 0, 8)
  table_XLM_EUR <- find_balance(table_XLM_EUR, XLM, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 5)
  table_XMR_USDT <- find_balance(table_XMR_USDT, XMR, BTC, bitfinex_fee, 1, 6, 3, 1, 1, 3)
  table_XRP_USDT <- find_balance(table_XRP_USDT, XRP, BTC, bitfinex_fee, 0, 8, 3, 1, 0, 5)
  table_ZEC_USDT <- find_balance(table_ZEC_USDT, ZEC, BTC, bitfinex_fee, 2, 6, 3, 1, 2, 2)
  
  # -----------------------------------------------------------------------------------
  # готовим отчет
  finish_table <- find_result(finish_table, 1, table_BTG_USDT, BTG, BTC, "BTG->BTC->USD->BTG", "BTG->USD->BTC->BTG", "BTC->BTG->USD->BTC", "BTC->USD->BTG->BTC")
  finish_table <- find_result(finish_table, 2, table_DASH_USDT, DASH, BTC, "DASH->BTC->USD->DASH", "DASH->USD->BTC->DASH", "BTC->DASH->USD->BTC", "BTC->USD->DASH->BTC")
  finish_table <- find_result(finish_table, 3, table_EOS_USDT, EOS, BTC, "EOS->BTC->USD->EOS", "EOS->USD->BTC->EOS", "BTC->EOS->USD->BTC", "BTC->USD->EOS->BTC")
  finish_table <- find_result(finish_table, 4, table_EOS_ETH, EOS, ETH, "EOS->ETH->BTC->EOS", "EOS->BTC->ETH->EOS", "ETH->EOS->BTC->ETH", "ETH->BTC->EOS->ETH")
  finish_table <- find_result(finish_table, 5, table_EOS_EUR, EOS, BTC, "EOS->BTC->EUR->EOS", "EOS->EUR->BTC->EOS", "BTC->EOS->EUR->BTC", "BTC->EUR->EOS->BTC")
  finish_table <- find_result(finish_table, 6, table_ETC_USDT, ETC, BTC, "ETC->BTC->USD->ETC", "ETC->USD->BTC->ETC", "BTC->ETC->USD->BTC", "BTC->USD->ETC->BTC")
  finish_table <- find_result(finish_table, 7, table_ETH_USDT, ETH, BTC, "ETH->BTC->USD->ETH", "ETH->USD->BTC->ETH", "BTC->ETH->USD->BTC", "BTC->USD->ETH->BTC")
  finish_table <- find_result(finish_table, 8, table_ETH_EUR, ETH, BTC, "ETH->BTC->EUR->ETH", "ETH->EUR->BTC->ETH", "BTC->ETH->EUR->BTC", "BTC->EUR->ETH->BTC")
  finish_table <- find_result(finish_table, 9, table_MIOTA_USDT, MIOTA, BTC, "MIOTA->BTC->USD->MIOTA", "MIOTA->USD->BTC->MIOTA", "BTC->MIOTA->USD->BTC", "BTC->USD->MIOTA->BTC")
  finish_table <- find_result(finish_table, 10, table_MIOTA_ETH, MIOTA, ETH, "MIOTA->ETH->BTC->MIOTA", "MIOTA->BTC->ETH->MIOTA", "ETH->MIOTA->BTC->ETH", "ETH->BTC->MIOTA->ETH")
  finish_table <- find_result(finish_table, 11, table_MIOTA_EUR, MIOTA, BTC, "MIOTA->BTC->EUR->MIOTA", "MIOTA->EUR->BTC->MIOTA", "BTC->MIOTA->EUR->BTC", "BTC->EUR->MIOTA->BTC")
  finish_table <- find_result(finish_table, 12, table_LTC_USDT, LTC, BTC, "LTC->BTC->USD->LTC", "LTC->USD->BTC->LTC", "BTC->LTC->USD->BTC", "BTC->USD->LTC->BTC")
  finish_table <- find_result(finish_table, 13, table_NEO_USDT, NEO, BTC, "NEO->BTC->USD->NEO", "NEO->USD->BTC->NEO", "BTC->NEO->USD->BTC", "BTC->USD->NEO->BTC")
  finish_table <- find_result(finish_table, 14, table_NEO_ETH, NEO, ETH, "NEO->ETH->BTC->NEO", "NEO->BTC->ETH->NEO", "ETH->NEO->BTC->ETH", "ETH->BTC->NEO->ETH")
  finish_table <- find_result(finish_table, 15, table_NEO_EUR, NEO, BTC, "NEO->BTC->EUR->NEO", "NEO->EUR->BTC->NEO", "BTC->NEO->EUR->BTC", "BTC->EUR->NEO->BTC")
  finish_table <- find_result(finish_table, 16, table_QTUM_USDT, QTUM, BTC, "QTUM->BTC->USD->QTUM", "QTUM->USD->BTC->QTUM", "BTC->QTUM->USD->BTC", "BTC->USD->QTUM->BTC")
  finish_table <- find_result(finish_table, 17, table_QTUM_ETH, QTUM, ETH, "QTUM->ETH->BTC->QTUM", "QTUM->BTC->ETH->QTUM", "ETH->QTUM->BTC->ETH", "ETH->BTC->QTUM->ETH")
  finish_table <- find_result(finish_table, 18, table_TRX_USDT, TRX, BTC, "TRX->BTC->USD->TRX", "TRX->USD->BTC->TRX", "BTC->TRX->USD->BTC", "BTC->USD->TRX->BTC")
  finish_table <- find_result(finish_table, 19, table_TRX_ETH, TRX, ETH, "TRX->ETH->BTC->TRX", "TRX->BTC->ETH->TRX", "ETH->TRX->BTC->ETH", "ETH->BTC->TRX->ETH")
  finish_table <- find_result(finish_table, 20, table_XLM_USDT, XLM, BTC, "XLM->BTC->USD->XLM", "XLM->USD->BTC->XLM", "BTC->XLM->USD->BTC", "BTC->USD->XLM->BTC")
  finish_table <- find_result(finish_table, 21, table_XLM_ETH, XLM, ETH, "XLM->ETH->BTC->XLM", "XLM->BTC->ETH->XLM", "ETH->XLM->BTC->ETH", "ETH->BTC->XLM->ETH")
  finish_table <- find_result(finish_table, 22, table_XLM_EUR, XLM, BTC, "XLM->BTC->EUR->XLM", "XLM->EUR->BTC->XLM", "BTC->XLM->EUR->BTC", "BTC->EUR->XLM->BTC")
  finish_table <- find_result(finish_table, 23, table_XMR_USDT, XMR, BTC, "XMR->BTC->USD->XMR", "XMR->USD->BTC->XMR", "BTC->XMR->USD->BTC", "BTC->USD->XMR->BTC")
  finish_table <- find_result(finish_table, 24, table_XRP_USDT, XRP, BTC, "XRP->BTC->USD->XRP", "XRP->USD->BTC->XRP", "BTC->XRP->USD->BTC", "BTC->USD->XRP->BTC")
  finish_table <- find_result(finish_table, 25, table_ZEC_USDT, ZEC, BTC, "ZEC->BTC->USD->ZEC", "ZEC->USD->BTC->ZEC", "BTC->ZEC->USD->BTC", "BTC->USD->ZEC->BTC")
  
  # ------------------
  return(finish_table)
}
