source("main_func.R")

poloniex_func <- function(finish_table, lim_or_mark, data_market_price){
  # заполнение курсов binance
  get_price_poloniex <- function(Num_crypt1, Num_crypt2, Num_crypt3, table_Crypto, lim_or_mark, data_market_price){
    switch(lim_or_mark, 
           lim={
             # заполняем строчку курсов для limit price
             # при прямой --------------------------------------------------
             table_Crypto[1,2] <- as.numeric(data_market_price[[Num_crypt1]][["highestBid"]])
             table_Crypto[1,3] <- as.numeric(data_market_price[[Num_crypt2]][["highestBid"]])
             table_Crypto[1,4] <- 1/(as.numeric(data_market_price[[Num_crypt3]][["lowestAsk"]]))
             table_Crypto[1,10] <- 1/(as.numeric(data_market_price[[Num_crypt1]][["lowestAsk"]]))
             table_Crypto[1,11] <- as.numeric(data_market_price[[Num_crypt3]][["highestBid"]])
             table_Crypto[1,12] <- 1/(as.numeric(data_market_price[[Num_crypt2]][["lowestAsk"]]))
             # при обратной ------------------------------------------------
             table_Crypto[1,6] <- as.numeric(data_market_price[[Num_crypt3]][["highestBid"]])
             table_Crypto[1,7] <- 1/(as.numeric(data_market_price[[Num_crypt2]][["lowestAsk"]]))
             table_Crypto[1,8] <- 1/(as.numeric(data_market_price[[Num_crypt1]][["lowestAsk"]]))
             table_Crypto[1,14] <- as.numeric(data_market_price[[Num_crypt2]][["highestBid"]])
             table_Crypto[1,15] <- 1/(as.numeric(data_market_price[[Num_crypt3]][["lowestAsk"]]))
             table_Crypto[1,16] <- as.numeric(data_market_price[[Num_crypt1]][["highestBid"]])
           },
           mark={
             # заполняем строчку курсов для market price
             # при прямой --------------------------------------------------
             table_Crypto[1,2] <- as.numeric(data_market_price[[Num_crypt1]][["last"]])
             table_Crypto[1,3] <- as.numeric(data_market_price[[Num_crypt2]][["last"]])
             table_Crypto[1,4] <- 1/(as.numeric(data_market_price[[Num_crypt3]][["last"]]))
             table_Crypto[1,10] <- 1/(as.numeric(data_market_price[[Num_crypt1]][["last"]]))
             table_Crypto[1,11] <- as.numeric(data_market_price[[Num_crypt3]][["last"]])
             table_Crypto[1,12] <- 1/(as.numeric(data_market_price[[Num_crypt2]][["last"]]))
             # при обратной ------------------------------------------------
             table_Crypto[1,6] <- as.numeric(data_market_price[[Num_crypt3]][["last"]])
             table_Crypto[1,7] <- 1/(as.numeric(data_market_price[[Num_crypt2]][["last"]]))
             table_Crypto[1,8] <- 1/(as.numeric(data_market_price[[Num_crypt1]][["last"]]))
             table_Crypto[1,14] <- as.numeric(data_market_price[[Num_crypt2]][["last"]])
             table_Crypto[1,15] <- 1/(as.numeric(data_market_price[[Num_crypt3]][["last"]]))
             table_Crypto[1,16] <- as.numeric(data_market_price[[Num_crypt1]][["last"]])
           })
    # ------------------
    return(table_Crypto)
  }
  
  # -----------------------------------------------------------------------------------
  # узнать номер криптовалютной пары для первых 3 параметров в нижних функциях
  #what_num_crypt<-as.data.frame.raw(data_market_price)
  #View(what_num_crypt)
  
  # заполняем значение курса
  table_BCH_USDT <- get_price_poloniex("BTC_BCH", "USDT_BTC", "USDT_BCH", table_BCH_USDT, lim_or_mark, data_market_price)
  table_BCH_ETH <- get_price_poloniex("ETH_BCH", "BTC_ETH", "BTC_BCH", table_BCH_ETH, lim_or_mark, data_market_price)
  table_DASH_USDT <- get_price_poloniex("BTC_DASH", "USDT_BTC", "USDT_DASH", table_DASH_USDT, lim_or_mark, data_market_price)
  table_EOS_USDT <- get_price_poloniex("BTC_EOS", "USDT_BTC", "USDT_EOS", table_EOS_USDT, lim_or_mark, data_market_price)
  table_EOS_ETH <- get_price_poloniex("ETH_EOS", "BTC_ETH", "BTC_EOS", table_EOS_ETH, lim_or_mark, data_market_price)
  table_ETC_USDT <- get_price_poloniex("BTC_ETC", "USDT_BTC", "USDT_ETC", table_ETC_USDT, lim_or_mark, data_market_price)
  table_ETC_ETH <- get_price_poloniex("ETH_ETC", "BTC_ETH", "BTC_ETC", table_ETC_ETH, lim_or_mark, data_market_price)
  table_GNT_ETH <- get_price_poloniex("ETH_GNT", "BTC_ETH", "BTC_GNT", table_GNT_ETH, lim_or_mark, data_market_price)
  table_LTC_USDT <- get_price_poloniex("BTC_LTC", "USDT_BTC", "USDT_LTC", table_LTC_USDT, lim_or_mark, data_market_price)
  table_LSK_ETH <- get_price_poloniex("ETH_LSK", "BTC_ETH", "BTC_LSK", table_LSK_ETH, lim_or_mark, data_market_price)
  table_OMG_ETH <- get_price_poloniex("ETH_OMG", "BTC_ETH", "BTC_OMG", table_OMG_ETH, lim_or_mark, data_market_price)
  table_REP_USDT <- get_price_poloniex("BTC_REP", "USDT_BTC", "USDT_REP", table_REP_USDT, lim_or_mark, data_market_price)
  table_REP_ETH <- get_price_poloniex("ETH_REP", "BTC_ETH", "BTC_REP", table_REP_ETH, lim_or_mark, data_market_price)
  table_STR_USDT <- get_price_poloniex("BTC_STR", "USDT_BTC", "USDT_STR", table_STR_USDT, lim_or_mark, data_market_price)
  table_XMR_USDT <- get_price_poloniex("BTC_XMR", "USDT_BTC", "USDT_XMR", table_XMR_USDT, lim_or_mark, data_market_price)
  table_XRP_USDT <- get_price_poloniex("BTC_XRP", "USDT_BTC", "USDT_XRP", table_XRP_USDT, lim_or_mark, data_market_price)
  table_ZEC_USDT <- get_price_poloniex("BTC_ZEC", "USDT_BTC", "USDT_ZEC", table_ZEC_USDT, lim_or_mark, data_market_price)
  table_ZEC_ETH <- get_price_poloniex("ETH_ZEC", "BTC_ETH", "BTC_ZEC", table_ZEC_ETH, lim_or_mark, data_market_price)
  table_ZRX_ETH <- get_price_poloniex("ETH_ZRX", "BTC_ETH", "BTC_ZRX", table_ZRX_ETH, lim_or_mark, data_market_price)
  
  # -----------------------------------------------------------------------------------
  # выполняем расчет и заполняем строчки
  # https://poloniex.com/public?command=returnTicker - тут смотрим делители, а именно колличество знаков после запятой (del1.1 - last первой пары, del1.2 - last первой пары, del2.1 - last второй пары, del2.2 - last второй пары, del3.1 - last третьей пары, del3.2 - last третьей пары)
  table_BCH_USDT <- find_balance(table_BCH_USDT, BCH, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_BCH_ETH <- find_balance(table_BCH_ETH, BCH, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_DASH_USDT <- find_balance(table_DASH_USDT, DASH, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_EOS_USDT <- find_balance(table_EOS_USDT, EOS, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_EOS_ETH <- find_balance(table_EOS_ETH, EOS, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_ETC_USDT <- find_balance(table_ETC_USDT, ETC, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_ETC_ETH <- find_balance(table_ETC_ETH, ETC, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_GNT_ETH <- find_balance(table_GNT_ETH, GNT, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_LTC_USDT <- find_balance(table_LTC_USDT, LTC, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_LSK_ETH <- find_balance(table_LSK_ETH, LSK, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_OMG_ETH <- find_balance(table_OMG_ETH, OMG, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_REP_USDT <- find_balance(table_REP_USDT, REP, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_REP_ETH <- find_balance(table_REP_ETH, REP, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_STR_USDT <- find_balance(table_STR_USDT, STR, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_XMR_USDT <- find_balance(table_XMR_USDT, XMR, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_XRP_USDT <- find_balance(table_XRP_USDT, XRP, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_ZEC_USDT <- find_balance(table_ZEC_USDT, ZEC, BTC, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_ZEC_ETH <- find_balance(table_ZEC_ETH, ZEC, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  table_ZRX_ETH <- find_balance(table_ZRX_ETH, ZRX, ETH, poloniex_fee, 8, 8, 8, 8, 8, 8)
  
  # -----------------------------------------------------------------------------------
  # готовим отчет
  finish_table <- find_result(finish_table, 1, table_BCH_USDT, BCH, BTC, "BCH->BTC->USDT->BCH", "BCH->USDT->BTC->BCH", "BTC->BCH->USDT->BTC", "BTC->USDT->BCH->BTC")
  finish_table <- find_result(finish_table, 2, table_BCH_ETH, BCH, ETH, "BCH->ETH->BTC->BCH", "BCH->BTC->ETH->BCH", "ETH->BCH->BTC->ETH", "ETH->BTC->BCH->ETH")
  finish_table <- find_result(finish_table, 3, table_DASH_USDT, DASH, BTC, "DASH->BTC->USDT->DASH", "DASH->USDT->BTC->DASH", "BTC->DASH->USDT->BTC", "BTC->USDT->DASH->BTC")
  finish_table <- find_result(finish_table, 4, table_EOS_USDT, EOS, BTC, "EOS->BTC->USDT->EOS", "EOS->USDT->BTC->EOS", "BTC->EOS->USDT->BTC", "BTC->USDT->EOS->BTC")
  finish_table <- find_result(finish_table, 5, table_EOS_ETH, EOS, ETH, "EOS->ETH->BTC->EOS", "EOS->BTC->ETH->EOS", "ETH->EOS->BTC->ETH", "ETH->BTC->EOS->ETH")
  finish_table <- find_result(finish_table, 6, table_ETC_USDT, ETC, BTC, "ETC->BTC->USDT->ETC", "ETC->USDT->BTC->ETC", "BTC->ETC->USDT->BTC", "BTC->USDT->ETC->BTC")
  finish_table <- find_result(finish_table, 7, table_ETC_ETH, ETC, ETH, "ETC->ETH->BTC->ETC", "ETC->BTC->ETH->ETC", "ETH->ETC->BTC->ETH", "ETH->BTC->ETC->ETH")
  finish_table <- find_result(finish_table, 8, table_GNT_ETH, GNT, ETH, "GNT->ETH->BTC->GNT", "GNT->BTC->ETH->GNT", "ETH->GNT->BTC->ETH", "ETH->BTC->GNT->ETH")
  finish_table <- find_result(finish_table, 9, table_LTC_USDT, LTC, BTC, "LTC->BTC->USDT->LTC", "LTC->USDT->BTC->LTC", "BTC->LTC->USDT->BTC", "BTC->USDT->LTC->BTC")
  finish_table <- find_result(finish_table, 10, table_LSK_ETH, LSK, ETH, "LSK->ETH->BTC->LSK", "LSK->BTC->ETH->LSK", "ETH->LSK->BTC->ETH", "ETH->BTC->LSK->ETH")
  finish_table <- find_result(finish_table, 12, table_OMG_ETH, OMG, ETH, "OMG->ETH->BTC->OMG", "OMG->BTC->ETH->OMG", "ETH->OMG->BTC->ETH", "ETH->BTC->OMG->ETH")
  finish_table <- find_result(finish_table, 13, table_REP_USDT, REP, BTC, "REP->BTC->USDT->REP", "REP->USDT->BTC->REP", "BTC->REP->USDT->BTC", "BTC->USDT->REP->BTC")
  finish_table <- find_result(finish_table, 14, table_REP_ETH, REP, ETH, "REP->ETH->BTC->REP", "REP->BTC->ETH->REP", "ETH->REP->BTC->ETH", "ETH->BTC->REP->ETH")
  finish_table <- find_result(finish_table, 15, table_STR_USDT, STR, BTC, "STR->BTC->USDT->STR", "STR->USDT->BTC->STR", "BTC->STR->USDT->BTC", "BTC->USDT->STR->BTC")
  finish_table <- find_result(finish_table, 16, table_XMR_USDT, XMR, BTC, "XMR->BTC->USDT->XMR", "XMR->USDT->BTC->XMR", "BTC->XMR->USDT->BTC", "BTC->USDT->XMR->BTC")
  finish_table <- find_result(finish_table, 17, table_XRP_USDT, XRP, BTC, "XRP->BTC->USDT->XRP", "XRP->USDT->BTC->XRP", "BTC->XRP->USDT->BTC", "BTC->USDT->XRP->BTC")
  finish_table <- find_result(finish_table, 18, table_ZEC_USDT, ZEC, BTC, "ZEC->BTC->USDT->ZEC", "ZEC->USDT->BTC->ZEC", "BTC->ZEC->USDT->BTC", "BTC->USDT->ZEC->BTC")
  finish_table <- find_result(finish_table, 19, table_ZEC_ETH, ZEC, ETH, "ZEC->ETH->BTC->ZEC", "ZEC->BTC->ETH->ZEC", "ETH->ZEC->BTC->ETH", "ETH->BTC->ZEC->ETH")
  finish_table <- find_result(finish_table, 11, table_ZRX_ETH, ZRX, ETH, "ZRX->ETH->BTC->ZRX", "ZRX->BTC->ETH->ZRX", "ETH->ZRX->BTC->ETH", "ETH->BTC->ZRX->ETH")
  
  # ------------------
  return(finish_table)
}
