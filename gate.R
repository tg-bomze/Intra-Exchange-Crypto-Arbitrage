source("main_func.R")

gate_func <- function(finish_table, lim_or_mark, data_market_price){
  # заполнение курсов binance
  get_price_gate <- function(Num_crypt1, Num_crypt2, Num_crypt3, table_Crypto, lim_or_mark, data_market_price){
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
  table_ADA_USDT <- get_price_gate("ada_btc", "btc_usdt", "ada_usdt", table_ADA_USDT, lim_or_mark, data_market_price)
  table_AE_USDT <- get_price_gate("ae_btc", "btc_usdt", "ae_usdt", table_AE_USDT, lim_or_mark, data_market_price)
  table_AE_ETH <- get_price_gate("ae_eth", "eth_btc", "ae_btc", table_AE_ETH, lim_or_mark, data_market_price)
  table_BCH_USDT <- get_price_gate("bch_btc", "btc_usdt", "bch_usdt", table_BCH_USDT, lim_or_mark, data_market_price)
  table_BTS_USDT <- get_price_gate("bts_btc", "btc_usdt", "bts_usdt", table_BTS_USDT, lim_or_mark, data_market_price)
  table_DASH_USDT <- get_price_gate("dash_btc", "btc_usdt", "dash_usdt", table_DASH_USDT, lim_or_mark, data_market_price)
  table_DOGE_USDT <- get_price_gate("doge_btc", "btc_usdt", "doge_usdt", table_DOGE_USDT, lim_or_mark, data_market_price)
  table_EOS_USDT <- get_price_gate("eos_btc", "btc_usdt", "eos_usdt", table_EOS_USDT, lim_or_mark, data_market_price)
  table_EOS_ETH <- get_price_gate("eos_eth", "eth_btc", "eos_btc", table_EOS_ETH, lim_or_mark, data_market_price)
  table_ETC_USDT <- get_price_gate("etc_btc", "btc_usdt", "etc_usdt", table_ETC_USDT, lim_or_mark, data_market_price)
  table_ETC_ETH <- get_price_gate("etc_eth", "eth_btc", "etc_btc", table_ETC_ETH, lim_or_mark, data_market_price)
  table_ETH_USDT <- get_price_gate("eth_btc", "btc_usdt", "eth_usdt", table_ETH_USDT, lim_or_mark, data_market_price)
  table_LTC_USDT <- get_price_gate("ltc_btc", "btc_usdt", "ltc_usdt", table_LTC_USDT, lim_or_mark, data_market_price)
  table_NEO_USDT <- get_price_gate("neo_btc", "btc_usdt", "neo_usdt", table_NEO_USDT, lim_or_mark, data_market_price)
  table_OMG_USDT <- get_price_gate("omg_btc", "btc_usdt", "omg_usdt", table_OMG_USDT, lim_or_mark, data_market_price)
  table_OMG_ETH <- get_price_gate("omg_eth", "eth_btc", "omg_btc", table_OMG_ETH, lim_or_mark, data_market_price)
  table_QTUM_USDT <- get_price_gate("qtum_btc", "btc_usdt", "qtum_usdt", table_QTUM_USDT, lim_or_mark, data_market_price)
  table_QTUM_ETH <- get_price_gate("qtum_eth", "eth_btc", "qtum_btc", table_QTUM_ETH, lim_or_mark, data_market_price)
  table_SNT_USDT <- get_price_gate("snt_btc", "btc_usdt", "snt_usdt", table_SNT_USDT, lim_or_mark, data_market_price)
  table_SNT_ETH <- get_price_gate("snt_eth", "eth_btc", "snt_btc", table_SNT_ETH, lim_or_mark, data_market_price)
  table_XLM_USDT <- get_price_gate("xlm_btc", "btc_usdt", "xlm_usdt", table_XLM_USDT, lim_or_mark, data_market_price)
  table_XLM_ETH <- get_price_gate("xlm_eth", "eth_btc", "xlm_btc", table_XLM_ETH, lim_or_mark, data_market_price)
  table_XMR_USDT <- get_price_gate("xmr_btc", "btc_usdt", "xmr_usdt", table_XMR_USDT, lim_or_mark, data_market_price)
  table_XRP_USDT <- get_price_gate("xrp_btc", "btc_usdt", "xrp_usdt", table_XRP_USDT, lim_or_mark, data_market_price)
  table_ZEC_USDT <- get_price_gate("zec_btc", "btc_usdt", "zec_usdt", table_ZEC_USDT, lim_or_mark, data_market_price)
  table_TRX_ETH <- get_price_gate("trx_eth", "eth_usdt", "trx_usdt", table_TRX_ETH, lim_or_mark, data_market_price)
  table_ONT_ETH <- get_price_gate("ont_eth", "eth_usdt", "ont_usdt", table_ONT_ETH, lim_or_mark, data_market_price)
  table_VET_ETH <- get_price_gate("vet_eth", "eth_usdt", "vet_usdt", table_VET_ETH, lim_or_mark, data_market_price)
  
  # -----------------------------------------------------------------------------------
  # выполняем расчет и заполняем строчки
  # https://data.gate.io/api/1/marketinfo - тут смотрим делители, а именно колличество знаков после запятой (del1.1 - min_amount первой пары, del1.2 - decimal_places первой пары, del2.1 - min_amount второй пары, del2.2 - decimal_places второй пары, del3.1 - min_amount третьей пары, del3.2 - decimal_places третьей пары)
  table_ADA_USDT <- find_balance(table_ADA_USDT, ADA, BTC, gate_fee, 4, 8, 4, 2, 4, 4)
  table_AE_USDT <- find_balance(table_AE_USDT, AE, BTC, gate_fee, 4, 8, 4, 2, 4, 4)
  table_AE_ETH <- find_balance(table_AE_ETH, AE, ETH, gate_fee, 4, 6, 3, 6, 4, 8)
  table_BCH_USDT <- find_balance(table_BCH_USDT, BCH, BTC, gate_fee, 4, 4, 4, 2, 4, 2)
  table_BTS_USDT <- find_balance(table_BTS_USDT, BTS, BTC, gate_fee, 4, 8, 4, 2, 4, 4)
  table_DASH_USDT <- find_balance(table_DASH_USDT, DASH, BTC, gate_fee, 4, 5, 4, 2, 4, 2)
  table_DOGE_USDT <- find_balance(table_DOGE_USDT, DOGE, BTC, gate_fee, 4, 8, 4, 2, 4, 6)
  table_EOS_USDT <- find_balance(table_EOS_USDT, EOS, BTC, gate_fee, 4, 7, 4, 2, 4, 4)
  table_EOS_ETH <- find_balance(table_EOS_ETH, EOS, ETH, gate_fee, 4, 6, 3, 6, 4, 7)
  table_ETC_USDT <- find_balance(table_ETC_USDT, ETC, BTC, gate_fee, 4, 6, 4, 2, 4, 2)
  table_ETC_ETH <- find_balance(table_ETC_ETH, ETC, ETH, gate_fee, 4, 6, 3, 6, 4, 6)
  table_ETH_USDT <- find_balance(table_ETH_USDT, ETH, BTC, gate_fee, 3, 6, 4, 2, 3, 2)
  table_LTC_USDT <- find_balance(table_LTC_USDT, LTC, BTC, gate_fee, 3, 5, 4, 2, 3, 2)
  table_NEO_USDT <- find_balance(table_NEO_USDT, NEO, BTC, gate_fee, 4, 8, 4, 2, 4, 2)
  table_OMG_USDT <- find_balance(table_OMG_USDT, OMG, BTC, gate_fee, 4, 6, 4, 2, 4, 2)
  table_OMG_ETH <- find_balance(table_OMG_ETH, OMG, ETH, gate_fee, 4, 5, 3, 6, 4, 6)
  table_QTUM_USDT <- find_balance(table_QTUM_USDT, QTUM, BTC, gate_fee, 2, 6, 4, 2, 2, 2)
  table_QTUM_ETH <- find_balance(table_QTUM_ETH, QTUM, ETH, gate_fee, 2, 5, 3, 6, 2, 6)
  table_SNT_USDT <- find_balance(table_SNT_USDT, SNT, BTC, gate_fee, 4, 7, 4, 2, 4, 4)
  table_SNT_ETH <- find_balance(table_SNT_ETH, SNT, ETH, gate_fee, 4, 7, 3, 6, 4, 7)
  table_XLM_USDT <- find_balance(table_XLM_USDT, XLM, BTC, gate_fee, 4, 8, 4, 2, 4, 4)
  table_XLM_ETH <- find_balance(table_XLM_ETH, XLM, ETH, gate_fee, 4, 6, 3, 6, 4, 8)
  table_XMR_USDT <- find_balance(table_XMR_USDT, XMR, BTC, gate_fee, 4, 8, 4, 2, 4, 2)
  table_XRP_USDT <- find_balance(table_XRP_USDT, XRP, BTC, gate_fee, 4, 7, 4, 2, 4, 4)
  table_ZEC_USDT <- find_balance(table_ZEC_USDT, ZEC, BTC, gate_fee, 4, 4, 4, 2, 4, 2)
  table_TRX_ETH <- find_balance(table_TRX_ETH, TRX, ETH, gate_fee, 4, 8, 3, 2, 4, 5)
  table_ONT_ETH <- find_balance(table_ONT_ETH, ONT, ETH, gate_fee, 4, 8, 3, 2, 4, 3)
  table_VET_ETH <- find_balance(table_VET_ETH, VET, ETH, gate_fee, 4, 6, 3, 2, 4, 4)
  
  # -----------------------------------------------------------------------------------
  # готовим отчет
  finish_table <- find_result(finish_table, 1, table_ADA_USDT, ADA, BTC, "ADA->BTC->USDT->ADA", "ADA->USDT->BTC->ADA", "BTC->ADA->USDT->BTC", "BTC->USDT->ADA->BTC")
  finish_table <- find_result(finish_table, 2, table_AE_USDT, AE, BTC, "AE->BTC->USDT->AE", "AE->USDT->BTC->AE", "BTC->AE->USDT->BTC", "BTC->USDT->AE->BTC")
  finish_table <- find_result(finish_table, 3, table_BCH_USDT, BCH, BTC, "BCH->BTC->USDT->BCH", "BCH->USDT->BTC->BCH", "BTC->BCH->USDT->BTC", "BTC->USDT->BCH->BTC")
  finish_table <- find_result(finish_table, 4, table_BTS_USDT, BTS, BTC, "BTS->BTC->USDT->BTS", "BTS->USDT->BTC->BTS", "BTC->BTS->USDT->BTC", "BTC->USDT->BTS->BTC")
  finish_table <- find_result(finish_table, 5, table_DASH_USDT, DASH, BTC, "DASH->BTC->USDT->DASH", "DASH->USDT->BTC->DASH", "BTC->DASH->USDT->BTC", "BTC->USDT->DASH->BTC")
  finish_table <- find_result(finish_table, 6, table_DOGE_USDT, DOGE, BTC, "DOGE->BTC->USDT->DOGE", "DOGE->USDT->BTC->DOGE", "BTC->DOGE->USDT->BTC", "BTC->USDT->DOGE->BTC")
  finish_table <- find_result(finish_table, 7, table_EOS_USDT, EOS, BTC, "EOS->BTC->USDT->EOS", "EOS->USDT->BTC->EOS", "BTC->EOS->USDT->BTC", "BTC->USDT->EOS->BTC")
  finish_table <- find_result(finish_table, 8, table_EOS_ETH, EOS, ETH, "EOS->ETH->BTC->EOS", "EOS->BTC->ETH->EOS", "ETH->EOS->BTC->ETH", "ETH->BTC->EOS->ETH")
  finish_table <- find_result(finish_table, 9, table_ETC_USDT, ETC, BTC, "ETC->BTC->USDT->ETC", "ETC->USDT->BTC->ETC", "BTC->ETC->USDT->BTC", "BTC->USDT->ETC->BTC")
  finish_table <- find_result(finish_table, 10, table_ETC_ETH, ETC, ETH, "ETC->ETH->BTC->ETC", "ETC->BTC->ETH->ETC", "ETH->ETC->BTC->ETH", "ETH->BTC->ETC->ETH")
  finish_table <- find_result(finish_table, 11, table_ETH_USDT, ETH, BTC, "ETH->BTC->USDT->ETH", "ETH->USDT->BTC->ETH", "BTC->ETH->USDT->BTC", "BTC->USDT->ETH->BTC")
  finish_table <- find_result(finish_table, 12, table_LTC_USDT, LTC, BTC, "LTC->BTC->USDT->LTC", "LTC->USDT->BTC->LTC", "BTC->LTC->USDT->BTC", "BTC->USDT->LTC->BTC")
  finish_table <- find_result(finish_table, 13, table_NEO_USDT, NEO, BTC, "NEO->BTC->USDT->NEO", "NEO->USDT->BTC->NEO", "BTC->NEO->USDT->BTC", "BTC->USDT->NEO->BTC")
  finish_table <- find_result(finish_table, 14, table_OMG_USDT, OMG, BTC, "OMG->BTC->USDT->OMG", "OMG->USDT->BTC->OMG", "BTC->OMG->USDT->BTC", "BTC->USDT->OMG->BTC")
  finish_table <- find_result(finish_table, 15, table_OMG_ETH, OMG, ETH, "OMG->ETH->BTC->OMG", "OMG->BTC->ETH->OMG", "ETH->OMG->BTC->ETH", "ETH->BTC->OMG->ETH")
  finish_table <- find_result(finish_table, 16, table_QTUM_USDT, QTUM, BTC, "QTUM->BTC->USDT->QTUM", "QTUM->USDT->BTC->QTUM", "BTC->QTUM->USDT->BTC", "BTC->USDT->QTUM->BTC")
  finish_table <- find_result(finish_table, 17, table_QTUM_ETH, QTUM, ETH, "QTUM->ETH->BTC->QTUM", "QTUM->BTC->ETH->QTUM", "ETH->QTUM->BTC->ETH", "ETH->BTC->QTUM->ETH")
  finish_table <- find_result(finish_table, 18, table_SNT_USDT, SNT, BTC, "SNT->BTC->USDT->SNT", "SNT->USDT->BTC->SNT", "BTC->SNT->USDT->BTC", "BTC->USDT->SNT->BTC")
  finish_table <- find_result(finish_table, 19, table_SNT_ETH, SNT, ETH, "SNT->ETH->BTC->SNT", "SNT->BTC->ETH->SNT", "ETH->SNT->BTC->ETH", "ETH->BTC->SNT->ETH")
  finish_table <- find_result(finish_table, 20, table_XLM_USDT, XLM, BTC, "XLM->BTC->USDT->XLM", "XLM->USDT->BTC->XLM", "BTC->XLM->USDT->BTC", "BTC->USDT->XLM->BTC")
  finish_table <- find_result(finish_table, 21, table_XLM_ETH, XLM, ETH, "XLM->ETH->BTC->XLM", "XLM->BTC->ETH->XLM", "ETH->XLM->BTC->ETH", "ETH->BTC->XLM->ETH")
  finish_table <- find_result(finish_table, 22, table_XMR_USDT, XMR, BTC, "XMR->BTC->USDT->XMR", "XMR->USDT->BTC->XMR", "BTC->XMR->USDT->BTC", "BTC->USDT->XMR->BTC")
  finish_table <- find_result(finish_table, 23, table_XRP_USDT, XRP, BTC, "XRP->BTC->USDT->XRP", "XRP->USDT->BTC->XRP", "BTC->XRP->USDT->BTC", "BTC->USDT->XRP->BTC")
  finish_table <- find_result(finish_table, 24, table_ZEC_USDT, ZEC, BTC, "ZEC->BTC->USDT->ZEC", "ZEC->USDT->BTC->ZEC", "BTC->ZEC->USDT->BTC", "BTC->USDT->ZEC->BTC")
  finish_table <- find_result(finish_table, 25, table_TRX_ETH, TRX, ETH, "TRX->ETH->USDT->TRX", "TRX->USDT->ETH->TRX", "ETH->TRX->USDT->ETH", "ETH->USDT->TRX->ETH")
  finish_table <- find_result(finish_table, 26, table_ONT_ETH, ONT, ETH, "ONT->ETH->USDT->ONT", "ONT->USDT->ETH->ONT", "ETH->ONT->USDT->ETH", "ETH->USDT->ONT->ETH")
  finish_table <- find_result(finish_table, 27, table_VET_ETH, VET, ETH, "VET->ETH->USDT->VET", "VET->USDT->ETH->VET", "ETH->VET->USDT->ETH", "ETH->USDT->VET->ETH")
  finish_table <- find_result(finish_table, 28, table_AE_ETH, AE, ETH, "AE->ETH->BTC->AE", "AE->BTC->ETH->AE", "ETH->AE->BTC->ETH", "ETH->BTC->AE->ETH")
  
  # ------------------
  return(finish_table)
}
