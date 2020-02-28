# заполняем строчку курсов и считаем баланс после каждой трнзакции
find_balance <- function(table_Crypto, Crypto1, Crypto2, exchange_fee, del1.1=8, del1.2=8, del2.1=8, del2.2=8, del3.1=8, del3.2=8){
  # считаем баланс после каждой трнзакции
  # при прямой ---------------------------------------------------------
  table_Crypto[2,2] <- round(Crypto1-(5*(0.1^(del1.1+1))),del1.1)-(round(Crypto1-(5*(0.1^(del1.1+1))),del1.1)*exchange_fee)
  table_Crypto[3,2] <- table_Crypto[1,2]*table_Crypto[2,2]
  table_Crypto[2,3] <- round(table_Crypto[3,2]-(5*(0.1^(del2.1+1))),del2.1)-(round(table_Crypto[3,2]-(5*(0.1^(del2.1+1))),del2.1)*exchange_fee)
  table_Crypto[3,3] <- table_Crypto[1,3]*table_Crypto[2,3]
  table_Crypto[2,4] <- round(table_Crypto[3,3]-(5*(0.1^(del3.2+1))),del3.2)-(round(table_Crypto[3,3]-(5*(0.1^(del3.2+1))),del3.2)*exchange_fee)
  table_Crypto[3,4] <- table_Crypto[1,4]*table_Crypto[2,4]
  table_Crypto[2,10] <- round(Crypto2-(5*(0.1^(del1.2+1))),del1.2)-(round(Crypto2-(5*(0.1^(del1.2+1))),del1.2)*exchange_fee)
  table_Crypto[3,10] <- table_Crypto[1,10]*table_Crypto[2,10]
  table_Crypto[2,11] <- round(table_Crypto[3,10]-(5*(0.1^(del3.1+1))),del3.1)-(round(table_Crypto[3,10]-(5*(0.1^(del3.1+1))),del3.1)*exchange_fee)
  table_Crypto[3,11] <- table_Crypto[1,11]*table_Crypto[2,11]
  table_Crypto[2,12] <- round(table_Crypto[3,11]-(5*(0.1^(del2.2+1))),del2.2)-(round(table_Crypto[3,11]-(5*(0.1^(del2.2+1))),del2.2)*exchange_fee)
  table_Crypto[3,12] <- table_Crypto[1,12]*table_Crypto[2,12]
  # при обратной -------------------------------------------------------
  table_Crypto[2,6] <- round(Crypto1-(5*(0.1^(del3.1+1))),del3.1)-(round(Crypto1-(5*(0.1^(del3.1+1))),del3.1)*exchange_fee)
  table_Crypto[3,6] <- table_Crypto[1,6]*table_Crypto[2,6]
  table_Crypto[2,7] <- round(table_Crypto[3,6]-(5*(0.1^(del2.2+1))),del2.2)-(round(table_Crypto[3,6]-(5*(0.1^(del2.2+1))),del2.2)*exchange_fee)
  table_Crypto[3,7] <- table_Crypto[1,7]*table_Crypto[2,7]
  table_Crypto[2,8] <- round(table_Crypto[3,7]-(5*(0.1^(del1.2+1))),del1.2)-(round(table_Crypto[3,7]-(5*(0.1^(del1.2+1))),del1.2)*exchange_fee)
  table_Crypto[3,8] <- table_Crypto[1,8]*table_Crypto[2,8]
  table_Crypto[2,14] <- round(Crypto2-(5*(0.1^(del2.1+1))),del2.1)-(round(Crypto2-(5*(0.1^(del2.1+1))),del2.1)*exchange_fee)
  table_Crypto[3,14] <- table_Crypto[1,14]*table_Crypto[2,14]
  table_Crypto[2,15] <- round(table_Crypto[3,14]-(5*(0.1^(del3.2+1))),del3.2)-(round(table_Crypto[3,14]-(5*(0.1^(del3.2+1))),del3.2)*exchange_fee)
  table_Crypto[3,15] <- table_Crypto[1,15]*table_Crypto[2,15]
  table_Crypto[2,16] <- round(table_Crypto[3,15]-(5*(0.1^(del1.1+1))),del1.1)-(round(table_Crypto[3,15]-(5*(0.1^(del1.1+1))),del1.1)*exchange_fee)
  table_Crypto[3,16] <- table_Crypto[1,16]*table_Crypto[2,16]
  # ------------------
  return(table_Crypto)
}

# расчет результата расчетов
find_result <- function(main_tab, num_Crypto, table_Crypto, Crypto1, Crypto2, Plan1, Plan2, Plan3, Plan4){
  num_Crypto <- num_Crypto*4
  
  main_tab[num_Crypto-3,1] <- Plan1
  main_tab[num_Crypto-2,1] <- Plan2
  main_tab[num_Crypto-1,1] <- Plan3
  main_tab[num_Crypto,1] <- Plan4
  
  main_tab[num_Crypto-3,2] <- round(((table_Crypto[3,4]-Crypto1)*10000/Crypto1),2)
  main_tab[num_Crypto-2,2] <- round(((table_Crypto[3,8]-Crypto1)*10000/Crypto1),2)
  main_tab[num_Crypto-1,2] <- round(((table_Crypto[3,12]-Crypto2)*10000/Crypto2),2)
  main_tab[num_Crypto,2] <- round(((table_Crypto[3,16]-Crypto2)*10000/Crypto2),2)
  
  main_tab[num_Crypto-3,3] <- round(((table_Crypto[3,4]-Crypto1)*100/Crypto1)-(0.005),2)
  main_tab[num_Crypto-2,3] <- round(((table_Crypto[3,8]-Crypto1)*100/Crypto1)-(0.005),2)
  main_tab[num_Crypto-1,3] <- round(((table_Crypto[3,12]-Crypto2)*100/Crypto2)-(0.005),2)
  main_tab[num_Crypto,3] <- round(((table_Crypto[3,16]-Crypto2)*100/Crypto2)-(0.005),2)
  # ------------------
  return(main_tab)
}
