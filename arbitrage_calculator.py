import decimal
decimal.getcontext().prec = 30

def calculate_trade(amount, fee):
    """Calculates the resulting amount after a trade, applying fee."""
    # Convert amount to Decimal if it's not already
    if not isinstance(amount, decimal.Decimal):
        amount = decimal.Decimal(str(amount))
        
    if amount <= 0:
      return decimal.Decimal('0')

    # Apply fee directly
    final_amount = amount * (1 - fee)
    
    return final_amount

def find_balance(prices, initial_balance_crypto1, initial_balance_crypto2, fee):
    """ 
    Calculates the final balances after executing four different 
    triangular arbitrage transaction chains based on provided prices and parameters.
    
    Args:
        prices (dict): A dictionary containing the necessary prices:
            'price1_bid', 'price1_ask', 
            'price2_bid', 'price2_ask',
            'price3_bid', 'price3_ask'
            Note: Prices should be strings or Decimals for precision.
        initial_balance_crypto1 (Decimal): Initial amount of the first crypto.
        initial_balance_crypto2 (Decimal): Initial amount of the second crypto (used for reverse chains).
        fee (Decimal): The exchange fee per transaction.

    Returns:
        dict: A dictionary containing the final balances for each of the four chains:
              'chain1_final', 'chain2_final', 'chain3_final', 'chain4_final'
    """
    results = {}
    
    # Convert all input prices and balances to Decimal for precision
    p = {k: decimal.Decimal(str(v)) if v is not None else decimal.Decimal('0') for k, v in prices.items()}
    bal1 = decimal.Decimal(str(initial_balance_crypto1))
    bal2 = decimal.Decimal(str(initial_balance_crypto2))
    fee_dec = decimal.Decimal(str(fee))

    # --- Chain 1: Crypto1 -> Crypto2 -> Base -> Crypto1 (using bids where selling, asks where buying) --- 
    # Step 1: Sell Crypto1 for Crypto2 (Price: price1_bid)
    step1_crypto1_after_trade = calculate_trade(bal1, fee_dec)
    step1_crypto2_received = step1_crypto1_after_trade * p.get('price1_bid', 0)
    
    # Step 2: Sell Crypto2 for Base (Price: price2_bid)
    step2_crypto2_after_trade = calculate_trade(step1_crypto2_received, fee_dec)
    step2_base_received = step2_crypto2_after_trade * p.get('price2_bid', 0)
    
    # Step 3: Buy Crypto1 with Base (Price: 1 / price3_ask)
    # Need to handle potential division by zero if price3_ask is 0
    price3_ask_inv = (1 / p['price3_ask']) if p.get('price3_ask', 0) > 0 else decimal.Decimal('0') 
    step3_base_after_trade = calculate_trade(step2_base_received, fee_dec)
    results['chain1_final'] = step3_base_after_trade * price3_ask_inv
    
    # --- Chain 2: Crypto2 -> Base -> Crypto1 -> Crypto2 (using bids/asks appropriately) ---
    # Step 1: Sell Crypto2 for Base (Price: price2_bid) - Wait, R code uses 1/price1_ask for [1,10]...
    # Let's re-evaluate the R code's column mapping carefully. 
    # R columns: [1,2]=p1b, [1,3]=p2b, [1,4]=1/p3a | [1,10]=1/p1a, [1,11]=p3b, [1,12]=1/p2a
    # R chain 2 (cols 10, 11, 12) starts with Crypto2, uses prices at [1,10], [1,11], [1,12]
    # Step 1: Trade Crypto2 using price at [1,10] (1/price1_ask)
    # This seems unusual. Does it mean Sell Base for Crypto1? Let's assume the R comments describe the path better.
    # Path: BTC->USDT->ADA->BTC (R comment for table_ADA_USDT, Plan3)
    # This uses cols 10, 11, 12. Start with Crypto2 (BTC). 
    # Price[1,10]=1/ADAUSDT_ask, Price[1,11]=ADABTC_bid, Price[1,12]=1/BTCUSDT_ask
    # So, Step 1: Sell BTC for ADA? (Using 1/ADAUSDT_ask) - This doesn't fit the price pair. 
    # Let's stick to the R calculation logic for now, even if the path description seems off.
    
    # Chain 2 (Following R cols 10, 11, 12 calculations)
    # Starting with Crypto2 (e.g., BTC)
    price10 = (1 / p['price1_ask']) if p.get('price1_ask', 0) > 0 else decimal.Decimal('0')
    price11 = p.get('price3_bid', 0)
    price12 = (1 / p['price2_ask']) if p.get('price2_ask', 0) > 0 else decimal.Decimal('0')

    step1_crypto2_after_trade_c2 = calculate_trade(bal2, fee_dec)
    step1_intermediate_c2 = step1_crypto2_after_trade_c2 * price10
    
    step2_intermediate_after_trade_c2 = calculate_trade(step1_intermediate_c2, fee_dec)
    step2_intermediate2_c2 = step2_intermediate_after_trade_c2 * price11

    step3_intermediate2_after_trade_c2 = calculate_trade(step2_intermediate2_c2, fee_dec)
    results['chain2_final'] = step3_intermediate2_after_trade_c2 * price12
    
    # --- Chain 3: Crypto1 -> Base -> Crypto2 -> Crypto1 (Reverse 1) ---
    # R columns: [1,6]=p3b, [1,7]=1/p2a, [1,8]=1/p1a
    # Path: ADA->BTC->ETH->ADA (R comment for table_ADA_ETH, Plan2)
    price6 = p.get('price3_bid', 0)
    price7 = (1 / p['price2_ask']) if p.get('price2_ask', 0) > 0 else decimal.Decimal('0')
    price8 = (1 / p['price1_ask']) if p.get('price1_ask', 0) > 0 else decimal.Decimal('0')

    step1_crypto1_after_trade_c3 = calculate_trade(bal1, fee_dec)
    step1_intermediate_c3 = step1_crypto1_after_trade_c3 * price6
    
    step2_intermediate_after_trade_c3 = calculate_trade(step1_intermediate_c3, fee_dec)
    step2_intermediate2_c3 = step2_intermediate_after_trade_c3 * price7

    step3_intermediate2_after_trade_c3 = calculate_trade(step2_intermediate2_c3, fee_dec)
    results['chain3_final'] = step3_intermediate2_after_trade_c3 * price8
    
    # --- Chain 4: Crypto2 -> Crypto1 -> Base -> Crypto2 (Reverse 2) ---
    # R columns: [1,14]=p2b, [1,15]=1/p3a, [1,16]=p1b
    # Path: BTC->USDT->ADA->BTC (R comment for table_ADA_USDT, Plan4)
    price14 = p.get('price2_bid', 0)
    price15 = (1 / p['price3_ask']) if p.get('price3_ask', 0) > 0 else decimal.Decimal('0')
    price16 = p.get('price1_bid', 0)
    
    step1_crypto2_after_trade_c4 = calculate_trade(bal2, fee_dec)
    step1_intermediate_c4 = step1_crypto2_after_trade_c4 * price14
    
    step2_intermediate_after_trade_c4 = calculate_trade(step1_intermediate_c4, fee_dec)
    step2_intermediate2_c4 = step2_intermediate_after_trade_c4 * price15

    step3_intermediate2_after_trade_c4 = calculate_trade(step2_intermediate2_c4, fee_dec)
    results['chain4_final'] = step3_intermediate2_after_trade_c4 * price16
    
    return results

def find_result(final_balances, initial_balance_crypto1, initial_balance_crypto2, plan1, plan2, plan3, plan4):
    """
    Calculates the profit/loss for each of the four chains.

    Args:
        final_balances (dict): Dictionary returned by find_balance.
        initial_balance_crypto1 (Decimal): Initial amount of the first crypto.
        initial_balance_crypto2 (Decimal): Initial amount of the second crypto.
        plan1, plan2, plan3, plan4 (str): Names/descriptions for each chain.

    Returns:
        list: A list of dictionaries, each representing a result row:
              {'Crypto_Plan': str, 'Percent_of_Profit': float}
    """
    results_list = [] 
    initial_balances = {
        'chain1_final': decimal.Decimal(str(initial_balance_crypto1)),
        'chain3_final': decimal.Decimal(str(initial_balance_crypto1)),
        'chain2_final': decimal.Decimal(str(initial_balance_crypto2)),
        'chain4_final': decimal.Decimal(str(initial_balance_crypto2))
    }
    plans = {
        'chain1_final': plan1,
        'chain3_final': plan2,
        'chain2_final': plan3,
        'chain4_final': plan4
    }
    chain_keys_ordered = ['chain1_final', 'chain3_final', 'chain2_final', 'chain4_final']

    for chain_key in chain_keys_ordered:
        final = final_balances.get(chain_key, decimal.Decimal('0'))
        initial = initial_balances[chain_key]
        plan_name = plans[chain_key]
        
        if initial > 0:
            # Calculate profit ratio
            result_val = (final - initial) / initial
            
            # Calculate percentage profit
            percent_profit = result_val * decimal.Decimal('100') 
            
            results_list.append({
                'Crypto_Plan': plan_name,
                'Percent_of_Profit': round(float(percent_profit), 2) 
            })
        else:
            # Handle cases where initial balance is zero
            results_list.append({
                'Crypto_Plan': plan_name,
                'Percent_of_Profit': 0.00
            })
            
    return results_list

# Example Usage (requires price data and initial balances)
if __name__ == '__main__':
    # --- Dummy Data for Testing ---
    # Example from table_ADA_USDT
    test_prices = {
        'price1_bid': decimal.Decimal('0.00005'), # ADABTC bid
        'price1_ask': decimal.Decimal('0.0000505'),# ADABTC ask
        'price2_bid': decimal.Decimal('40000'),   # BTCUSDT bid
        'price2_ask': decimal.Decimal('40001'),   # BTCUSDT ask
        'price3_bid': decimal.Decimal('2.0'),     # ADAUSDT bid
        'price3_ask': decimal.Decimal('2.01')     # ADAUSDT ask
    }
    # Example initial balances (using standard values instead of RUB-based calculation)
    initial_ada = decimal.Decimal('1000')
    initial_btc = decimal.Decimal('1000') 
    binance_fee = decimal.Decimal('0.001')

    print("Calculating balances...")
    balances = find_balance(test_prices, initial_ada, initial_btc, binance_fee)
    print("Final Balances:", balances)

    # Plan names from R code for ADA/USDT
    p1 = "ADA->BTC->USDT->ADA"
    p2 = "ADA->USDT->BTC->ADA"
    p3 = "BTC->ADA->USDT->BTC"
    p4 = "BTC->USDT->ADA->BTC"
    
    print("\nCalculating results...")
    results = find_result(balances, initial_ada, initial_btc, p1, p2, p3, p4)
    print("Final Results:")
    for row in results:
        print(row) 