from arbitrage_calculator import find_balance, find_result
import requests

import decimal
decimal.getcontext().prec = 30
BINANCE_FEE = decimal.Decimal('0.001')

# Define the crypto pairs and their associated parameters (triangles, symbols)
CRYPTO_CONFIG = [
    {"pair1": "ADABTC",   "pair2": "BTCUSDT", "pair3": "ADAUSDT",  "crypto1": "ADA",   "crypto2": "BTC", "plans": ("ADA->BTC->USDT->ADA", "ADA->USDT->BTC->ADA", "BTC->ADA->USDT->BTC", "BTC->USDT->ADA->BTC")},
    {"pair1": "ADAETH",   "pair2": "ETHBTC",  "pair3": "ADABTC",   "crypto1": "ADA",   "crypto2": "ETH", "plans": ("ADA->ETH->BTC->ADA", "ADA->BTC->ETH->ADA", "ETH->ADA->BTC->ETH", "ETH->BTC->ADA->ETH")},
    {"pair1": "BNBBTC",   "pair2": "BTCUSDT", "pair3": "BNBUSDT",  "crypto1": "BNB",   "crypto2": "BTC", "plans": ("BNB->BTC->USDT->BNB", "BNB->USDT->BTC->BNB", "BTC->BNB->USDT->BTC", "BTC->USDT->BNB->BTC")},
    {"pair1": "BNBETH",   "pair2": "ETHBTC",  "pair3": "BNBBTC",   "crypto1": "BNB",   "crypto2": "ETH", "plans": ("BNB->ETH->BTC->BNB", "BNB->BTC->ETH->BNB", "ETH->BNB->BTC->ETH", "ETH->BTC->BNB->ETH")},
    {"pair1": "BTGETH",   "pair2": "ETHBTC",  "pair3": "BTGBTC",   "crypto1": "BTG",   "crypto2": "ETH", "plans": ("BTG->ETH->BTC->BTG", "BTG->BTC->ETH->BTG", "ETH->BTG->BTC->ETH", "ETH->BTC->BTG->ETH")},
    {"pair1": "DASHETH",  "pair2": "ETHBTC",  "pair3": "DASHBTC",  "crypto1": "DASH",  "crypto2": "ETH", "plans": ("DASH->ETH->BTC->DASH", "DASH->BTC->ETH->DASH", "ETH->DASH->BTC->ETH", "ETH->BTC->DASH->ETH")},
    {"pair1": "EOSBTC",   "pair2": "BTCUSDT", "pair3": "EOSUSDT",  "crypto1": "EOS",   "crypto2": "BTC", "plans": ("EOS->BTC->USDT->EOS", "EOS->USDT->BTC->EOS", "BTC->EOS->USDT->BTC", "BTC->USDT->EOS->BTC")},
    {"pair1": "EOSETH",   "pair2": "ETHBTC",  "pair3": "EOSBTC",   "crypto1": "EOS",   "crypto2": "ETH", "plans": ("EOS->ETH->BTC->EOS", "EOS->BTC->ETH->EOS", "ETH->EOS->BTC->ETH", "ETH->BTC->EOS->ETH")},
    {"pair1": "ETCBTC",   "pair2": "BTCUSDT", "pair3": "ETCUSDT",  "crypto1": "ETC",   "crypto2": "BTC", "plans": ("ETC->BTC->USDT->ETC", "ETC->USDT->BTC->ETC", "BTC->ETC->USDT->BTC", "BTC->USDT->ETC->BTC")},
    {"pair1": "ETCETH",   "pair2": "ETHBTC",  "pair3": "ETCBTC",   "crypto1": "ETC",   "crypto2": "ETH", "plans": ("ETC->ETH->BTC->ETC", "ETC->BTC->ETH->ETC", "ETH->ETC->BTC->ETH", "ETH->BTC->ETC->ETH")},
    {"pair1": "ETHBTC",   "pair2": "BTCUSDT", "pair3": "ETHUSDT",  "crypto1": "ETH",   "crypto2": "BTC", "plans": ("ETH->BTC->USDT->ETH", "ETH->USDT->BTC->ETH", "BTC->ETH->USDT->BTC", "BTC->USDT->ETH->BTC")},
    {"pair1": "ICXBTC",   "pair2": "BTCUSDT", "pair3": "ICXUSDT",  "crypto1": "ICX",   "crypto2": "BTC", "plans": ("ICX->BTC->USDT->ICX", "ICX->USDT->BTC->ICX", "BTC->ICX->USDT->BTC", "BTC->USDT->ICX->BTC")},
    {"pair1": "ICXETH",   "pair2": "ETHBTC",  "pair3": "ICXBTC",   "crypto1": "ICX",   "crypto2": "ETH", "plans": ("ICX->ETH->BTC->ICX", "ICX->BTC->ETH->ICX", "ETH->ICX->BTC->ETH", "ETH->BTC->ICX->ETH")},
    {"pair1": "IOTABTC",  "pair2": "BTCUSDT", "pair3": "IOTAUSDT", "crypto1": "IOTA",  "crypto2": "BTC", "plans": ("IOTA->BTC->USDT->IOTA", "IOTA->USDT->BTC->IOTA", "BTC->IOTA->USDT->BTC", "BTC->USDT->IOTA->BTC")},
    {"pair1": "IOTAETH",  "pair2": "ETHBTC",  "pair3": "IOTABTC",  "crypto1": "IOTA",  "crypto2": "ETH", "plans": ("IOTA->ETH->BTC->IOTA", "IOTA->BTC->ETH->IOTA", "ETH->IOTA->BTC->ETH", "ETH->BTC->IOTA->ETH")},
    {"pair1": "LSKETH",   "pair2": "ETHBTC",  "pair3": "LSKBTC",   "crypto1": "LSK",   "crypto2": "ETH", "plans": ("LSK->ETH->BTC->LSK", "LSK->BTC->ETH->LSK", "ETH->LSK->BTC->ETH", "ETH->BTC->LSK->ETH")},
    {"pair1": "LTCBTC",   "pair2": "BTCUSDT", "pair3": "LTCUSDT",  "crypto1": "LTC",   "crypto2": "BTC", "plans": ("LTC->BTC->USDT->LTC", "LTC->USDT->BTC->LTC", "BTC->LTC->USDT->BTC", "BTC->USDT->LTC->BTC")},
    {"pair1": "LTCETH",   "pair2": "ETHBTC",  "pair3": "LTCBTC",   "crypto1": "LTC",   "crypto2": "ETH", "plans": ("LTC->ETH->BTC->LTC", "LTC->BTC->ETH->LTC", "ETH->LTC->BTC->ETH", "ETH->BTC->LTC->ETH")},
    {"pair1": "NEOBTC",   "pair2": "BTCUSDT", "pair3": "NEOUSDT",  "crypto1": "NEO",   "crypto2": "BTC", "plans": ("NEO->BTC->USDT->NEO", "NEO->USDT->BTC->NEO", "BTC->NEO->USDT->BTC", "BTC->USDT->NEO->BTC")},
    {"pair1": "NEOETH",   "pair2": "ETHBTC",  "pair3": "NEOBTC",   "crypto1": "NEO",   "crypto2": "ETH", "plans": ("NEO->ETH->BTC->NEO", "NEO->BTC->ETH->NEO", "ETH->NEO->BTC->ETH", "ETH->BTC->NEO->ETH")},
    {"pair1": "OMGETH",   "pair2": "ETHBTC",  "pair3": "OMGBTC",   "crypto1": "OMG",   "crypto2": "ETH", "plans": ("OMG->ETH->BTC->OMG", "OMG->BTC->ETH->OMG", "ETH->OMG->BTC->ETH", "ETH->BTC->OMG->ETH")},
    {"pair1": "ONTBTC",   "pair2": "BTCUSDT", "pair3": "ONTUSDT",  "crypto1": "ONT",   "crypto2": "BTC", "plans": ("ONT->BTC->USDT->ONT", "ONT->USDT->BTC->ONT", "BTC->ONT->USDT->BTC", "BTC->USDT->ONT->BTC")},
    {"pair1": "ONTETH",   "pair2": "ETHBTC",  "pair3": "ONTBTC",   "crypto1": "ONT",   "crypto2": "ETH", "plans": ("ONT->ETH->BTC->ONT", "ONT->BTC->ETH->ONT", "ETH->ONT->BTC->ETH", "ETH->BTC->ONT->ETH")},
    {"pair1": "QTUMBTC",  "pair2": "BTCUSDT", "pair3": "QTUMUSDT", "crypto1": "QTUM",  "crypto2": "BTC", "plans": ("QTUM->BTC->USDT->QTUM", "QTUM->USDT->BTC->QTUM", "BTC->QTUM->USDT->BTC", "BTC->USDT->QTUM->BTC")},
    {"pair1": "QTUMETH",  "pair2": "ETHBTC",  "pair3": "QTUMBTC",  "crypto1": "QTUM",  "crypto2": "ETH", "plans": ("QTUM->ETH->BTC->QTUM", "QTUM->BTC->ETH->QTUM", "ETH->QTUM->BTC->ETH", "ETH->BTC->QTUM->ETH")},
    {"pair1": "SNTETH",   "pair2": "ETHBTC",  "pair3": "SNTBTC",   "crypto1": "SNT",   "crypto2": "ETH", "plans": ("SNT->ETH->BTC->SNT", "SNT->BTC->ETH->SNT", "ETH->SNT->BTC->ETH", "ETH->BTC->SNT->ETH")},
    {"pair1": "TRXBTC",   "pair2": "BTCUSDT", "pair3": "TRXUSDT",  "crypto1": "TRX",   "crypto2": "BTC", "plans": ("TRX->BTC->USDT->TRX", "TRX->USDT->BTC->TRX", "BTC->TRX->USDT->BTC", "BTC->USDT->TRX->BTC")},
    {"pair1": "TRXETH",   "pair2": "ETHBTC",  "pair3": "TRXBTC",   "crypto1": "TRX",   "crypto2": "ETH", "plans": ("TRX->ETH->BTC->TRX", "TRX->BTC->ETH->TRX", "ETH->TRX->BTC->ETH", "ETH->BTC->TRX->ETH")},
    {"pair1": "VETBTC",   "pair2": "BTCUSDT", "pair3": "VETUSDT",  "crypto1": "VET",   "crypto2": "BTC", "plans": ("VET->BTC->USDT->VET", "VET->USDT->BTC->VET", "BTC->VET->USDT->BTC", "BTC->USDT->VET->BTC")},
    {"pair1": "VETETH",   "pair2": "ETHBTC",  "pair3": "VETBTC",   "crypto1": "VET",   "crypto2": "ETH", "plans": ("VET->ETH->BTC->VET", "VET->BTC->ETH->VET", "ETH->VET->BTC->ETH", "ETH->BTC->VET->ETH")},
    {"pair1": "WAVESETH", "pair2": "ETHBTC",  "pair3": "WAVESBTC", "crypto1": "WAVES", "crypto2": "ETH", "plans": ("WAVES->ETH->BTC->WAVES", "WAVES->BTC->ETH->WAVES", "ETH->WAVES->BTC->ETH", "ETH->BTC->WAVES->ETH")},
    {"pair1": "XEMETH",   "pair2": "ETHBTC",  "pair3": "XEMBTC",   "crypto1": "XEM",   "crypto2": "ETH", "plans": ("XEM->ETH->BTC->XEM", "XEM->BTC->ETH->XEM", "ETH->XEM->BTC->ETH", "ETH->BTC->XEM->ETH")},
    {"pair1": "XLMBTC",   "pair2": "BTCUSDT", "pair3": "XLMUSDT",  "crypto1": "XLM",   "crypto2": "BTC", "plans": ("XLM->BTC->USDT->XLM", "XLM->USDT->BTC->XLM", "BTC->XLM->USDT->BTC", "BTC->USDT->XLM->BTC")},
    {"pair1": "XLMETH",   "pair2": "ETHBTC",  "pair3": "XLMBTC",   "crypto1": "XLM",   "crypto2": "ETH", "plans": ("XLM->ETH->BTC->XLM", "XLM->BTC->ETH->XLM", "ETH->XLM->BTC->ETH", "ETH->BTC->XLM->ETH")},
    {"pair1": "XMRETH",   "pair2": "ETHBTC",  "pair3": "XMRBTC",   "crypto1": "XMR",   "crypto2": "ETH", "plans": ("XMR->ETH->BTC->XMR", "XMR->BTC->ETH->XMR", "ETH->XMR->BTC->ETH", "ETH->BTC->XMR->ETH")},
    {"pair1": "XRPBTC",   "pair2": "BTCUSDT", "pair3": "XRPUSDT",  "crypto1": "XRP",   "crypto2": "BTC", "plans": ("XRP->BTC->USDT->XRP", "XRP->USDT->BTC->XRP", "BTC->XRP->USDT->BTC", "BTC->USDT->XRP->BTC")},
    {"pair1": "XRPETH",   "pair2": "ETHBTC",  "pair3": "XRPBTC",   "crypto1": "XRP",   "crypto2": "ETH", "plans": ("XRP->ETH->BTC->XRP", "XRP->BTC->ETH->XRP", "ETH->XRP->BTC->ETH", "ETH->BTC->XRP->ETH")},
    {"pair1": "XVGETH",   "pair2": "ETHBTC",  "pair3": "XVGBTC",   "crypto1": "XVG",   "crypto2": "ETH", "plans": ("XVG->ETH->BTC->XVG", "XVG->BTC->ETH->XVG", "ETH->XVG->BTC->ETH", "ETH->BTC->XVG->ETH")},
    {"pair1": "ZECETH",   "pair2": "ETHBTC",  "pair3": "ZECBTC",   "crypto1": "ZEC",   "crypto2": "ETH", "plans": ("ZEC->ETH->BTC->ZEC", "ZEC->BTC->ETH->ZEC", "ETH->ZEC->BTC->ETH", "ETH->BTC->ZEC->ETH")},
    {"pair1": "ZRXETH",   "pair2": "ETHBTC",  "pair3": "ZRXBTC",   "crypto1": "ZRX",   "crypto2": "ETH", "plans": ("ZRX->ETH->BTC->ZRX", "ZRX->BTC->ETH->ZRX", "ETH->ZRX->BTC->ETH", "ETH->BTC->ZRX->ETH")}
]

def get_binance_market_data():
    """Fetches 24hr ticker price change statistics from Binance."""
    url = "https://api.binance.com/api/v1/ticker/24hr"
    try:
        response = requests.get(url)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        print(f"Error fetching market data from Binance: {e}")
        return None

def get_prices_for_pair(pair_config, data_market_price_map, price_mode="mark"):
    """Extracts bid/ask or last prices for the three pairs in a config."""
    p1_sym, p2_sym, p3_sym = pair_config['pair1'], pair_config['pair2'], pair_config['pair3']
    
    prices = {
        'price1_bid': None, 'price1_ask': None,
        'price2_bid': None, 'price2_ask': None,
        'price3_bid': None, 'price3_ask': None,
    }

    ticker1 = data_market_price_map.get(p1_sym)
    ticker2 = data_market_price_map.get(p2_sym)
    ticker3 = data_market_price_map.get(p3_sym)

    if price_mode == "lim": # Use bid/ask for limit orders
        if ticker1: 
            prices['price1_bid'] = ticker1.get('bidPrice')
            prices['price1_ask'] = ticker1.get('askPrice')
        if ticker2:
            prices['price2_bid'] = ticker2.get('bidPrice')
            prices['price2_ask'] = ticker2.get('askPrice')
        if ticker3:
            prices['price3_bid'] = ticker3.get('bidPrice')
            prices['price3_ask'] = ticker3.get('askPrice')
    else: # Use lastPrice for market orders
        if ticker1: 
            prices['price1_bid'] = ticker1.get('lastPrice')
            prices['price1_ask'] = ticker1.get('lastPrice')
        if ticker2:
            prices['price2_bid'] = ticker2.get('lastPrice')
            prices['price2_ask'] = ticker2.get('lastPrice')
        if ticker3:
            prices['price3_bid'] = ticker3.get('lastPrice')
            prices['price3_ask'] = ticker3.get('lastPrice')
            
    # Basic validation: check if any essential price is missing or zero
    required_keys = ['price1_bid', 'price1_ask', 'price2_bid', 'price2_ask', 'price3_bid', 'price3_ask']
    for key in required_keys:
        try:
            # Convert to Decimal and check if > 0
            if prices[key] is None or decimal.Decimal(str(prices[key])) <= 0:
                # print(f"Warning: Missing or invalid price for {key} in pair config: {pair_config['pair1']}/{pair_config['pair2']}/{pair_config['pair3']}. Setting to 0.")
                prices[key] = '0' # Set to string '0' for consistency, Decimal conversion happens later
        except (decimal.InvalidOperation, TypeError):
             # print(f"Warning: Invalid price format for {key} ('{prices[key]}') in pair config: {pair_config['pair1']}/{pair_config['pair2']}/{pair_config['pair3']}. Setting to 0.")
             prices[key] = '0'

    return prices

def process_binance_data(price_mode, data_market_price, initial_balances=None):
    """
    Processes the fetched Binance market data to calculate arbitrage opportunities.

    Args:
        price_mode (str): 'lim' for limit prices (bid/ask), 'mark' for market (last price).
        data_market_price (list): List of ticker data from Binance API.
        initial_balances (dict): Optional dictionary of initial crypto balances.
                                If None, will use a standard value of 1000 for each crypto.

    Returns:
        list: A list of dictionaries, each containing the results for a crypto plan.
              Returns an empty list if input data is invalid.
    """
    all_results = []
    
    if not data_market_price or not isinstance(data_market_price, list):
        print("Error: Invalid or empty Binance market data provided.")
        return all_results
        
    # If no initial balances provided, create a default dictionary with 1000 for each crypto
    if not initial_balances or not isinstance(initial_balances, dict):
        print("Using default standard balance of 1000 for each cryptocurrency")
        initial_balances = {}
        # Extract all unique crypto symbols from the configuration
        all_cryptos = set()
        for config in CRYPTO_CONFIG:
            all_cryptos.add(config['crypto1'])
            all_cryptos.add(config['crypto2'])
        
        # Set a standard amount of 1000 for each cryptocurrency
        initial_balances = {crypto: decimal.Decimal('1000') for crypto in all_cryptos}

    # Create a dictionary for faster lookups by symbol
    data_market_price_map = {ticker['symbol']: ticker for ticker in data_market_price}

    for config in CRYPTO_CONFIG:
        crypto1_sym = config['crypto1']
        crypto2_sym = config['crypto2']
        
        # Get initial balances, default to 1000 if not found
        balance1 = initial_balances.get(crypto1_sym, decimal.Decimal('1000'))
        balance2 = initial_balances.get(crypto2_sym, decimal.Decimal('1000'))

        # Extract prices for the current triplet
        prices = get_prices_for_pair(config, data_market_price_map, price_mode)

        # Check if all necessary prices were found and are valid (> 0)
        if any(decimal.Decimal(str(prices.get(k, '0'))) <= 0 for k in ['price1_bid', 'price1_ask', 'price2_bid', 'price2_ask', 'price3_bid', 'price3_ask']):
            # print(f"Skipping {config['pair1']} due to missing or zero prices: {prices}")
            continue # Skip this triplet if any price is missing/zero

        # Calculate final balances for the 4 chains
        final_balances = find_balance(prices, balance1, balance2, BINANCE_FEE)

        # Calculate profit/loss results
        plan1, plan2, plan3, plan4 = config['plans']
        results = find_result(
            final_balances, 
            balance1, 
            balance2, 
            plan1, plan2, plan3, plan4
        )
        
        all_results.extend(results)

    # Sort results by percentage profit (descending)
    all_results.sort(key=lambda x: x['Percent_of_Profit'], reverse=True)
    
    return all_results

if __name__ == '__main__':
    # Fetch market data
    binance_market_data = get_binance_market_data()
    print(f"Fetched {len(binance_market_data)} tickers.")

    # Example usage of process_binance_data
    price_mode = "mark"  # or "lim" for market prices
    initial_balances = None  # or a dictionary of initial balances
    results = process_binance_data(price_mode, binance_market_data, initial_balances)
    for result in results:
        if result['Percent_of_Profit'] > 0:
            print(result)