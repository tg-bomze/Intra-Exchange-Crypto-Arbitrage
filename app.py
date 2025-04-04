from flask import Flask, render_template, redirect, url_for, flash, session
import datetime
import os
import binance_logic

from dotenv import load_dotenv
load_dotenv()

app = Flask(__name__)
app.secret_key = os.urandom(24)

# --- Global Data Initialization ---
LAST_UPDATE_TIME = "Never"
EXCHANGE_NAME = "None"
LIM_RESULTS = []
MARK_RESULTS = []

@app.route('/')
def index():
    """Renders the main page."""
    # Use session to retrieve results if they exist, otherwise use global defaults (or fetch fresh)
    lim_results = session.get('lim_results', [])
    mark_results = session.get('mark_results', [])
    exchange_name = session.get('exchange_name', EXCHANGE_NAME)
    last_update = session.get('last_update', LAST_UPDATE_TIME)

    return render_template('index.html', 
                           lim_results=lim_results,
                           mark_results=mark_results,
                           exchange_name=exchange_name,
                           last_update=last_update)

@app.route('/update_binance', methods=['POST'])
def update_binance():
    """Handles the request to update data from Binance."""
    global LAST_UPDATE_TIME, EXCHANGE_NAME, LIM_RESULTS, MARK_RESULTS
    
    print("Updating Binance data...")
    flash("Fetching Binance data... Please wait.", "info")

    # Fetch fresh market data from Binance
    data_market_price = binance_logic.get_binance_market_data()

    if not data_market_price:
        flash("Error: Failed to fetch market data from Binance.", "error")
        LIM_RESULTS = []
        MARK_RESULTS = []
    else:
        flash(f"Successfully fetched {len(data_market_price)} tickers from Binance. Processing...", "info")
        print("Processing limit prices...")
        lim_res = binance_logic.process_binance_data("lim", data_market_price)
        print("Processing market prices...")
        mark_res = binance_logic.process_binance_data("mark", data_market_price)

        LIM_RESULTS = lim_res
        MARK_RESULTS = mark_res
        LAST_UPDATE_TIME = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        EXCHANGE_NAME = "Binance"
        flash("Calculations complete.", "success")

        # Store results in session
        session['lim_results'] = LIM_RESULTS
        session['mark_results'] = MARK_RESULTS
        session['exchange_name'] = EXCHANGE_NAME
        session['last_update'] = LAST_UPDATE_TIME
        
    return redirect(url_for('index'))

if __name__ == '__main__':
    print("Starting Flask app...")
    app.run(debug=True, port=5001)