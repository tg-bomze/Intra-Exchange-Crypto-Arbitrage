# устанавливаем пакет для получения данных через web api
#install.packages('httr', repos = c('https://xran.yihui.name', 'https://cran.r-project.org'))
# устанавливаем пакеты для работы с shiny
#install.packages('shiny', repos = c('https://xran.yihui.name', 'https://cran.r-project.org'))
#install.packages('rsconnect', repos = c('https://xran.yihui.name', 'https://cran.r-project.org'))

# подключаем библиотеки
library(httr)
library(shiny)
library(rsconnect)

source("main_func.R")
source("binance.R")
source("bitfinex.R")
source("bittrex.R")
source("poloniex.R")
source("hitbtc.R")
source("gate.R")

# авторизация
#rsconnect::setAccountInfo(name='ieca', token='', secret='')

ui <- fluidPage(
  #titlePanel(h1("Intra-Exchange Crypto-Arbitrage", align="center")),
  br(),
  
  sidebarPanel(tags$img(src='logo.png',width="100%"),
               br(),
               h5("IECA (Intra-Exchange Crypto-Arbitrage) is tool for manual monitoring of arbitration situations on the intra-exchange crypto-currency market."),
               h6("*Tab 'Limit Price' uses BID and ASK, and tab 'Market Price' uses last actual price"),
               h3("Control Panel", align="center"),
               h4("Choose Exchange (update):"),
               actionButton("binance_act","Binance"),
               actionButton("bitfinex_act","Bitfinex"),
               br(),br(),
               actionButton("bittrex_act","Bittrex"),
               actionButton("poloniex_act","Poloniex"),
               br(),br(),
               actionButton("hitbtc_act","HitBTC"),
               actionButton("gate_act","Gate"),
               br(),br(),br(),
               verbatimTextOutput("title_name"),
               verbatimTextOutput("last_update")
  ),

  mainPanel(tabsetPanel(
              tabPanel("Market Price", tableOutput("mark_tab")),
              tabPanel("Limit Price", tableOutput("lim_tab"))))
)


server <- function(input, output) {
  name_exch <- "Now: "
  output$title_name <- renderPrint({name_exch})
  ls_update <- "Last update: "
  output$last_update <- renderPrint({ls_update})
  
  # ----------создаем таблицы для расчетов ----------
  table_ADA_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ADA_BTC=0, BTC_USDT=0, USDT_ADA=0, I=c("|"), ADA_USDT=0, USDT_BTC=0, BTC_ADA=0, I=c("|"), BTC_ADA=0, ADA_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ADA=0, ADA_BTC=0)
  table_ADA_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ADA_ETH=0, ETH_BTC=0, BTC_ADA=0, I=c("|"), ADA_BTC=0, BTC_ETH=0, ETH_ADA=0, I=c("|"), ETH_ADA=0, ADA_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ADA=0, ADA_ETH=0)
  table_AE_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), AE_BTC=0, BTC_USDT=0, USDT_AE=0, I=c("|"), AE_USDT=0, USDT_BTC=0, BTC_AE=0, I=c("|"), BTC_AE=0, AE_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_AE=0, AE_BTC=0)
  table_AE_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), AE_ETH=0, ETH_BTC=0, BTC_AE=0, I=c("|"), AE_BTC=0, BTC_ETH=0, ETH_AE=0, I=c("|"), ETH_AE=0, AE_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_AE=0, AE_ETH=0)
  table_BCH_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), BCH_BTC=0, BTC_USDT=0, USDT_BCH=0, I=c("|"), BCH_USDT=0, USDT_BTC=0, BTC_BCH=0, I=c("|"), BTC_BCH=0, BCH_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_BCH=0, BCH_BTC=0)
  table_BCH_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), BCH_ETH=0, ETH_BTC=0, BTC_BCH=0, I=c("|"), BCH_BTC=0, BTC_ETH=0, ETH_BCH=0, I=c("|"), ETH_BCH=0, BCH_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_BCH=0, BCH_ETH=0)
  table_BCN_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), BCN_BTC=0, BTC_USDT=0, USDT_BCN=0, I=c("|"), BCN_USDT=0, USDT_BTC=0, BTC_BCN=0, I=c("|"), BTC_BCN=0, BCN_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_BCN=0, BCN_BTC=0)
  table_BCN_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), BCN_ETH=0, ETH_BTC=0, BTC_BCN=0, I=c("|"), BCN_BTC=0, BTC_ETH=0, ETH_BCN=0, I=c("|"), ETH_BCN=0, BCN_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_BCN=0, BCN_ETH=0)
  table_BNB_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), BNB_BTC=0, BTC_USDT=0, USDT_BNB=0, I=c("|"), BNB_USDT=0, USDT_BTC=0, BTC_BNB=0, I=c("|"), BTC_BNB=0, BNB_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_BNB=0, BNB_BTC=0)
  table_BNB_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), BNB_ETH=0, ETH_BTC=0, BTC_BNB=0, I=c("|"), BNB_BTC=0, BTC_ETH=0, ETH_BNB=0, I=c("|"), ETH_BNB=0, BNB_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_BNB=0, BNB_ETH=0)
  table_BTG_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), BTG_BTC=0, BTC_USDT=0, USDT_BTG=0, I=c("|"), BTG_USDT=0, USDT_BTC=0, BTC_BTG=0, I=c("|"), BTC_BTG=0, BTG_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_BTG=0, BTG_BTC=0)
  table_BTG_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), BTG_ETH=0, ETH_BTC=0, BTC_BTG=0, I=c("|"), BTG_BTC=0, BTC_ETH=0, ETH_BTG=0, I=c("|"), ETH_BTG=0, BTG_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_BTG=0, BTG_ETH=0)
  table_BTS_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), BTS_BTC=0, BTC_USDT=0, USDT_BTS=0, I=c("|"), BTS_USDT=0, USDT_BTC=0, BTC_BTS=0, I=c("|"), BTC_BTS=0, BTS_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_BTS=0, BTS_BTC=0)
  table_DASH_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), DASH_BTC=0, BTC_USDT=0, USDT_DASH=0, I=c("|"), DASH_USDT=0, USDT_BTC=0, BTC_DASH=0, I=c("|"), BTC_DASH=0, DASH_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_DASH=0, DASH_BTC=0)
  table_DASH_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), DASH_ETH=0, ETH_BTC=0, BTC_DASH=0, I=c("|"), DASH_BTC=0, BTC_ETH=0, ETH_DASH=0, I=c("|"), ETH_DASH=0, DASH_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_DASH=0, DASH_ETH=0)
  table_DCR_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), DCR_BTC=0, BTC_USDT=0, USDT_DCR=0, I=c("|"), DCR_USDT=0, USDT_BTC=0, BTC_DCR=0, I=c("|"), BTC_DCR=0, DCR_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_DCR=0, DCR_BTC=0)
  table_DOGE_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), DOGE_BTC=0, BTC_USDT=0, USDT_DOGE=0, I=c("|"), DOGE_USDT=0, USDT_BTC=0, BTC_DOGE=0, I=c("|"), BTC_DOGE=0, DOGE_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_DOGE=0, DOGE_BTC=0)
  table_DOGE_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), DOGE_ETH=0, ETH_BTC=0, BTC_DOGE=0, I=c("|"), DOGE_BTC=0, BTC_ETH=0, ETH_DOGE=0, I=c("|"), ETH_DOGE=0, DOGE_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_DOGE=0, DOGE_ETH=0)
  table_DGB_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), DGB_BTC=0, BTC_USDT=0, USDT_DGB=0, I=c("|"), DGB_USDT=0, USDT_BTC=0, BTC_DGB=0, I=c("|"), BTC_DGB=0, DGB_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_DGB=0, DGB_BTC=0)
  table_DGB_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), DGB_ETH=0, ETH_BTC=0, BTC_DGB=0, I=c("|"), DGB_BTC=0, BTC_ETH=0, ETH_DGB=0, I=c("|"), ETH_DGB=0, DGB_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_DGB=0, DGB_ETH=0)
  table_EOS_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), EOS_BTC=0, BTC_USDT=0, USDT_EOS=0, I=c("|"), EOS_USDT=0, USDT_BTC=0, BTC_EOS=0, I=c("|"), BTC_EOS=0, EOS_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_EOS=0, EOS_BTC=0)
  table_EOS_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), EOS_ETH=0, ETH_BTC=0, BTC_EOS=0, I=c("|"), EOS_BTC=0, BTC_ETH=0, ETH_EOS=0, I=c("|"), ETH_EOS=0, EOS_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_EOS=0, EOS_ETH=0)
  table_EOS_EUR <<- data.frame(Name=c("Price", "No Commission", "Balance"), EOS_BTC=0, BTC_EUR=0, EUR_EOS=0, I=c("|"), EOS_EUR=0, EUR_BTC=0, BTC_EOS=0, I=c("|"), BTC_EOS=0, EOS_EUR=0, EUR_BTC=0, I=c("|"), BTC_EUR=0, EUR_EOS=0, EOS_BTC=0)
  table_ETC_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ETC_BTC=0, BTC_USDT=0, USDT_ETC=0, I=c("|"), ETC_USDT=0, USDT_BTC=0, BTC_ETC=0, I=c("|"), BTC_ETC=0, ETC_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ETC=0, ETC_BTC=0)
  table_ETC_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ETC_ETH=0, ETH_BTC=0, BTC_ETC=0, I=c("|"), ETC_BTC=0, BTC_ETH=0, ETH_ETC=0, I=c("|"), ETH_ETC=0, ETC_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ETC=0, ETC_ETH=0)
  table_ETH_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ETH_BTC=0, BTC_USDT=0, USDT_ETH=0, I=c("|"), ETH_USDT=0, USDT_BTC=0, BTC_ETH=0, I=c("|"), BTC_ETH=0, ETH_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ETH=0, ETH_BTC=0)
  table_ETH_EUR <<- data.frame(Name=c("Price", "No Commission", "Balance"), ETH_BTC=0, BTC_EUR=0, EUR_ETH=0, I=c("|"), ETH_EUR=0, EUR_BTC=0, BTC_ETH=0, I=c("|"), BTC_ETH=0, ETH_EUR=0, EUR_BTC=0, I=c("|"), BTC_EUR=0, EUR_ETH=0, ETH_BTC=0)
  table_GNT_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), GNT_ETH=0, ETH_BTC=0, BTC_GNT=0, I=c("|"), GNT_BTC=0, BTC_ETH=0, ETH_GNT=0, I=c("|"), ETH_GNT=0, GNT_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_GNT=0, GNT_ETH=0)
  table_HSR_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), HSR_BTC=0, BTC_USDT=0, USDT_HSR=0, I=c("|"), HSR_USDT=0, USDT_BTC=0, BTC_HSR=0, I=c("|"), BTC_HSR=0, HSR_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_HSR=0, HSR_BTC=0)
  table_ICX_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ICX_BTC=0, BTC_USDT=0, USDT_ICX=0, I=c("|"), ICX_USDT=0, USDT_BTC=0, BTC_ICX=0, I=c("|"), BTC_ICX=0, ICX_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ICX=0, ICX_BTC=0)
  table_ICX_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ICX_ETH=0, ETH_BTC=0, BTC_ICX=0, I=c("|"), ICX_BTC=0, BTC_ETH=0, ETH_ICX=0, I=c("|"), ETH_ICX=0, ICX_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ICX=0, ICX_ETH=0)
  table_IOTA_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), IOTA_BTC=0, BTC_USDT=0, USDT_IOTA=0, I=c("|"), IOTA_USDT=0, USDT_BTC=0, BTC_IOTA=0, I=c("|"), BTC_IOTA=0, IOTA_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_IOTA=0, IOTA_BTC=0)
  table_IOTA_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), IOTA_ETH=0, ETH_BTC=0, BTC_IOTA=0, I=c("|"), IOTA_BTC=0, BTC_ETH=0, ETH_IOTA=0, I=c("|"), ETH_IOTA=0, IOTA_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_IOTA=0, IOTA_ETH=0)
  table_LSK_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), LSK_BTC=0, BTC_USDT=0, USDT_LSK=0, I=c("|"), LSK_USDT=0, USDT_BTC=0, BTC_LSK=0, I=c("|"), BTC_LSK=0, LSK_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_LSK=0, LSK_BTC=0)
  table_LSK_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), LSK_ETH=0, ETH_BTC=0, BTC_LSK=0, I=c("|"), LSK_BTC=0, BTC_ETH=0, ETH_LSK=0, I=c("|"), ETH_LSK=0, LSK_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_LSK=0, LSK_ETH=0)
  table_LTC_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), LTC_BTC=0, BTC_USDT=0, USDT_LTC=0, I=c("|"), LTC_USDT=0, USDT_BTC=0, BTC_LTC=0, I=c("|"), BTC_LTC=0, LTC_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_LTC=0, LTC_BTC=0)
  table_LTC_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), LTC_ETH=0, ETH_BTC=0, BTC_LTC=0, I=c("|"), LTC_BTC=0, BTC_ETH=0, ETH_LTC=0, I=c("|"), ETH_LTC=0, LTC_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_LTC=0, LTC_ETH=0)
  table_MIOTA_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), MIOTA_BTC=0, BTC_USDT=0, USDT_MIOTA=0, I=c("|"), MIOTA_USDT=0, USDT_BTC=0, BTC_MIOTA=0, I=c("|"), BTC_MIOTA=0, MIOTA_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_MIOTA=0, MIOTA_BTC=0)
  table_MIOTA_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), MIOTA_ETH=0, ETH_BTC=0, BTC_MIOTA=0, I=c("|"), MIOTA_BTC=0, BTC_ETH=0, ETH_MIOTA=0, I=c("|"), ETH_MIOTA=0, MIOTA_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_MIOTA=0, MIOTA_ETH=0)
  table_MIOTA_EUR <<- data.frame(Name=c("Price", "No Commission", "Balance"), MIOTA_BTC=0, BTC_EUR=0, EUR_MIOTA=0, I=c("|"), MIOTA_EUR=0, EUR_BTC=0, BTC_MIOTA=0, I=c("|"), BTC_MIOTA=0, MIOTA_EUR=0, EUR_BTC=0, I=c("|"), BTC_EUR=0, EUR_MIOTA=0, MIOTA_BTC=0)
  table_NANO_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), NANO_ETH=0, ETH_BTC=0, BTC_NANO=0, I=c("|"), NANO_BTC=0, BTC_ETH=0, ETH_NANO=0, I=c("|"), ETH_NANO=0, NANO_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_NANO=0, NANO_ETH=0)
  table_NEO_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), NEO_BTC=0, BTC_USDT=0, USDT_NEO=0, I=c("|"), NEO_USDT=0, USDT_BTC=0, BTC_NEO=0, I=c("|"), BTC_NEO=0, NEO_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_NEO=0, NEO_BTC=0)
  table_NEO_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), NEO_ETH=0, ETH_BTC=0, BTC_NEO=0, I=c("|"), NEO_BTC=0, BTC_ETH=0, ETH_NEO=0, I=c("|"), ETH_NEO=0, NEO_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_NEO=0, NEO_ETH=0)
  table_NEO_EUR <<- data.frame(Name=c("Price", "No Commission", "Balance"), NEO_BTC=0, BTC_EUR=0, EUR_NEO=0, I=c("|"), NEO_EUR=0, EUR_BTC=0, BTC_NEO=0, I=c("|"), BTC_NEO=0, NEO_EUR=0, EUR_BTC=0, I=c("|"), BTC_EUR=0, EUR_NEO=0, NEO_BTC=0)
  table_NXT_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), NXT_BTC=0, BTC_USDT=0, USDT_NXT=0, I=c("|"), NXT_USDT=0, USDT_BTC=0, BTC_NXT=0, I=c("|"), BTC_NXT=0, NXT_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_NXT=0, NXT_BTC=0)
  table_NXT_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), NXT_ETH=0, ETH_BTC=0, BTC_NXT=0, I=c("|"), NXT_BTC=0, BTC_ETH=0, ETH_NXT=0, I=c("|"), ETH_NXT=0, NXT_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_NXT=0, NXT_ETH=0)
  table_OMG_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), OMG_BTC=0, BTC_USDT=0, USDT_OMG=0, I=c("|"), OMG_USDT=0, USDT_BTC=0, BTC_OMG=0, I=c("|"), BTC_OMG=0, OMG_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_OMG=0, OMG_BTC=0)
  table_OMG_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), OMG_ETH=0, ETH_BTC=0, BTC_OMG=0, I=c("|"), OMG_BTC=0, BTC_ETH=0, ETH_OMG=0, I=c("|"), ETH_OMG=0, OMG_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_OMG=0, OMG_ETH=0)
  table_ONT_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ONT_BTC=0, BTC_USDT=0, USDT_ONT=0, I=c("|"), ONT_USDT=0, USDT_BTC=0, BTC_ONT=0, I=c("|"), BTC_ONT=0, ONT_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ONT=0, ONT_BTC=0)
  table_ONT_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ONT_ETH=0, ETH_BTC=0, BTC_ONT=0, I=c("|"), ONT_BTC=0, BTC_ETH=0, ETH_ONT=0, I=c("|"), ETH_ONT=0, ONT_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ONT=0, ONT_ETH=0)
  table_POWR_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), POWR_ETH=0, ETH_BTC=0, BTC_POWR=0, I=c("|"), POWR_BTC=0, BTC_ETH=0, ETH_POWR=0, I=c("|"), ETH_POWR=0, POWR_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_POWR=0, POWR_ETH=0)
  table_QTUM_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), QTUM_BTC=0, BTC_USDT=0, USDT_QTUM=0, I=c("|"), QTUM_USDT=0, USDT_BTC=0, BTC_QTUM=0, I=c("|"), BTC_QTUM=0, QTUM_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_QTUM=0, QTUM_BTC=0)
  table_QTUM_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), QTUM_ETH=0, ETH_BTC=0, BTC_QTUM=0, I=c("|"), QTUM_BTC=0, BTC_ETH=0, ETH_QTUM=0, I=c("|"), ETH_QTUM=0, QTUM_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_QTUM=0, QTUM_ETH=0)
  table_REP_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), REP_BTC=0, BTC_USDT=0, USDT_REP=0, I=c("|"), REP_USDT=0, USDT_BTC=0, BTC_REP=0, I=c("|"), BTC_REP=0, REP_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_REP=0, REP_BTC=0)
  table_REP_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), REP_ETH=0, ETH_BTC=0, BTC_REP=0, I=c("|"), REP_BTC=0, BTC_ETH=0, ETH_REP=0, I=c("|"), ETH_REP=0, REP_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_REP=0, REP_ETH=0)
  table_SC_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), SC_BTC=0, BTC_USDT=0, USDT_SC=0, I=c("|"), SC_USDT=0, USDT_BTC=0, BTC_SC=0, I=c("|"), BTC_SC=0, SC_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_SC=0, SC_BTC=0)
  table_SC_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), SC_ETH=0, ETH_BTC=0, BTC_SC=0, I=c("|"), SC_BTC=0, BTC_ETH=0, ETH_SC=0, I=c("|"), ETH_SC=0, SC_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_SC=0, SC_ETH=0)
  table_SNT_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), SNT_BTC=0, BTC_USDT=0, USDT_SNT=0, I=c("|"), SNT_USDT=0, USDT_BTC=0, BTC_SNT=0, I=c("|"), BTC_SNT=0, SNT_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_SNT=0, SNT_BTC=0)
  table_SNT_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), SNT_ETH=0, ETH_BTC=0, BTC_SNT=0, I=c("|"), SNT_BTC=0, BTC_ETH=0, ETH_SNT=0, I=c("|"), ETH_SNT=0, SNT_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_SNT=0, SNT_ETH=0)
  table_STR_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), STR_BTC=0, BTC_USDT=0, USDT_STR=0, I=c("|"), STR_USDT=0, USDT_BTC=0, BTC_STR=0, I=c("|"), BTC_STR=0, STR_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_STR=0, STR_BTC=0)
  table_STRAT_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), STRAT_ETH=0, ETH_BTC=0, BTC_STRAT=0, I=c("|"), STRAT_BTC=0, BTC_ETH=0, ETH_STRAT=0, I=c("|"), ETH_STRAT=0, STRAT_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_STRAT=0, STRAT_ETH=0)
  table_TRX_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), TRX_BTC=0, BTC_USDT=0, USDT_TRX=0, I=c("|"), TRX_USDT=0, USDT_BTC=0, BTC_TRX=0, I=c("|"), BTC_TRX=0, TRX_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_TRX=0, TRX_BTC=0)
  table_TRX_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), TRX_ETH=0, ETH_BTC=0, BTC_TRX=0, I=c("|"), TRX_BTC=0, BTC_ETH=0, ETH_TRX=0, I=c("|"), ETH_TRX=0, TRX_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_TRX=0, TRX_ETH=0)
  table_VET_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), VET_BTC=0, BTC_USDT=0, USDT_VET=0, I=c("|"), VET_USDT=0, USDT_BTC=0, BTC_VET=0, I=c("|"), BTC_VET=0, VET_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_VET=0, VET_BTC=0)
  table_VET_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), VET_ETH=0, ETH_BTC=0, BTC_VET=0, I=c("|"), VET_BTC=0, BTC_ETH=0, ETH_VET=0, I=c("|"), ETH_VET=0, VET_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_VET=0, VET_ETH=0)
  table_WAVES_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), WAVES_ETH=0, ETH_BTC=0, BTC_WAVES=0, I=c("|"), WAVES_BTC=0, BTC_ETH=0, ETH_WAVES=0, I=c("|"), ETH_WAVES=0, WAVES_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_WAVES=0, WAVES_ETH=0)
  table_WAX_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), WAX_ETH=0, ETH_BTC=0, BTC_WAX=0, I=c("|"), WAX_BTC=0, BTC_ETH=0, ETH_WAX=0, I=c("|"), ETH_WAX=0, WAX_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_WAX=0, WAX_ETH=0)
  table_XEM_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), XEM_BTC=0, BTC_USDT=0, USDT_XEM=0, I=c("|"), XEM_USDT=0, USDT_BTC=0, BTC_XEM=0, I=c("|"), BTC_XEM=0, XEM_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_XEM=0, XEM_BTC=0)
  table_XEM_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), XEM_ETH=0, ETH_BTC=0, BTC_XEM=0, I=c("|"), XEM_BTC=0, BTC_ETH=0, ETH_XEM=0, I=c("|"), ETH_XEM=0, XEM_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_XEM=0, XEM_ETH=0)
  table_XLM_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), XLM_BTC=0, BTC_USDT=0, USDT_XLM=0, I=c("|"), XLM_USDT=0, USDT_BTC=0, BTC_XLM=0, I=c("|"), BTC_XLM=0, XLM_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_XLM=0, XLM_BTC=0)
  table_XLM_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), XLM_ETH=0, ETH_BTC=0, BTC_XLM=0, I=c("|"), XLM_BTC=0, BTC_ETH=0, ETH_XLM=0, I=c("|"), ETH_XLM=0, XLM_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_XLM=0, XLM_ETH=0)
  table_XLM_EUR <<- data.frame(Name=c("Price", "No Commission", "Balance"), XLM_BTC=0, BTC_EUR=0, EUR_XLM=0, I=c("|"), XLM_EUR=0, EUR_BTC=0, BTC_XLM=0, I=c("|"), BTC_XLM=0, XLM_EUR=0, EUR_BTC=0, I=c("|"), BTC_EUR=0, EUR_XLM=0, XLM_BTC=0)
  table_XMR_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), XMR_BTC=0, BTC_USDT=0, USDT_XMR=0, I=c("|"), XMR_USDT=0, USDT_BTC=0, BTC_XMR=0, I=c("|"), BTC_XMR=0, XMR_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_XMR=0, XMR_BTC=0)
  table_XMR_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), XMR_ETH=0, ETH_BTC=0, BTC_XMR=0, I=c("|"), XMR_BTC=0, BTC_ETH=0, ETH_XMR=0, I=c("|"), ETH_XMR=0, XMR_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_XMR=0, XMR_ETH=0)
  table_XRP_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), XRP_BTC=0, BTC_USDT=0, USDT_XRP=0, I=c("|"), XRP_USDT=0, USDT_BTC=0, BTC_XRP=0, I=c("|"), BTC_XRP=0, XRP_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_XRP=0, XRP_BTC=0)
  table_XRP_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), XRP_ETH=0, ETH_BTC=0, BTC_XRP=0, I=c("|"), XRP_BTC=0, BTC_ETH=0, ETH_XRP=0, I=c("|"), ETH_XRP=0, XRP_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_XRP=0, XRP_ETH=0)
  table_XVG_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), XVG_BTC=0, BTC_USDT=0, USDT_XVG=0, I=c("|"), XVG_USDT=0, USDT_BTC=0, BTC_XVG=0, I=c("|"), BTC_XVG=0, XVG_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_XVG=0, XVG_BTC=0)
  table_XVG_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), XVG_ETH=0, ETH_BTC=0, BTC_XVG=0, I=c("|"), XVG_BTC=0, BTC_ETH=0, ETH_XVG=0, I=c("|"), ETH_XVG=0, XVG_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_XVG=0, XVG_ETH=0)
  table_ZEC_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ZEC_BTC=0, BTC_USDT=0, USDT_ZEC=0, I=c("|"), ZEC_USDT=0, USDT_BTC=0, BTC_ZEC=0, I=c("|"), BTC_ZEC=0, ZEC_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ZEC=0, ZEC_BTC=0)
  table_ZEC_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ZEC_ETH=0, ETH_BTC=0, BTC_ZEC=0, I=c("|"), ZEC_BTC=0, BTC_ETH=0, ETH_ZEC=0, I=c("|"), ETH_ZEC=0, ZEC_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ZEC=0, ZEC_ETH=0)
  table_ZRX_USDT <<- data.frame(Name=c("Price", "No Commission", "Balance"), ZRX_BTC=0, BTC_USDT=0, USDT_ZRX=0, I=c("|"), ZRX_USDT=0, USDT_BTC=0, BTC_ZRX=0, I=c("|"), BTC_ZRX=0, ZRX_USDT=0, USDT_BTC=0, I=c("|"), BTC_USDT=0, USDT_ZRX=0, ZRX_BTC=0)
  table_ZRX_ETH <<- data.frame(Name=c("Price", "No Commission", "Balance"), ZRX_ETH=0, ETH_BTC=0, BTC_ZRX=0, I=c("|"), ZRX_BTC=0, BTC_ETH=0, ETH_ZRX=0, I=c("|"), ETH_ZRX=0, ZRX_BTC=0, BTC_ETH=0, I=c("|"), ETH_BTC=0, BTC_ZRX=0, ZRX_ETH=0)
  # --------------------------------------
  
  # ----------постоянные данные ----------
  binance_fee <<- 0.001
  bitfinex_fee <<- 0.001
  bittrex_fee <<- 0.0025
  gate_fee <<- 0.002
  hitbtc_fee <<- 0.001
  poloniex_fee <<- 0.001
  data_XXX_RUB <<- content(GET(url="https://api.coinmarketcap.com/v2/ticker/?convert=RUB"), "parsed")
  ADA <<- 10000/data_XXX_RUB$data$`2010`$quotes$RUB$price
  AE <<- 10000/data_XXX_RUB$data$`1700`$quotes$RUB$price
  BCH <<- 10000/data_XXX_RUB$data$`1831`$quotes$RUB$price
  BCN <<- 10000/data_XXX_RUB$data$`372`$quotes$RUB$price
  BNB <<- 10000/data_XXX_RUB$data$`1839`$quotes$RUB$price
  BTC <<- 10000/data_XXX_RUB$data$`1`$quotes$RUB$price
  BTG <<- 10000/data_XXX_RUB$data$`2083`$quotes$RUB$price
  BTS <<- 10000/data_XXX_RUB$data$`463`$quotes$RUB$price
  DASH <<- 10000/data_XXX_RUB$data$`131`$quotes$RUB$price
  DCR <<- 10000/data_XXX_RUB$data$`1168`$quotes$RUB$price
  DOGE <<- 10000/data_XXX_RUB$data$`74`$quotes$RUB$price
  DGB <<- 10000/data_XXX_RUB$data$`109`$quotes$RUB$price
  EOS <<- 10000/data_XXX_RUB$data$`1765`$quotes$RUB$price
  ETC <<- 10000/data_XXX_RUB$data$`1321`$quotes$RUB$price
  ETH <<- 10000/data_XXX_RUB$data$`1027`$quotes$RUB$price
  GNT <<- 10000/data_XXX_RUB$data$`1455`$quotes$RUB$price
  HSR <<- 10000/data_XXX_RUB$data$`1903`$quotes$RUB$price
  ICX <<- 10000/data_XXX_RUB$data$`2099`$quotes$RUB$price
  IOTA <<- 10000/data_XXX_RUB$data$`1720`$quotes$RUB$price
  LSK <<- 10000/data_XXX_RUB$data$`1214`$quotes$RUB$price
  LTC <<- 10000/data_XXX_RUB$data$`2`$quotes$RUB$price
  MIOTA <<- 10000/data_XXX_RUB$data$`1720`$quotes$RUB$price
  NANO <<- 10000/data_XXX_RUB$data$`1567`$quotes$RUB$price
  NEO <<- 10000/data_XXX_RUB$data$`1376`$quotes$RUB$price
  OMG <<- 10000/data_XXX_RUB$data$`1808`$quotes$RUB$price
  ONT <<- 10000/data_XXX_RUB$data$`2566`$quotes$RUB$price
  QTUM <<- 10000/data_XXX_RUB$data$`1684`$quotes$RUB$price
  REP <<- 10000/data_XXX_RUB$data$`1104`$quotes$RUB$price
  SC <<- 10000/data_XXX_RUB$data$`1042`$quotes$RUB$price
  SNT <<- 10000/data_XXX_RUB$data$`1759`$quotes$RUB$price
  STR <<- 10000/data_XXX_RUB$data$`512`$quotes$RUB$price
  STRAT <<- 10000/data_XXX_RUB$data$`1343`$quotes$RUB$price
  TRX <<- 10000/data_XXX_RUB$data$`1958`$quotes$RUB$price
  VET <<- 10000/data_XXX_RUB$data$`3077`$quotes$RUB$price
  WAVES <<- 10000/data_XXX_RUB$data$`1274`$quotes$RUB$price
  WAX <<- 10000/data_XXX_RUB$data$`2300`$quotes$RUB$price
  XEM <<- 10000/data_XXX_RUB$data$`873`$quotes$RUB$price
  XLM <<- 10000/data_XXX_RUB$data$`512`$quotes$RUB$price
  XMR <<- 10000/data_XXX_RUB$data$`328`$quotes$RUB$price
  XRP <<- 10000/data_XXX_RUB$data$`52`$quotes$RUB$price
  XVG <<- 10000/data_XXX_RUB$data$`693`$quotes$RUB$price
  ZEC <<- 10000/data_XXX_RUB$data$`1437`$quotes$RUB$price
  ZRX <<- 10000/data_XXX_RUB$data$`1896`$quotes$RUB$price
  # ---------------------------
  
  # ---------- Binance ----------
  observeEvent(input$binance_act, {
    name_exch <- "Now: Binance"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://api.binance.com/api/v1/ticker/24hr"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- binance_func(result_lim, "lim", data_market_price)
      result_mark <<- binance_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
  
  # ---------- Bitfinex ----------
  observeEvent(input$bitfinex_act, {
    name_exch <- "Now: Bitfinex"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://api.bitfinex.com/v2/tickers?symbols=ALL"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- bitfinex_func(result_lim, "lim", data_market_price)
      result_mark <<- bitfinex_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
  
  # ---------- Bittrex ----------
  observeEvent(input$bittrex_act, {
    name_exch <- "Now: Bittrex"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://bittrex.com/api/v1.1/public/getmarketsummaries"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- bittrex_func(result_lim, "lim", data_market_price)
      result_mark <<- bittrex_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
  
  # ---------- Poloniex ----------
  observeEvent(input$poloniex_act, {
    name_exch <- "Now: Poloniex"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://poloniex.com/public?command=returnTicker"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- poloniex_func(result_lim, "lim", data_market_price)
      result_mark <<- poloniex_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
  
  # ---------- HitBTC ----------
  observeEvent(input$hitbtc_act, {
    name_exch <- "Now: HitBTC"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://api.hitbtc.com/api/2/public/ticker"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- hitbtc_func(result_lim, "lim", data_market_price)
      result_mark <<- hitbtc_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
  
  # ---------- Gate ----------
  observeEvent(input$gate_act, {
    name_exch <- "Now: Gate"
    output$title_name <- renderPrint({name_exch})
    ls_update <- paste("Last update: ",as.character(Sys.time()))
    output$last_update <- renderPrint({ls_update})
    
    withProgress(message = "Calculation", detail = "Please wait...", value = 0, { 
      incProgress(1) 
      # -----------------------------------------------------------------------------------
      # получаем данные курсов криптовалют
      data_market_price <<- content(GET(url="https://data.gate.io/api2/1/tickers"), "parsed")
      # -----------------------------------------------------------------------------------
      # таблица результатов
      result_lim <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      result_mark <<- data.frame(Crypto_Plan=0, Profit_with_10k_RUB=0, Percent_of_Profit=0, stringsAsFactors=FALSE)
      # -----------------------------------------------------------------------------------
      # готовим отчет
      result_lim <<- gate_func(result_lim, "lim", data_market_price)
      result_mark <<- gate_func(result_mark, "mark", data_market_price)
      # -----------------------------------------------------------------------------------
      # отправляем таблицы на вывод
      output$lim_tab<<-renderTable(result_lim[order(-result_lim[,3]),])
      output$mark_tab<<-renderTable(result_mark[order(-result_mark[,3]),])
    }) 
  })
  # ------------------
}

# запускаем проект
shinyApp(ui = ui, server = server)

