# install.packages('devtools')
# install.packages('beepr')
# devtools::install_github("ropensci/bittrex")
# devtools::install_github('toneloy/tradeR', auth_token = Sys.getenv('GITHUB_TOKEN'))

# Load packages ####
library(bittrex)
library(dplyr)
library(tidyr)

# Get markets ####
check_for_arbitrage <- function(.threshold = 1.0075) {
  market_summaries_response <- bt_getmarketsummaries()
  market_summaries <- market_summaries_response$result
  market_summaries <- market_summaries %>% 
    separate(market_name, into = c('base_symbol', 'market_symbol'), sep = '-')
  
  bid <- market_summaries %>% 
    select(market_symbol, base_symbol, bid) %>% 
    spread(base_symbol, bid)
  
  ask <- market_summaries %>% 
    select(market_symbol, base_symbol, ask) %>% 
    spread(base_symbol, ask)
  
  
  base_prices <- ask %>% filter(market_symbol %in% c('BTC', 'ETH'))
  eth_btc <- base_prices %>% filter(market_symbol == 'ETH') %>% pull(BTC)
  eth_usdt <- base_prices %>% filter(market_symbol == 'ETH') %>% pull(USDT)
  btc_usdt <- base_prices %>% filter(market_symbol == 'BTC') %>% pull(USDT)
  
  arbitrage_df <- inner_join(bid, ask, by = 'market_symbol', suffix = c('_BID', '_ASK')) %>% 
    select(market_symbol, starts_with('BTC'), starts_with('ETH'), starts_with('USDT')) %>% 
    mutate(arbitrage_btc_eth = eth_btc / (BTC_ASK / ETH_BID)) %>% 
    arrange(desc(arbitrage_btc_eth))
  
  if(sum(arbitrage_df$arbitrage_btc_eth > .threshold, na.rm = TRUE) > 0) {
    beepr::beep(5)
    cat("There's arbitrage!\n")
    print(arbitrage_df %>% filter(arbitrage_btc_eth > .threshold))
  }
}

while(TRUE) {
  Sys.sleep(1)
  check_for_arbitrage()
}
