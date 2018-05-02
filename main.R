# install.packages('devtools')
# install.packages('beepr')
# devtools::install_github("ropensci/bittrex")
# devtools::install_github('toneloy/tradeR', auth_token = Sys.getenv('GITHUB_TOKEN'))

# Load packages ####
library(bittrex)
library(dplyr)
library(tidyr)
library(lubridate)

DATA_DIR <- 'data'
PROCESSED_DATA_DIR <- file.path(DATA_DIR, 'processed')

N_SNAPSHOTS <- 10
INTERVAL <- 1
FILE_NAME <- 'arbitrage_snapshots.csv'

# Get markets ####
check_for_arbitrage <- function(.threshold = 1.0075, .fee = .0025, .sound = TRUE) {
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
    mutate(
      arbitrage_btc_eth_indirect = eth_btc / (BTC_ASK / ETH_BID),
      arbitrage_btc_eth_direct = 1 / arbitrage_btc_eth_indirect
    ) %>% 
    mutate(
      indirect_gain = arbitrage_btc_eth_indirect * ((1 - .fee) ^ 3) - 1,
      direct_gain = arbitrage_btc_eth_direct * ((1 - .fee) ^ 3) - 1
    ) %>% 
    arrange(desc(pmax(arbitrage_btc_eth_direct, arbitrage_btc_eth_indirect)))
  
  arbitrage_snapshot = arbitrage_df %>% 
    select(market_symbol, indirect_gain, direct_gain) %>% 
    gather(key = 'type', value = 'gain', indirect_gain, direct_gain, na.rm = TRUE) %>%
    mutate(timestamp = as.numeric(Sys.time())) %>% 
    filter(gain > 0)
  
  return(list(arbitrage_snapshot = arbitrage_snapshot, arbitrage_df = arbitrage_df))
}

arbitrage_snapshots = data_frame(
  market_symbol = character(),
  type = character(),
  gain = numeric(),
  timestamp = numeric()
)

for(i in 1:N_SNAPSHOTS) {
  cat(sprintf('Working on iteration %s\n', i))
  arbitrage_snapshots <<- arbitrage_snapshots %>% bind_rows(check_for_arbitrage()$arbitrage_snapshot)
  Sys.sleep(INTERVAL)
}

cat('Exporting data')
arbitrage_snapshots <- arbitrage_snapshots %>% mutate(timestamp = as_datetime(timestamp))
write.csv(arbitrage_snapshots, file.path(PROCESSED_DATA_DIR, FILE_NAME), row.names = FALSE)
