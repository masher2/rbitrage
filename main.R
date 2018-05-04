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

N_SNAPSHOTS <- 60
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
  
  
  base_prices_ask <- ask %>% filter(market_symbol %in% c('BTC', 'ETH'))
  eth_btc_ask <- base_prices_ask %>% filter(market_symbol == 'ETH') %>% pull(BTC)
  eth_usdt_ask <- base_prices_ask %>% filter(market_symbol == 'ETH') %>% pull(USDT)
  btc_usdt_ask <- base_prices_ask %>% filter(market_symbol == 'BTC') %>% pull(USDT)
  
  base_prices_bid <- bid %>% filter(market_symbol %in% c('BTC', 'ETH'))
  eth_btc_bid <- base_prices_bid %>% filter(market_symbol == 'ETH') %>% pull(BTC)
  eth_usdt_bid <- base_prices_bid %>% filter(market_symbol == 'ETH') %>% pull(USDT)
  btc_usdt_bid <- base_prices_bid %>% filter(market_symbol == 'BTC') %>% pull(USDT)
  
  arbitrage_df <- inner_join(bid, ask, by = 'market_symbol', suffix = c('_BID', '_ASK')) %>% 
    select(market_symbol, starts_with('BTC'), starts_with('ETH'), starts_with('USDT')) %>% 
    mutate(
      arbitrage_btc_eth_indirect = eth_btc_ask / (BTC_ASK / ETH_BID),
      arbitrage_btc_eth_direct = (BTC_BID / ETH_ASK) / eth_btc_bid,
      arbitrage_usdt_btc_indirect = btc_usdt_ask / (USDT_ASK / BTC_BID),
      arbitrage_usdt_btc_direct = (USDT_BID / BTC_ASK) / btc_usdt_bid,
      arbitrage_usdt_eth_indirect = eth_usdt_ask / (USDT_ASK / ETH_BID),
      arbitrage_usdt_eth_direct = (USDT_BID / ETH_ASK) / eth_usdt_bid
    ) %>% 
    mutate(
      btc_eth_indirect_gain = arbitrage_btc_eth_indirect * ((1 - .fee) ^ 3) - 1,
      btc_eth_direct_gain = arbitrage_btc_eth_direct * ((1 - .fee) ^ 3) - 1,
      usdt_btc_indirect_gain = arbitrage_usdt_btc_indirect * ((1 - .fee) ^ 3) - 1,
      usdt_btc_direct_gain = arbitrage_usdt_btc_direct * ((1 - .fee) ^ 3) - 1,
      usdt_eth_indirect_gain = arbitrage_usdt_eth_indirect * ((1 - .fee) ^ 3) - 1,
      usdt_eth_direct_gain = arbitrage_usdt_eth_direct * ((1 - .fee) ^ 3) - 1
    ) %>% 
    arrange(desc(pmax(arbitrage_btc_eth_direct, arbitrage_btc_eth_indirect)))
  
  btc_eth_arbitrage_snapshot = arbitrage_df %>% 
    select(market_symbol, btc_eth_indirect_gain, btc_eth_direct_gain) %>% 
    gather(key = 'type', value = 'gain', btc_eth_indirect_gain, btc_eth_direct_gain, na.rm = TRUE) %>%
    mutate(timestamp = as.numeric(Sys.time())) %>% 
    filter(gain > 0)
  
  usdt_btc_arbitrage_snapshot = arbitrage_df %>% 
    select(market_symbol, usdt_btc_indirect_gain, usdt_btc_direct_gain) %>% 
    gather(key = 'type', value = 'gain', usdt_btc_indirect_gain, usdt_btc_direct_gain, na.rm = TRUE) %>%
    mutate(timestamp = as.numeric(Sys.time())) %>% 
    filter(gain > 0)
  
  usdt_eth_arbitrage_snapshot = arbitrage_df %>% 
    select(market_symbol, usdt_eth_indirect_gain, usdt_eth_direct_gain) %>% 
    gather(key = 'type', value = 'gain', usdt_eth_indirect_gain, usdt_eth_direct_gain, na.rm = TRUE) %>%
    mutate(timestamp = as.numeric(Sys.time())) %>% 
    filter(gain > 0)
  
  arbitrage_snapshot <- bind_rows(list(
    btc_eth_arbitrage_snapshot,
    usdt_btc_arbitrage_snapshot,
    usdt_eth_arbitrage_snapshot
  ))
  
  return(list(arbitrage_snapshot = arbitrage_snapshot, arbitrage_df = arbitrage_df))
}

arbitrage_snapshots = data_frame(
  market_symbol = character(),
  type = character(),
  gain = numeric(),
  timestamp = numeric()
)

for(i in 1:N_SNAPSHOTS) {
  cat(sprintf('Working on iteration %s: ', i))
  snapshot <- check_for_arbitrage()$arbitrage_snapshot
  arbitrage_snapshots <- arbitrage_snapshots %>% bind_rows(snapshot)
  cat(sprintf('%s arbitrages\n', nrow(snapshot)))
  Sys.sleep(INTERVAL)
}

cat('Exporting data')
arbitrage_snapshots <- arbitrage_snapshots %>% mutate(timestamp = as_datetime(timestamp))
write.csv(arbitrage_snapshots, file.path(PROCESSED_DATA_DIR, FILE_NAME), row.names = FALSE)
