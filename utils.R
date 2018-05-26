#' Gets market summary for an Exchange.
#'
#' @param exchange The name of an exchange.
#'
#' @return A data.frame with the market summary.
get_market_summary <- function(exchange) {
  exchange <- tolower(exchange)
  
  if (exchange == "bittrex") {
    mk_summaries <- bittrex::bt_getmarketsummaries()
    mk_summaries <- 
      mk_summaries$result %>% 
      separate(market_name,
               into = c("base_symbol", "market_symbol"),
               sep = "-")
  } else {
    stop(paste0("Exchange '", exchange, "' not supported (yet) (or maybe you made a typo)"))
  }
  
  mk_summaries
}

#' Check for arbitrage.
#'
#' Checks the market summary for a given exchange looking for the existence of arbitrages.
#'
#' @param .exchange The name of an exchange.
#' @param .threshold 
#' @param .fee The fee of the exchange.
#' @param .sound 
#'
#' @return A named list with the following elements:
#' \itemize{
#'   \item{arbitrage_snapshot: }{A data.frame with the found arbitrages.}
#'   \item{arbitrage_df: }{A data.frame with all the markets evaluated.}
#' }
check_for_arbitrage <- function(.exchange, .threshold = 1.0075, .fee = .0025, .sound = TRUE) {
  market_summaries <- get_market_summary(.exchange)
  t_stamp <- Sys.time()
  
  bid <- 
    market_summaries %>% 
    select(market_symbol, base_symbol, bid) %>% 
    spread(base_symbol, bid)
  
  ask <-
    market_summaries %>% 
    select(market_symbol, base_symbol, ask) %>% 
    spread(base_symbol, ask)
  
  base_prices_ask <- ask %>% filter(market_symbol %in% c("BTC", "ETH"))
  eth_btc_ask <- base_prices_ask %>% filter(market_symbol == "ETH") %>% pull(BTC)
  eth_usdt_ask <- base_prices_ask %>% filter(market_symbol == "ETH") %>% pull(USDT)
  btc_usdt_ask <- base_prices_ask %>% filter(market_symbol == "BTC") %>% pull(USDT)
  
  base_prices_bid <- bid %>% filter(market_symbol %in% c("BTC", "ETH"))
  eth_btc_bid <- base_prices_bid %>% filter(market_symbol == "ETH") %>% pull(BTC)
  eth_usdt_bid <- base_prices_bid %>% filter(market_symbol == "ETH") %>% pull(USDT)
  btc_usdt_bid <- base_prices_bid %>% filter(market_symbol == "BTC") %>% pull(USDT)
  
  arbitrage_df <-
    inner_join(bid, ask,
               by = "market_symbol",
               suffix = c("_BID", "_ASK")) %>% 
    select(market_symbol, matches("^(BTC|ETH|USDT)")) %>% 
    mutate(
      arbitrage_btc_eth_indirect = eth_btc_ask / (BTC_ASK / ETH_BID),
      arbitrage_btc_eth_direct = (BTC_BID / ETH_ASK) / eth_btc_bid,
      arbitrage_usdt_btc_indirect = btc_usdt_ask / (USDT_ASK / BTC_BID),
      arbitrage_usdt_btc_direct = (USDT_BID / BTC_ASK) / btc_usdt_bid,
      arbitrage_usdt_eth_indirect = eth_usdt_ask / (USDT_ASK / ETH_BID),
      arbitrage_usdt_eth_direct = (USDT_BID / ETH_ASK) / eth_usdt_bid
    ) %>% 
    mutate(
      btc_eth_indirect_gain = arbitrage_btc_eth_indirect*((1 - .fee)^3) - 1,
      btc_eth_direct_gain = arbitrage_btc_eth_direct*((1 - .fee)^3) - 1,
      usdt_btc_indirect_gain = arbitrage_usdt_btc_indirect*((1 - .fee)^3) - 1,
      usdt_btc_direct_gain = arbitrage_usdt_btc_direct*((1 - .fee)^3) - 1,
      usdt_eth_indirect_gain = arbitrage_usdt_eth_indirect*((1 - .fee)^3) - 1,
      usdt_eth_direct_gain = arbitrage_usdt_eth_direct*((1 - .fee)^3) - 1
    ) %>% 
    arrange(desc(pmax(arbitrage_btc_eth_direct, arbitrage_btc_eth_indirect)))
  
  arbitrage_snapshot <- 
    arbitrage_df %>% 
    select(market_symbol, ends_with("gain")) %>% 
    gather("type", "gain", ends_with("gain"), na.rm = TRUE) %>%
    mutate(timestamp = as.numeric(t_stamp)) %>% 
    filter(gain > 0)
  
  return(list(arbitrage_snapshot = arbitrage_snapshot, arbitrage_df = arbitrage_df))
}