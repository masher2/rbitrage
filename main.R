# install.packages("devtools")
# install.packages("beepr")
# devtools::install_github("ropensci/bittrex")
# devtools::install_github("toneloy/tradeR", auth_token = Sys.getenv("GITHUB_TOKEN"))

# Load packages -----------------------------------------------------------

library(bittrex)
library(dplyr)
library(tidyr)
library(lubridate)
import::from("utils.R", check_for_arbitrage)


# Initial constants -------------------------------------------------------

DATA_DIR <- "data"
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "processed")

N_SNAPSHOTS <- 60
INTERVAL <- 1
FILE_NAME <- "arbitrage_snapshots.csv"


# Arbitrage snapshots -----------------------------------------------------

arbitrage_snapshots = data_frame(
  market_symbol = character(),
  type = character(),
  gain = numeric(),
  timestamp = numeric()
)

for(i in 1:N_SNAPSHOTS) {
  cat(sprintf("Working on iteration %s: ", i))
  
  snapshot <- check_for_arbitrage()$arbitrage_snapshot
  arbitrage_snapshots <- arbitrage_snapshots %>% bind_rows(snapshot)
  cat(sprintf("%s arbitrages\n", nrow(snapshot)))
  
  Sys.sleep(INTERVAL)
}

cat("Exporting data")
arbitrage_snapshots <- arbitrage_snapshots %>% mutate(timestamp = as_datetime(timestamp))
write.csv(arbitrage_snapshots,
          file.path(PROCESSED_DATA_DIR, FILE_NAME),
          row.names = FALSE)
