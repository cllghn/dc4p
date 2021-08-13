# https://comtrade.un.org/data/doc/api/#DataAvailabilityRequests
get_comtrade <- function(url = "http://comtrade.un.org/api/get?",
                         reporter, # Reporting area
                         partner, # Partner area: 'all', or 496 (Mongolia) 
                         frequency = "A", # Data set frequency: Annual (A) or Monthly (M)
                         time_period = "now", # Time period: 'YYYY', 'now' or 'recent'
                         classification = "HS", # Classification
                         trade_flow = "all", # Trade flow: 'all', 1 = imports, 2 = exports
                         classification_code = "TOTAL", # Classification code
                         # fmt = "json", # Output format: 'json' or 'csv'
                         maxrec = 10000, 
                         trade_type = "C", # Trade data type: 'C' = commodities and 'S' = services
                         quietly = TRUE
) {
  input <- paste0(url, "max=", maxrec, "&", "type=", trade_type, "&", 
                  "freq=", frequency, "&", "px=", classification, "&",
                  "ps=", time_period, "&", "r=", reporter, "&",
                  "p=", partner, "&", "rg=", trade_flow, "&",
                  "cc=", classification_code, "&", "fmt=json"
  )
  if (!quietly) {
    print(input)
  }
  out <- jsonlite::fromJSON(txt = input)
  return(out)
}
reporters <- jsonlite::fromJSON("https://comtrade.un.org/Data/cache/reporterAreas.json")[["results"]]
reporters <- reporters[!reporters$text %in% c("All", "World", "Mongolia"), ]

china <- get_comtrade(reporter = "156", partner  = "all", 
                      classification_code = "TOTAL", quietly = FALSE)[["dataset"]] 
japan <- get_comtrade(reporter = "392", partner  = "all", 
                      classification_code = "TOTAL", quietly = FALSE)[["dataset"]] 
south_korea <- get_comtrade(reporter = "410", partner  = "all", 
                            classification_code = "TOTAL", quietly = FALSE)[["dataset"]] 
russia <- get_comtrade(reporter = "643", partner  = "all", 
                      classification_code = "TOTAL", quietly = FALSE)[["dataset"]] 
usa <- get_comtrade(reporter = "842", partner  = "all", 
                    classification_code = "TOTAL", quietly = FALSE)[["dataset"]] 

library(tidyverse)
library(igraph)
trade <- rbind(china, japan, south_korea, russia, usa) %>%
  janitor::clean_names()

g <- trade %>%
  select(rt_title, pt_title, rg_desc, trade_value) %>%
  mutate(rg_desc = case_when(rg_desc == "Re-Import" ~ "Import",
                             rg_desc == "Re-Export" ~ "Export", 
                             TRUE ~ rg_desc),
         source = case_when(rg_desc == "Import" ~ pt_title, 
                            rg_desc == "Export" ~ rt_title),
         target = case_when(rg_desc == "Import" ~ rt_title, 
                            rg_desc == "Export" ~ pt_title)
         ) %>%
  group_by(source, target, rg_desc) %>%
  summarise(total_trade_value = sum(trade_value)) %>%
  graph_from_data_frame()
degree_distribution(g)
  
  
g %>%
  visNetwork::visIgraph() %>%
  visNetwork::visIgraphLayout(layout = "layout_with_fr")

  

  
  
  
  