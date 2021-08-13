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
partners <- jsonlite::fromJSON("https://comtrade.un.org/Data/cache/partnerAreas.json")[["results"]]
partners <- partners[!partners$text %in% c("All", "World"), ]


# Mongolia: 496
commodities_exchanges_2020 <- get_comtrade(reporter = "all", partner  = "496",
                                           classification_code = "TOTAL", quietly = FALSE)[["dataset"]]
commodities_exchanges_2019 <- get_comtrade(reporter = "all", partner  = "496",
                                           classification_code = "TOTAL",
                                           time_period = 2019, quietly = FALSE)[["dataset"]]
commodities_exchanges_2018 <- get_comtrade(reporter = "all", partner  = "496",
                                           classification_code = "TOTAL",
                                           time_period = 2018, quietly = FALSE)[["dataset"]]

out <- rbind(commodities_exchanges_2020, commodities_exchanges_2019, commodities_exchanges_2018)
write.csv(out, file = "un_comtrade_mongolia_commodities_exchanges_2018to2020.csv", row.names = FALSE)


test <- get_comtrade(reporter = "156", partner  = "all",
             classification_code = "TOTAL", quietly = FALSE)[["dataset"]]

write.csv(rbind(test, test2), file = "dummy.csv")

test2 <- get_comtrade(reporter = "all", partner  = "156",
                     classification_code = "TOTAL", quietly = FALSE)[["dataset"]]
# TODO
all_exchanges_china <- get_comtrade(reporter = "196", partner  = "496",
                                   classification_code = "ALL",
                                   time_period = 2020, quietly = FALSE)

test <- lapply(reporters$id, function(x) {
  res <- tryCatch(
    {
    get_comtrade(reporter = x, partner = "all", classification_code = "ALL", quietly = FALSE)# [["dataset"]]
      },
    error = NULL,
    warning = NULL
    )

  if (!is.null(res) & length(res) != 0) {
    res[["my_reporter"]] <- x
    return(res)
  }
})

library(httr)
errors <- data.frame(id = NA_character_, error = NA_character_)
test <- lapply(reporters$id, function(x) {
  res <- tryCatch(
    {
      GET(paste0("http://comtrade.un.org/api/get?max=10000&type=C&freq=A&px=HS&ps=now&r=", x, "&p=all&rg=all&cc=TOTAL&fmt=json"))
    },
    error = NULL,
    warning = NULL
  )
  
  if (res$status_code == 200) {
    out <- res %>% 
      httr::content() %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON()
    out[["my_reporter"]] <- x
    print(paste0("Got dataframe for ", x))
    return(out)
  } else if (res$status_code == 500) {
    temp <- data.frame(id = x, error = res$status_code)
    rbind(errors, temp)
    print("Timing out...")
    Sys.sleep(3600)
  } else {
    temp <- data.frame(id = x, error = res$status_code)
    rbind(errors, temp)
    print(paste0("Skipping ", x))
  }
})

r <- GET("http://comtrade.un.org/api/get?max=10000&type=C&freq=A&px=HS&ps=now&r=156&p=all&rg=all&cc=TOTAL&fmt=json")

