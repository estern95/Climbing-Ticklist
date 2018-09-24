library(tidyverse)
library(httr)
library(jsonlite)

user_info <- "https://www.mountainproject.com/data/get-user?email=eric.stern1@gmail.com&key=109104070-683b2b72dd7bd52ea79b042e5aca9a3f"
tick_list <- "https://www.mountainproject.com/data/get-ticks?email=eric.stern1@gmail.com&key=109104070-683b2b72dd7bd52ea79b042e5aca9a3f"

API <- "https://www.mountainproject.com/data"

raw.result <- GET(url = user_info)
user_data <- content(raw.result)

get_ticks <- GET(url = tick_list)
tick_data <- content(get_ticks) 

new_ticks <- tick_data %>% 
  pluck("ticks") %>% 
  bind_rows()

# need to create a map for route ID

route_id <- new_ticks %>% 
  pluck("routeId") %>%
  map_df(function(.x) {
    url <- paste0('https://www.mountainproject.com/route/', .x)
    
    webpage <- read_html(url)
    
    rank_data_html <- html_nodes(webpage, '.pt-main-content') 
    
    subject_line <- data_frame(subject_line = html_text(rank_data_html))
  })
  




