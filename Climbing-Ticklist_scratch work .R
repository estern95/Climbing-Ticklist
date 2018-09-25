
# user_info <- "https://www.mountainproject.com/data/get-user?email=eric.stern1@gmail.com&key=109104070-683b2b72dd7bd52ea79b042e5aca9a3f"
# tick_list <- "https://www.mountainproject.com/data/get-ticks?email=eric.stern1@gmail.com&key=109104070-683b2b72dd7bd52ea79b042e5aca9a3f"
# 
# API <- "https://www.mountainproject.com/data"
# 
# raw.result <- GET(url = user_info)
# user_data <- content(raw.result)
# 
# get_ticks <- GET(url = tick_list)
# tick_data <- content(get_ticks) 
# 
# new_ticks <- tick_data %>% 
#   pluck("ticks") %>% 
#   bind_rows()
# 
# # need to create a map for route ID
# 
# route_id <- new_ticks %>% 
#   pluck("routeId") %>%
#   map_df(function(.x) {
#     url <- paste0('https://www.mountainproject.com/route/', .x)
#     
#     webpage <- read_html(url)
#     
#     rank_data_html <- html_nodes(webpage, '.pt-main-content') 
#     
#     subject_line <- data_frame(subject_line = html_text(rank_data_html))
#   })
#   


# gps <- read_html("https://www.mountainproject.com/route/105750778/the-flying-beast
# ") %>% 
#   html_node("td") %>% 
#   html_text()


ticks <- read_rds("ticks.rds") %>% 
  mutate_at(vars(lat, long), as.numeric) %>% 
  mutate(geo_out = map_if(location, !is.na(location), ~geocode(.))) %>% 
  unnest(location) 
  
  coalesce(lat, lat) %>% 
  coalesce(lon, long)


us_map <- map_data("usa")


# route map first draft
p <- ggplot() +
  geom_polygon(data = us_map, 
                 aes(x = long, y = lat, group = group),
               fill="grey") +
  geom_point(data = ticks,
             aes(x = long, y = lat, text = route)) +
  coord_map() +
  theme_void() 
  
ggplotly(p, tooltip = "text")
    
      
# returns a map with sattelite overlay
gs_ticks <- ticks %>% 
  filter(!is.na(lat))

get_googlemap(center = c(mean(gs_ticks$long, na.rm = T), 
                         mean(gs_ticks$lat, na.rm = T)), 
              zoom = 4,
              maptype = "satellite") %>%
  ggmap() +
  geom_point(data = gs_ticks,
             mapping = 
               aes(x = long, 
                   y = lat),
             color = "red")
  
  
  