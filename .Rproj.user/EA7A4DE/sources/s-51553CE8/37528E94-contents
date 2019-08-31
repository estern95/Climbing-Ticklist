library(plotly)

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
ticks1 <- ticks %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

get_googlemap(center = c(mean(ticks1$lon, na.rm = T), 
                         mean(ticks1$lat, na.rm = T)),
              maptype = "satellite") %>%
  ggmap() +
  geom_point(data = ticks1,
               aes(x = lon, 
                   y = lat),
             color = "red")
  
  
  