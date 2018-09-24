library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(googlesheets)
library(purrr)
library(shiny)
library(lubridate)
library(ggthemes)
library(forcats)
library(flexdashboard)

# Read in old Mountainproject ticks 
mp_ticks_raw <- read_csv(file = "MP ticks.csv") %>% 
  rename_all(str_to_lower) %>%
  rename_all(str_replace_all,
             pattern     = " ",
             replacement = "_") %>% 
  select(date, route, notes, pitches, location, route_type, grade = rating) %>%
  mutate(ticked = T)
  #separate(grade, into = c("grade", "safety_rating"), sep = "^([^ ]*)") #currently broken


  

# Read in googlesheets ticks
gs_ticks_raw <- gs_title("Climbing Log") %>% 
  gs_read() %>% 
  rename_all(str_to_lower) %>%
  rename_all(str_replace_all,
             pattern     = " ",
             replacement = "_") %>%
  filter(ticked == "TRUE") %>% 
  rename(route = route_name,
         pitches = pitch_count,
         gps     = location,
         route_type = style) %>% 
  mutate(grade = if_else(route_type != "Boulder", paste0("5.", grade), grade)) %>% 
  separate(gps, into = c("lat", "long"), sep = ",")

fcts <- expand.grid(paste0("5.", 0:14), c("a", "-", "a/b", "b", "", "b/c", "c", "c/d", "+", "d")) %>% 
  unite(col = grade_fct, remove = F, sep = "") %>% 
  mutate(Var2 = as_factor(Var2) %>% 
           fct_relevel(c("a", "-", "a/b", "b", "", "b/c", "c", "c/d", "+", "d"))) %>% 
  arrange(Var1, Var2)

  
  ticks <- mp_ticks_raw %>% 
    bind_rows(gs_ticks_raw) %>% 
    separate(grade, into = c("grade", "seriousness"), sep = " ") %>% 
    mutate(year = as.character(year(date)),
           boulder_grade = ifelse(str_detect(grade, "V"), grade, NA),
           # boulder_grade = as_factor() %>% 
           #   fct_relevel(paste0("V", 0:15)),
           grade = ifelse(str_detect(grade, "V"), NA, grade),
           grade = as_factor(grade) %>% 
             fct_relevel(fcts$grade_fct),
           num_grade = str_remove_all(grade, "[-+abcd//]"),
           num_grade = if_else(num_grade %in% c("5.1212", "5.1213"), "5.12", num_grade) %>% 
             as_factor() %>% 
             fct_relevel(paste0("5.", 0:14)),
           year = ifelse(is.na(year), "2018", year))

  

  
  
  write_rds(ticks, "ticks.rds")
  
  

  

  # 
  # ui <- fluidPage(
  #   selectInput(inputId = "route_type",
  #                "Style", choices = distinct(ticks, route_type), multiple = F),
  #   plotOutput(outputId = "freqpoly")
  # )
  # 
  # shinyServer <- function(input, output, ticks = ticks) {
  #   dat1 <- reactive({
  #     ticks %>% 
  #       filter(str_detect(route_type, input$route_type),
  #              ticked == T) 
  #   })
  #   
  #   output$freqpoly <- renderPlot({
  #       ggplot() +
  #       aes(x = dat1()$num_grade,
  #           color =  dat1()$year,
  #           group = dat1()$year) +
  #       geom_freqpoly(stat = "count") +
  #       theme_fivethirtyeight() +
  #       theme(axis.text.x = element_text(angle = 45)) 
  #   
  #   })
  # }
  # shinyApp(ui = ui, server = shinyServer)
  # 
  # 
  # shinyServer(function(input, output) {
  #   
  #   data <- read.csv("Amstat.csv", header = TRUE)
  #   amstat1<-reactive({
  #     data %>% select(Type,Price,Model ) %>% filter(Model == input$Model)
  #   })
  #   
  #   output$carpricePlot <- renderPlot({
  #     plot(amstat1()$Model,
  #          amstat1()$Price,
  #          xlab="Model",
  #          ylab="Price",
  #          pch=20)
  #     
  #   })
  # })
  # 