library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
dane<- read_csv("C:/Users/szyml/Downloads/GOTOES_FIT-CSV_4704834830900716.csv") 

folder_path <- "C:/Users/szyml/Desktop/programowanie/twd/projekt2"
csv_files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",  
  full.names = TRUE      
)


print(csv_files)
data_list <- lapply(csv_files, read_csv, show_col_types = FALSE)


dane <- bind_rows(data_list)
dane <-dane%>% 
  select(timestamp,position_lat,position_long,distance,speed)

dane_bez_NA <-dane %>% 
  filter(!is.na(position_lat)) %>% 
  slice(seq(1,nrow(dane),10))
leaflet(dane_bez_NA) %>% 
  addTiles() %>% 
  addCircles(
    lng=~position_long,
    lat=~position_lat
  )
