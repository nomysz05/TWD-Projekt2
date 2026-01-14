library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(leaflet)
library(ggplot2)


##  DANE DO TRAS:
user <- "nomysz05"
repo <- "TWD-Projekt2"
files_to_process <- list(
  list(path = "dane magda/dane_Magda.csv", owner = "Magda"),
  list(path = "dane_hela/dane_hela1.csv", owner = "Hela"),
  list(path = "dane_hela/dane_hela2.csv", owner = "Hela"),
  list(path = "Dane Szymon/dane_szymon.csv", owner = "Szymon")
)
fetch_github_data <- function(file_info) {
  url <- URLencode(paste0("https://raw.githubusercontent.com/", user, "/", repo, "/main/", file_info$path))
  tryCatch({
    df <- read_csv(url, show_col_types = FALSE)
    df %>% 
      filter(!is.na(position_lat) & !is.na(position_long)) %>% 
      mutate(osoba = file_info$owner, timestamp = as.POSIXct(timestamp))
  }, error = function(e) return(NULL))
}
wszystkie_punkty <- map_df(files_to_process, fetch_github_data)

## DANE 2 SZYMONA
rankingi_osobiste <- wszystkie_punkty %>%
  mutate(
    lat_approx = round(position_lat, 3),
    long_approx = round(position_long, 3),
    loc_id = paste(lat_approx, long_approx, sep = ", ")
  ) %>%
  group_by(osoba) %>% 
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(
    diff_time = as.numeric(difftime(lead(timestamp), timestamp, units = "mins")),
    is_same_loc = (lat_approx == lead(lat_approx) & long_approx == lead(long_approx))
  ) %>%
  filter(diff_time > 60 | (is_same_loc & diff_time > 30)) %>%
  group_by(osoba, loc_id, lat_approx, long_approx) %>%
  summarise(ilosc_wizyt = n(), ostatnia_wizyta = max(timestamp), .groups = "drop") %>%
  mutate(nazwa_wykres = case_when(
    osoba == "Hela" & loc_id == "52.204, 21.012" ~ "Dom",
    osoba == "Hela" & loc_id == "52.222, 21.007" ~ "MiNI",
    osoba == "Hela" & loc_id == "52.216, 21.017" ~ "Dom dziewczyny",
    osoba == "Hela" & loc_id == "61.317, 12.615" ~ "Wyjazd do Norwegii",
    osoba == "Hela" & loc_id == "52.221, 21.01"  ~ "Gmach Główny",
    osoba == "Hela" & loc_id == "52.19, 21.018"  ~ "Bar Wierzbno",
    osoba == "Hela" & loc_id == "52.218, 20.984" ~ "DS Alcatraz",
    osoba == "Hela" & loc_id == "52.204, 21.013" ~ "Dom",
    osoba == "Magda" & loc_id == "51.992, 21.059" ~ "Dom",
    osoba == "Magda" & loc_id == "52.222, 21.007" ~ "Gmach Główny",
    osoba == "Magda" & loc_id == "52.221, 21.009" ~ "MiNI",
    osoba == "Magda" & loc_id == "51.992, 21.06"  ~ "Dom",
    osoba == "Magda" & loc_id == "51.247, 22.565" ~ "Lublin",
    osoba == "Magda" & loc_id == "51.232, 22.567" ~ "Lublin",
    osoba == "Magda" & loc_id == "50.138, 19.558" ~ "Zepsuty pociąg",
    osoba == "Magda" & loc_id == "49.486, 21.613" ~ "Ropianka",
    osoba == "Szymon" & loc_id == "52.249, 21.088" ~ "Dom",
    osoba == "Szymon" & loc_id == "52.249, 21.089" ~ "Dom",
    osoba == "Szymon" & loc_id == "52.222, 21.007" ~ "MiNI",
    osoba == "Szymon" & loc_id == "52.213, 21.02"  ~ "Siłownia",
    osoba == "Szymon" & loc_id == "52.213, 21.019" ~ "Siłownia",
    osoba == "Szymon" & loc_id == "52.144, 21.005" ~ "Ścianka",
    osoba == "Szymon" & loc_id == "52.13, 20.768"  ~ "Dom dziewczyny",
    osoba == "Szymon" & loc_id == "46.43, 11.712"  ~ "wyjazd/włochy",
    TRUE ~ loc_id
  ))

aktywnosc_godzinowa <- wszystkie_punkty %>%
  mutate(speed = ifelse(is.na(speed), 0, speed)) %>% 
  filter(speed * 3.6 > 2) %>% 
  mutate(godzina = hour(timestamp)) %>%
  group_by(osoba, godzina) %>%
  summarise(intensywnosc = n(), .groups = "drop")

##  APLIKACJA:
ui <- navbarPage(
  title = "Nasza aktywność w ciągu miesiąca",
  theme = bs_theme(version = 5, bootswatch = "darkly"),

  tabPanel("Trasa aktywności",
           sidebarLayout(
             sidebarPanel(
               h4("Filtry"),
               dateInput("data", 
                         "Wybierz datę:", 
                         value = "2025-12-03",
                         min = "2025-12-03", 
                         max = "2026-01-13",
                         language = "pl"),
               checkboxGroupInput("osoba",
                                  label = "Wybierz osoby do wyświetlenia:",
                                  choices = c("Hela", "Magda", "Szymon"),
                                  selected = c("Hela", "Magda", "Szymon"),
                                  inline = FALSE)
             ),
             mainPanel(
               leafletOutput("mapa_sciezek", height = "600px")
               )
             )
           ),

  tabPanel("Gdzie najczęściej się pojawialiśmy",
           fluidPage(
             h4("Mapa najczęściej odwiedzanych miejsc"),
             card(
               card_body(
                 checkboxGroupInput("osoba_zbiorcza", "Wybierz osoby do wyświetlenia na mapie:",
                                    choices = c("Hela", "Magda", "Szymon"),
                                    selected = c("Hela", "Magda", "Szymon"),
                                    inline = TRUE)
               )
             ),
             leafletOutput("mapa_zbiorcza", height = "400px"),
             br(),
             card(
               card_header(h4("Najczęściej odwiedzane przez nas miejsca")),
               plotOutput("wykres_slupkowy", height = "500px")
             )
           )
           
  ),
  tabPanel("O której najczęściej się poruszaliśmy",
           fluidPage(
             h4("Intensywność ruchu w ciągu doby"),
             p("Wykres pokazuje liczbę zarejestrowanych punktów o prędkości > 2 km/h w podziale na godziny."),
             card(
               plotOutput("wykres_godzinowy", height = "600px")
             )
           )
  )
  
)
server <- function(input, output, session) {
    
    output$mapa_sciezek <- renderLeaflet({
      data <- wszystkie_punkty %>%
      filter(osoba %in% input$osoba,
             as.Date(timestamp) == input$data)
    
      validate(
        need(nrow(data) > 0, "Brak zarejestrowanych tras dla wybranych osób w tym dniu. Wybierz inną datę lub inną osobę.")
      )
      
      data_plot <- data %>% 
        group_by(osoba) %>%
        slice(seq(1, n(), by = 10)) %>%
        ungroup()
   
      pal <- colorFactor(palette = c("#E16036", "darkred", "#241715"), 
                         domain = c("Hela", "Magda", "Szymon"))
      
      leaflet(data_plot) %>%
        addTiles() %>%
        addCircles(~position_long, ~position_lat, color = ~pal(osoba), radius = 5, weight = 2,
                   fillOpacity = 0.6, popup = ~paste0("Godzina: ", format(timestamp, "%H:%M:%S"))) %>%
        addLegend("bottomright", pal = pal, values = c("Hela", "Magda", "Szymon"), title = "Osoba", opacity=1)
  })
    
    output$mapa_zbiorcza <- renderLeaflet({
      req(input$osoba_zbiorcza)
      data_zbiorcza <- rankingi_osobiste %>% filter(osoba %in% input$osoba_zbiorcza)
      
      color_palette <- colorFactor(
        palette = c("Hela" = "#E16036", "Magda" = "darkred", "Szymon" = "#241715"), 
        domain = c("Hela", "Magda", "Szymon")
      )
      
      leaflet(data_zbiorcza) %>%
        addTiles() %>%
        addCircleMarkers(~long_approx, ~lat_approx, radius = ~sqrt(ilosc_wizyt) * 5, 
                         color = ~color_palette(osoba), stroke = TRUE, weight = 2, fillOpacity = 0.6,
                         popup = ~paste0("<b>Osoba:</b> ", osoba, "<br><b>Miejsce:</b> ", nazwa_wykres, "<br><b>Wizyt:</b> ", ilosc_wizyt))
    })
    
    output$wykres_slupkowy <- renderPlot({
      rankingi_osobiste %>%
        group_by(osoba) %>%
        slice_max(order_by = ilosc_wizyt, n = 8, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(nazwa_sort = factor(paste(osoba, nazwa_wykres, sep = "__"))) %>%
        mutate(nazwa_sort = reorder(nazwa_sort, ilosc_wizyt)) %>%
        ggplot(aes(x = nazwa_sort, y = ilosc_wizyt, fill = osoba)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~osoba, scales = "free", ncol = 1) +
        scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
        coord_flip() +
        labs(x = "Miejsce", y = "Liczba wizyt") +
        theme_minimal() +
        scale_fill_manual(values = c("Magda" = "darkred", "Hela" = "#E16036", "Szymon" = "#241715"))
    })
    
    output$wykres_godzinowy <- renderPlot({
      ggplot(aktywnosc_godzinowa, aes(x = godzina, y = intensywnosc, color = osoba)) +
        geom_line(linewidth = 1.2) +
        geom_area(aes(fill = osoba), alpha = 0.1, position = "identity") +
        facet_wrap(~osoba, ncol = 1, scales = "free_y") +
        scale_x_continuous(breaks = 0:23) +
        scale_color_manual(values = c("Magda" = "darkred", "Hela" = "#E16036", "Szymon" = "#241715")) +
        scale_fill_manual(values = c("Magda" = "darkred", "Hela" = "#E16036", "Szymon" = "#241715")) +
        labs(x = "Godzina", y = "Liczba logów") +
        theme_minimal() +
        theme(legend.position = "none")
    })
}

shinyApp(ui = ui, server = server)

