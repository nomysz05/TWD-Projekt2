library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(leaflet)
library(ggplot2)
library(plotly)
library(scales)


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

##  DANE DO ZAKŁADKI 2 i 3 :
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
    osoba == "Hela" & loc_id == "52.216, 21.017" ~ "Mikrus",
    osoba == "Hela" & loc_id == "61.317, 12.615" ~ "Wyjazd do Norwegii",
    osoba == "Hela" & loc_id == "52.221, 21.01"  ~ "Gmach Główny",
    osoba == "Hela" & loc_id == "52.19, 21.018"  ~ "Dom znajomego",
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

##  DANE DO ZAKŁADKI 3 I 4:
daneM <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane%20magda/DailySteps_2026_01_13_1700.csv")
daneH <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane_hela/DailySteps_2026_01_13_1940.csv")
daneS <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/daneSzymon/PacerDataLogs/DailySteps_2026_01_13_1952.csv")

daneM_pom <- daneM %>%
  mutate(Data = as.Date(Date), Kroki = as.numeric(Steps), Dystans_km = as.numeric(Distance.meters.) / 1000, Osoba = "Magda") %>% 
  select(Data, Kroki, Dystans_km, Osoba)

daneH_pom <- daneH %>%
  mutate(Data = as.Date(Date), Kroki = as.numeric(Steps), Dystans_km = as.numeric(Distance.meters.) / 1000, Osoba = "Hela") %>% 
  select(Data, Kroki, Dystans_km, Osoba)

daneS_pom <- daneS %>%
  mutate(Data = as.Date(Date), Kroki = as.numeric(Steps), Dystans_km = as.numeric(Distance.meters.) / 1000, Osoba = "Szymon") %>% 
  select(Data, Kroki, Dystans_km, Osoba)

dane_zbiorcze_kroki <- bind_rows(daneM_pom, daneH_pom, daneS_pom) %>%
  filter(Data >= "2025-12-07") %>%
  arrange(Data) %>%
  group_by(Osoba) %>%
  mutate(Suma_km = cumsum(Dystans_km), Suma_Krokow = cumsum(Kroki)) %>%
  ungroup()

kolory <- c("Magda" = "#133DF6", "Hela" = "#1E96FC", "Szymon" = "#F26430")

##  APLIKACJA:
ui <- navbarPage(
  title = "Nasza aktywność w ciągu miesiąca",
  theme = bs_theme(version = 5, bootswatch = "darkly", "navbar-bg" = "#FDECDB"),
  tabPanel("O projekcie",
           fluidPage(
             container = list(style = "max-width: 900px; margin-top: 30px;"),
             card(
               card_header(h3("O projekcie", class = "text-center")),
               card_body(
                 p("Witaj w aplikacji monitorującej aktywność fizyczną i przemieszczanie się grupy znajomych: ", 
                   strong("Heli, Magdy i Szymona"), "."),
                 p("Aplikacja przetwarza dane z GPS oraz liczników kroków, aby wizualizować nasze nawyki i styl życia na przestrzeni ostatniego miesiąca."),
                 hr(),
                 h4("Co zawiera ta aplikacja?"),
                 tags$ul(
                   tags$li(strong("Trasa aktywności:"), " Interaktywna mapa pozwalająca prześledzić dokładne ścieżki, którymi poruszaliśmy się wybranego dnia."),
                   tags$li(strong("Lokalizacje:"), " Analiza miejsc, w których bywamy najczęściej – od uczelni (MiNI, Gmach Główny) po domy i miejsca rekreacji."),
                   tags$li(strong("Rytm dobowy:"), " Wykresy intensywności ruchu, które pokazują, w jakich godzinach jesteśmy najbardziej aktywni."),
                   tags$li(strong("Statystyki dystansu:"), " Zbiorcze zestawienie przebytych kilometrów i zrobionych kroków w formie interaktywnego wykresu liniowego.")
                 ),
                 hr(),
                 p(em("Dane zbieraliśmy za pomocą aplikacji Strava oraz Pacer, pobieramy je z repozytorium na GitHub.")),
                 hr()
               )
             )
           )
  ),

  tabPanel("Trasa aktywności",
           sidebarLayout(
             sidebarPanel(
               h3('Trasy aktywności'),
               p('Opis lalallallalallalallla'),
               hr(),
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
             p('Opis lalallallalallalallla'),
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
               p('Opis lalallallalallalallla'),
               plotOutput("wykres_slupkowy", height = "500px")
             )
           )
           
  ),
  tabPanel("O której najczęściej się poruszaliśmy",
           fluidPage(
             h4("Intensywność ruchu w ciągu doby"),
             p("Wykres pokazuje liczbę zarejestrowanych punktów o prędkości > 2 km/h w podziale na godziny."),
             p('Opis lalallallalallalallla'),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("zakres_godzin", 
                             "Wybierz przedział godzin:",
                             min = 0, max = 23, 
                             value = c(0, 23), # Domyślnie zaznaczone od 0 do 23
                             step = 1)
               ),
               mainPanel(
                 card(
                   plotOutput("wykres_godzinowy", height = "600px")
                 )
               )
             )
           )
  ),
  tabPanel("Przebyte dystanse",
           fluidPage(
             h4("Przebyte dystanse (liczone w krokach i kilometrach)"),
             p('Ile kroków (lub kilometrów) każdy z nas przebył od początku do końca pomiaru?'),
             p('Dla porównania z Warszawy do Torunia jest 180km w linii prostej. Zatem każdy z nas mógłby tam dojść.'),
             card(
               card_body(
                 selectInput("wybor_widoku", "Wybierz metrykę:",
                             choices = c("Kilometry (suma)" = "km", 
                                         "Kroki (suma)" = "kroki"),
                             selected = "km")
               )
             ),
             plotlyOutput("wykres_kroki_km", height = "600px")
           )
  ),
  tabPanel("Cel kroków",
           fluidPage(
             h4("Realizacja dziennego celu (6000 kroków)"),
             p("Sprawdźmy, czy udało nam się zrealizować założony cel danego dnia."),
             card(
               card_body(
                 radioButtons("wybor_osoby_cel", "Wybierz osobę:",
                              choices = c("Magda", "Hela", "Szymon"),
                              selected = "Magda",
                              inline = TRUE)
               )
             ),

             card(
               plotlyOutput("wykres_cel_krokow", height = "600px")
             ),
             
             br(),

             plotOutput("pasek_procentowy", height = "100px")
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
        slice(seq(1, n(), by = 6)) %>%
        ungroup()
   
      pal <- colorFactor(palette =kolory, 
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
        palette = kolory, 
        domain = c("Hela", "Magda", "Szymon")
      )
      
      leaflet(data_zbiorcza) %>%
        addTiles() %>%
        addCircleMarkers(~long_approx, ~lat_approx, radius = ~sqrt(ilosc_wizyt) * 5, 
                         color = ~color_palette(osoba), stroke = TRUE, weight = 2, fillOpacity = 0.6,
                         popup = ~paste0("<b>Osoba:</b> ", osoba, "<br><b>Miejsce:</b> ", nazwa_wykres, "<br><b>Wizyt:</b> ", ilosc_wizyt)) %>% 
        addLegend("bottomright", pal = color_palette, values = c("Hela", "Magda", "Szymon"), title = "Osoba", opacity=1)
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
        scale_fill_manual(values = kolory)+
        theme(axis.text = element_text(color = "white"),
              axis.title = element_text(color = "white"),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              axis.text.x = element_text(size = 12), 
              axis.text.y = element_text(size = 13),
              strip.text = element_text(size = 18, face = "bold", color="white"),
              plot.background = element_rect(fill = "#222222", color = NA),
              panel.background = element_rect(fill = "#222222", color = NA),
              panel.grid.major = element_line(color = "#444444"),
              panel.grid.minor = element_blank())
    })
    
    output$wykres_godzinowy <- renderPlot({
      dane_filtrowane <- aktywnosc_godzinowa %>%
        filter(godzina >= input$zakres_godzin[1], 
               godzina <= input$zakres_godzin[2])
      
      ggplot(dane_filtrowane, aes(x = godzina, y = intensywnosc, color = osoba)) +
        geom_line(linewidth = 1.2) +
        geom_area(aes(fill = osoba), alpha = 0.1, position = "identity") +
        facet_wrap(~osoba, ncol = 1, scales = "free") +
        scale_x_continuous(breaks = 0:23) +
        scale_color_manual(values = kolory) +
        scale_fill_manual(values = kolory) +
        labs(x = "Godzina", y = "Liczba logów") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text = element_text(color = "white"),
              axis.title = element_text(color = "white"),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              axis.text.x = element_text(size = 13), 
              axis.text.y = element_text(size = 12),
              strip.text = element_text(size = 18, face = "bold", color="white"),
              panel.spacing = unit(2, "lines"),
              plot.background = element_rect(fill = "#222222", color = NA),
              panel.background = element_rect(fill = "#222222", color = NA),
              panel.grid.major = element_line(color = "#444444"),
              panel.grid.minor = element_blank())
    })
    
    output$wykres_kroki_km <- renderPlotly({
      if (input$wybor_widoku == "km") {
        p <- ggplot(dane_zbiorcze_kroki, aes(x = Data, y = Suma_km, color = Osoba)) +
          geom_line(size = 1) +
          geom_point(aes(text = paste0("<b>Osoba:</b> ", Osoba, "<br><b>Data:</b> ", Data, 
                                       "<br><b>Dziś:</b> ", round(Dystans_km, 2), " km",
                                       "<br><b>Suma:</b> ", round(Suma_km, 1), " km"))) +
          labs(title = "Suma przebytych kilometrów", y = "Kilometry")
      } else {
        p <- ggplot(dane_zbiorcze_kroki, aes(x = Data, y = Suma_Krokow, color = Osoba)) +
          geom_line(size = 1) +
          geom_point(aes(text = paste0("<b>Osoba:</b> ", Osoba, "<br><b>Data:</b> ", Data, 
                                       "<br><b>Dziś:</b> ", Kroki, " kroków",
                                       "<br><b>Suma:</b> ", format(Suma_Krokow, big.mark=" ")))) +
          labs(title = "Suma zrobionych kroków", y = "Liczba kroków")
      }
      
      p <- p + scale_color_manual(values = kolory) +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, size = 15, color="white"),
              axis.text = element_text(color = "white"),
              axis.title = element_text(color = "white"),
              legend.text = element_text(color = "white", size = 10),
              legend.title = element_text(color = "white", face = "bold"),
              plot.background = element_rect(fill = "#222222", color = NA),
              panel.background = element_rect(fill = "#222222", color = NA),
              panel.grid.major = element_line(color = "#444444"),
              panel.grid.minor = element_blank()
              ) +
        scale_y_continuous(labels = label_number(big.mark = " "))
      
      ggplotly(p, tooltip = "text")%>%
        layout(hoverlabel = list(bordercolor = "transparent",
                                 font = list(color = "white", size = 13)))
    })
    
    output$wykres_cel_krokow <- renderPlotly({
      wybrana_osoba <- input$wybor_osoby_cel
      dane_wykres <- dane_zbiorcze_kroki %>% filter(Osoba == wybrana_osoba)
      
      p <- ggplot(dane_wykres, aes(x = Data, y = Kroki)) +
        geom_segment(aes(x = Data, xend = Data, y = 0, yend = Kroki), color = "gray80") +
        geom_point(size = 3, aes(
          color = Kroki >= 6000,
          text = paste0("<b>Data:</b> ", Data, "<br><b>Liczba kroków:</b> ", Kroki, 
                        "<br><b>Status:</b> ", ifelse(Kroki >= 6000, "Cel zrealizowany", "Poniżej celu"))
        )) +
        geom_hline(yintercept = 6000, linetype = "dashed", color = "white", alpha = 0.5) +
        scale_color_manual(values = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71")) +
        scale_x_date(date_labels = "%b %d") +
        scale_y_continuous(breaks = c(0, 6000, 10000, 15000, 20000, 25000, 30000)) +
        theme_minimal(base_family = "Century Gothic") +
        labs(title = paste(wybrana_osoba), x = "", y = "Liczba kroków") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          plot.title = element_text(color = "white", hjust = 0.5, size = 16),
          legend.position = "none",
          plot.background = element_rect(fill = "#222222", color = NA),
          panel.background = element_rect(fill = "#222222", color = NA),
          panel.grid.major = element_line(color = "#444444"),
          panel.grid.minor = element_blank()
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white", bordercolor = "transparent", font = list(family = "Century Gothic", color = "black", size = 13)))
    })
    

    output$pasek_procentowy <- renderPlot({
      wybrana_osoba <- input$wybor_osoby_cel
      
      statystyki <- dane_zbiorcze_kroki %>%
        filter(Osoba == wybrana_osoba) %>%
        mutate(Status = ifelse(Kroki >= 6000, "Zrealizowany", "Niezrealizowany")) %>%
        group_by(Status) %>%
        summarise(liczba = n(), .groups = "drop") %>%
        mutate(proc = liczba / sum(liczba)) %>%
        arrange(desc(Status)) 
      
      ggplot(statystyki, aes(x = 1, y = proc, fill = Status)) +
        geom_col(position = "fill", width = 0.6) + 
        coord_flip() + 
        scale_fill_manual(values = c("Niezrealizowany" = "#e74c3c", "Zrealizowany" = "#2ecc71")) +
        geom_text(aes(label = paste0(Status, "\n", round(proc * 100), "%")), 
                  position = position_fill(vjust = 0.5), 
                  color = "white", size = 6, fontface = "bold", family = "Century Gothic") +
        theme_void() +
        theme(legend.position = "none")
    }, bg = "transparent") 
}
shinyApp(ui = ui, server = server)


