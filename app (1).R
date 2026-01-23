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
    osoba == "Szymon" & loc_id == "46.495, 11.751"  ~ "wyjazd/włochy",
    osoba == "Szymon" & loc_id == "46.477, 11.744"  ~ "wyjazd/włochy",
    osoba == "Szymon" & loc_id == "46.476, 11.771"  ~ "wyjazd/włochy",
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
  theme = bs_theme(version = 5, bootswatch = "darkly"),
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
                   tags$li(strong("Statystyki dystansu:"), " Zbiorcze zestawienie przebytych kilometrów i zrobionych kroków w formie interaktywnego wykresu liniowego."),
                   tags$li(strong("Cel kroków:"), " Monitoring realizacji dziennego wyzwania 6000 kroków wraz z podsumowaniem skuteczności dla każdego z nas.")
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
               p('Ta sekcja pozwala na wizualizację tras przebytych przez nas każdego dnia. Dzięki danym GPS możemy zobaczyć, jakimi ścieżkami poruszaliśmy się po mieście lub w trakcie wyjazdów.'),
               hr(),
               h4("Filtry"),
               dateRangeInput("data", 
                              "Wybierz przedział dat:", 
                              start = "2025-12-03",
                              end = "2025-12-03",
                              min = "2025-12-03", 
                              max = "2026-01-13",
                              separator = " do ",
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
             p('Poniższa mapa i wykresy przedstawiają naszą "geograficzną bazę". Wielkość punktów na mapie odpowiada liczbie wizyt w danej lokalizacji. Możesz tu zaobserwować nasze główne punkty zainteresowań: uczelnię, domy oraz miejsca, w których spędzamy czas wolny.'),
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
               card_header(h4("Ranking ulubionych lokalizacji")),
               p('Zestawienie miejsc, w których system GPS najczęściej rejestrował naszą obecność powyżej 30 minut.'),
               plotlyOutput("wykres_slupkowy", height = "500px")
             )
           )
           
  ),
  tabPanel("O której najczęściej się poruszaliśmy",
           fluidPage(
             h4("Intensywność ruchu w ciągu doby"),
             p("Poniższy wykres prezentuje nasz rytm dobowy. Pokazuje on procentowy udział aktywności (przemieszczania się z prędkością powyżej 2 km/h) w konkretnych godzinach. Pozwala to określić, kto z nas jest rannym ptaszkiem, a kto preferuje wieczorne spacery lub powroty."),
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
                   plotlyOutput("wykres_godzinowy", height = "600px")
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
    # 1. Filtrowanie danych na podstawie wybranego zakresu
    data_filtered <- wszystkie_punkty %>%
      filter(osoba %in% input$osoba,
             as.Date(timestamp) >= input$data[1],
             as.Date(timestamp) <= input$data[2])
    
    # Sprawdzenie czy są dane
    validate(
      need(nrow(data_filtered) > 0, "Brak zarejestrowanych tras w tym przedziale czasowym.")
    )
    
    # 2. Próbkowanie punktów (żeby mapa działała płynnie)
    data_plot <- data_filtered %>% 
      group_by(osoba) %>%
      slice(seq(1, n(), by = 10)) %>% # Zwiększyłem skok do 10 dla lepszej wydajności przy zakresach
      ungroup()
    
    pal <- colorFactor(palette = kolory, 
                       domain = c("Hela", "Magda", "Szymon"))
    
    # 3. Tworzenie mapy z automatycznym centrowaniem
    leaflet(data_plot) %>%
      addTiles() %>%
      # Ta linia odpowiada za przybliżenie mapy do aktualnie przefiltrowanych punktów:
      fitBounds(lng1 = min(data_plot$position_long), lat1 = min(data_plot$position_lat),
                lng2 = max(data_plot$position_long), lat2 = max(data_plot$position_lat)) %>%
      addCircles(~position_long, ~position_lat, 
                 color = ~pal(osoba), 
                 radius = 5, weight = 2,
                 fillOpacity = 0.6, 
                 popup = ~paste0("Osoba: ", osoba, "<br>Godzina: ", format(timestamp, "%H:%M:%S"))) %>%
      addLegend("bottomright", pal = pal, values = c("Hela", "Magda", "Szymon"), 
                title = "Osoba", opacity = 1)
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
  
  output$wykres_slupkowy <- renderPlotly({
    dane_do_wykresu <- rankingi_osobiste %>%
      filter(nazwa_wykres != loc_id) %>%
      group_by(osoba, nazwa_wykres) %>%
      summarise(ilosc_wizyt = sum(ilosc_wizyt), .groups = "drop") %>%
      group_by(osoba) %>%
      slice_max(order_by = ilosc_wizyt, n = 6, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        nazwa_sort = reorder(paste(osoba, nazwa_wykres, sep = "___"), ilosc_wizyt),
        label_text = paste0("<b>Miejsce:</b> ", nazwa_wykres, "<br>",
                            "<b>Liczba wizyt:</b> ", ilosc_wizyt)
      )
    
    p <- ggplot(dane_do_wykresu, aes(x = nazwa_sort, y = ilosc_wizyt, fill = osoba, text = label_text)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~osoba, scales = "free_y", ncol = 1) +
      scale_x_discrete(labels = function(x) sub(".*___", "", x)) +
      scale_fill_manual(values = kolory) +
      coord_flip() +
      labs(x = "Miejsce", y = "Liczba wizyt") +
      theme_minimal() +
      theme(
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        strip.text = element_text(size = 14, face = "bold", color = "white"),
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major = element_line(color = "#444444"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 100, r = 20, t = 40, b = 40), 
        hoverlabel = list(bgcolor = "#333333", font = list(color = "white"))
      )
  })
  
  output$wykres_godzinowy <- renderPlotly({
    
    dane_procentowe <- aktywnosc_godzinowa %>%
      group_by(osoba) %>%
      tidyr::complete(godzina = 0:23, fill = list(intensywnosc = 0)) %>% 
      mutate(total_osoba = sum(intensywnosc, na.rm = TRUE)) %>% 
      mutate(
        procent_udzialu = ifelse(total_osoba > 0, (intensywnosc / total_osoba) * 100, 0)
      ) %>%
      ungroup() %>%
      filter(godzina >= input$zakres_godzin[1], 
             godzina <= input$zakres_godzin[2]) %>%
      mutate(
        label_text = paste0(
          "<b>Osoba:</b> ", osoba, "<br>",
          "<b>Godzina:</b> ", godzina, ":00<br>",
          "<b>Udział:</b> ", round(procent_udzialu, 2), "%<br>"
        )
      )
    
    p <- ggplot(dane_procentowe, aes(x = godzina, y = procent_udzialu, color = osoba, group = osoba, text = label_text)) +
      geom_line(linewidth = 1) +
      geom_area(aes(fill = osoba), alpha = 0.2, position = "identity") +
      facet_wrap(~osoba, ncol = 1) + 
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(labels = scales::unit_format(unit = "%")) + 
      scale_color_manual(values = kolory) +
      scale_fill_manual(values = kolory) +
      labs(
        x = "Godzina", 
        y = "% całkowitej aktywności"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#222222", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        strip.text = element_text(size = 14, face = "bold", color = "white"),
        panel.grid.major = element_line(color = "#444444"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 60, b = 80, l = 60, r = 20),
        hoverlabel = list(bgcolor = "#333333", font = list(color = "white")),
        xaxis = list(
          title = list(font = list(color = "white"), standoff = 20),
          tickfont = list(color = "white")
        ),
        yaxis = list(
          title = list(font = list(color = "white")),
          tickfont = list(color = "white")
        )
      )
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
      theme_minimal() +
      labs(title = paste("Dzienny cel kroków -", wybrana_osoba), x = "", y = "Liczba kroków") +
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
      layout(hoverlabel = list(bgcolor = "white", bordercolor = "transparent", font = list(color = "black", size = 13)))
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
                color = "white", size = 6, fontface = "bold") +
      theme_void() +
      theme(legend.position = "none")
  }, bg = "transparent") 
}

shinyApp(ui = ui, server = server)
