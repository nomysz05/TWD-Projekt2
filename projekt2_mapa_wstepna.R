library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(leaflet)

# --- 1. POBIERANIE DANYCH Z GITHUB ---
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
      mutate(osoba = file_info$owner)
  }, error = function(e) return(NULL))
}

# Łączymy wszystkie dane
wszystkie_punkty <- map_df(files_to_process, fetch_github_data)

# --- 2. FILTROWANIE (CO 10 ZAPIS) ---
dane_do_mapy <- wszystkie_punkty %>%
  group_by(osoba) %>%
  # Wybieramy co 10-ty wiersz dla każdej osoby
  slice(seq(1, n(), by = 10)) %>%
  ungroup()

# --- 3. RYSOWANIE MAPY WSZYSTKICH PUNKTÓW ---
pal <- colorFactor(palette = c("red", "blue", "black"), domain = dane_do_mapy$osoba)

mapa_sciezek <- leaflet(dane_do_mapy) %>%
  addTiles() %>%
  # Używamy addCircles dla wydajności przy dużej ilości punktów
  addCircles(
    lng = ~position_long, 
    lat = ~position_lat, 
    color = ~pal(osoba),
    radius = 5,           # Stały mały rozmiar punktu (metry)
    weight = 1, 
    fillOpacity = 0.5,
    group = ~osoba,       # Grupowanie umożliwia wyłączanie osób w menu
    popup = ~paste0("Osoba: ", osoba, "<br>Czas: ", timestamp)
  ) %>%
  addLegend(
    "bottomright", 
    pal = pal, 
    values = ~osoba, 
    title = "Trasy użytkowników",
    opacity = 1
  ) %>%
  # Dodajemy panel wyboru osób (warstw)
  addLayersControl(
    overlayGroups = unique(dane_do_mapy$osoba),
    options = layersControlOptions(collapsed = FALSE)
  )

# Wyświetlenie mapy
print(mapa_sciezek)