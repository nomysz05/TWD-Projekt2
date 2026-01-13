library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(leaflet)
library(htmltools)

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

wszystkie_dane_surowe <- map_df(files_to_process, fetch_github_data)

rankingi_osobiste <- wszystkie_dane_surowe %>%
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
    osoba == "Hela" & loc_id == "52.204, 21.012" ~ "dom",
    osoba == "Hela" & loc_id == "52.222, 21.007" ~ "mini",
    osoba == "Hela" & loc_id == "52.216, 21.017" ~ "mikrus",
    osoba == "Hela" & loc_id == "61.317, 12.615" ~ "chatka w norwegii",
    osoba == "Hela" & loc_id == "52.221, 21.01"  ~ "GG",
    osoba == "Hela" & loc_id == "52.19, 21.018"  ~ "bar wierzbno",
    osoba == "Hela" & loc_id == "52.218, 20.984" ~ "Alcatraz",
    osoba == "Hela" & loc_id == "52.204, 21.013" ~ "dom",
    osoba == "Magda" & loc_id == "51.992, 21.059" ~ "dom",
    osoba == "Magda" & loc_id == "52.222, 21.007" ~ "GG",
    osoba == "Magda" & loc_id == "52.221, 21.009" ~ "mini",
    osoba == "Magda" & loc_id == "51.992, 21.06"  ~ "dom",
    osoba == "Magda" & loc_id == "51.247, 22.565" ~ "lublin",
    osoba == "Magda" & loc_id == "51.232, 22.567" ~ "lublin",
    osoba == "Magda" & loc_id == "50.138, 19.558" ~ "zepsuty pociąg",
    osoba == "Magda" & loc_id == "49.486, 21.613" ~ "Ropianka",
    osoba == "Szymon" & loc_id == "52.249, 21.088" ~ "dom",
    osoba == "Szymon" & loc_id == "52.249, 21.089" ~ "dom",
    osoba == "Szymon" & loc_id == "52.222, 21.007" ~ "mini",
    osoba == "Szymon" & loc_id == "52.213, 21.02"  ~ "siłownia",
    osoba == "Szymon" & loc_id == "52.213, 21.019" ~ "siłownia",
    osoba == "Szymon" & loc_id == "52.144, 21.005" ~ "ścianka",
    osoba == "Szymon" & loc_id == "52.13, 20.768"  ~ "dom dziewczyny",
    osoba == "Szymon" & loc_id == "46.43, 11.712"  ~ "wyjazd/włochy",
    TRUE ~ loc_id
  ))

color_palette <- colorFactor(palette = "Set1", domain = rankingi_osobiste$osoba)

mapa_zbiorcza <- leaflet(rankingi_osobiste) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long_approx, ~lat_approx,
    radius = ~sqrt(ilosc_wizyt) * 10, 
    color = ~color_palette(osoba),
    stroke = TRUE, weight = 2, fillOpacity = 0.6,
    popup = ~paste0("<b>Osoba:</b> ", osoba, "<br><b>Miejsce:</b> ", nazwa_wykres, "<br><b>Wizyt:</b> ", ilosc_wizyt)
  ) %>%
  addLegend("bottomright", pal = color_palette, values = ~osoba, title = "Osoba")

wykres_slupkowy <- rankingi_osobiste %>%
  group_by(osoba) %>%
  slice_max(order_by = ilosc_wizyt, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  # Tworzymy unikalną kombinację nazwy i osoby, aby sortowanie działało poprawnie
  mutate(nazwa_sort = factor(paste(osoba, nazwa_wykres, sep = "__"))) %>%
  mutate(nazwa_sort = reorder(nazwa_sort, ilosc_wizyt)) %>%
  ggplot(aes(x = nazwa_sort, y = ilosc_wizyt, fill = osoba)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~osoba, scales = "free", ncol = 1) +
  # Czyścimy nazwy osi X, usuwając dodany prefix osoby
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  labs(title = "najczęściej odwiedzane miejsca", x = "Miejsce", y = "Liczba wizyt") +
  theme_minimal() +
  scale_fill_manual(values = c("Magda" = "#E41A1C", "Hela" = "#377EB8", "Szymon" = "#4DAF4A"))

aktywnosc_godzinowa <- wszystkie_dane_surowe %>%
  filter(speed * 3.6 > 2) %>% 
  mutate(godzina = hour(timestamp)) %>%
  group_by(osoba, godzina) %>%
  summarise(intensywnosc = n(), .groups = "drop")

wykres_godzinowy <- ggplot(aktywnosc_godzinowa, aes(x = godzina, y = intensywnosc, color = osoba)) +
  geom_line(size = 1.2) +
  geom_area(aes(fill = osoba), alpha = 0.1, position = "identity") +
  facet_wrap(~osoba, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Intensywność ruchu w ciągu doby (v > 2 km/h)", x = "Godzina", y = "Liczba logów") +
  theme_minimal()

print(mapa_zbiorcza)
print(wykres_slupkowy)
print(wykres_godzinowy)