library(ggplot2)
library(dplyr)
library(plotly)
library(htmlwidgets)


czcionka <- "Century Gothic"
daneM <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane%20magda/DailySteps_2026_01_13_1700.csv")
daneH <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane_hela/DailySteps_2026_01_13_1940.csv")
daneS <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/daneSzymon/PacerDataLogs/DailySteps_2026_01_13_1952.csv")


daneM_proc <- daneM %>%
  mutate(Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Dystans_km = as.numeric(get("Distance.meters.")) / 1000, 
    Osoba = "Magda") %>% select(Data, Kroki, Dystans_km, Osoba)

daneH_proc <- daneH %>%
  mutate(
    Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Dystans_km = as.numeric(get("Distance.meters.")) / 1000,
    Osoba = "Hela") %>% select(Data, Kroki, Dystans_km, Osoba)

daneS_proc <- daneS %>%
  mutate(
    Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Dystans_km = as.numeric(get("Distance.meters.")) / 1000,
    Osoba = "Szymon") %>% select(Data, Kroki, Dystans_km, Osoba)


dane <- bind_rows(daneM_proc, daneH_proc, daneS_proc) %>%
  filter(Data >= "2025-12-07") %>%
  arrange(Data) %>%
  group_by(Osoba) %>%
  mutate(Suma_km = cumsum(Dystans_km)) %>%
  ungroup()

p <- ggplot(dane, aes(x = Data, y = Suma_km, color = Osoba, fill = Osoba)) +
  geom_area(alpha = 0.1, position = "identity", show.legend = FALSE) +
  geom_line(size = 1) +
  
  geom_point(size = 1.5, aes(text = paste0(
    "<b>Osoba:</b> ", Osoba, "<br>",
    "<b>Data:</b> ", Data, "<br>",
    "<b>Łącznie przebyto:</b> ", round(Suma_km, 1), " km<br>",
    "<b>Tego dnia:</b> ", round(Dystans_km, 2), " km"))) +
  
  scale_color_manual(values = c("Magda" = "#2ecc71", "Hela" = "#e74c3c", "Szymon" = "#3498db")) +
  scale_fill_manual(values = c("Magda" = "#2ecc71", "Hela" = "#e74c3c", "Szymon" = "#3498db")) +

  theme_bw(base_family = czcionka) +
  labs(title = "Skumulowana suma przebytych kilometrów",
       x = "Data",
       y = "Całkowity dystans (km)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wykres_kilometry <- ggplotly(p, tooltip = "text") %>%
  layout(font = list(family = czcionka),
    hoverlabel = list(
      bordercolor = "transparent",
      font = list(family = czcionka, color = "white", size = 13)))

wykres_kilometry

