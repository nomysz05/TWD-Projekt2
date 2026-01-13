library(ggplot2)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(scales)

moj_font <- "Century Gothic"


daneM <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane%20magda/DailySteps_2026_01_13_1700.csv")
daneH <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane_hela/DailySteps_2026_01_13_1940.csv")
daneS <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/daneSzymon/PacerDataLogs/DailySteps_2026_01_13_1952.csv")


daneM_proc <- daneM %>%
  mutate(Data = as.Date(get("Date")), Kroki = as.numeric(get("Steps")), Osoba = "Magda") %>%
  select(Data, Kroki, Osoba)

daneH_proc <- daneH %>%
  mutate(Data = as.Date(get("Date")), Kroki = as.numeric(get("Steps")), Osoba = "Hela") %>%
  select(Data, Kroki, Osoba)

daneS_proc <- daneS %>%
  mutate(Data = as.Date(get("Date")), Kroki = as.numeric(get("Steps")), Osoba = "Szymon") %>%
  select(Data, Kroki, Osoba)


dane_do_wykresu <- bind_rows(daneM_proc, daneH_proc, daneS_proc) %>%
  filter(Data >= "2025-12-07") %>% 
  arrange(Data) %>%
  group_by(Osoba) %>%
  mutate(Suma_Krokow = cumsum(Kroki)) %>% 
  ungroup()

p <- ggplot(dane_do_wykresu, aes(x = Data, y = Suma_Krokow, color = Osoba)) +
  geom_line(size = 1) + 
  geom_point(size = 2, aes(text = paste0(
    "<b>Osoba:</b> ", Osoba, "<br>",
    "<b>Data:</b> ", Data, "<br>", 
    "<b>Suma kroków:</b> ", Suma_Krokow, "<br>",
    "<b>Kroki tego dnia:</b> ", Kroki))) +
  
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
  scale_color_manual(values = c("Magda" = "#2ecc71", "Hela" = "#e74c3c", "Szymon" = "#3498db")) +
  theme_bw(base_family = moj_font) + 
  labs(title = "Zsumowana liczba kroków (od 7 grudnia)",
       x = "Data",
       y = "Liczba kroków") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


wykres_interaktywny <- ggplotly(p, tooltip = "text") %>%
  layout(
    font = list(family = moj_font),
    hoverlabel = list(bgcolor = "rgba(255, 0, 0, 0.6)", 
      bordercolor = "transparent",
      font = list(family = moj_font, color = "white", size = 13) ))

wykres_interaktywny
