library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(htmlwidgets)

moj_font <- "Century Gothic"
styl <- list(
  bgcolor = "rgba(255, 0, 0, 0.6)",
  bordercolor = "transparent",
  font = list(family = moj_font, color = "white", size = 13))


daneM <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane%20magda/DailySteps_2026_01_13_1700.csv")
daneH <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane_hela/DailySteps_2026_01_13_1940.csv")
daneS <- read.csv("https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/daneSzymon/PacerDataLogs/DailySteps_2026_01_13_1952.csv")

daneM <- daneM %>%
  mutate(
    Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Osoba = "Magda") %>% arrange(Data)

daneH <- daneH %>%
  mutate(
    Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Osoba = "Hela") %>% arrange(Data)

daneS <- daneS %>%
  mutate(
    Data = as.Date(get("Date")),
    Kroki = as.numeric(get("Steps")),
    Osoba = "Szymon") %>% arrange(Data)


cel <- 6000

p2 <- ggplot(daneM, aes(x = Data, y = Kroki)) +
  geom_segment(aes(x = Data, xend = Data, y = 0, yend = Kroki), color = "gray") +
  geom_point(size = 3, aes(color = Kroki >= cel, 
                           text = paste0(
                             "<b>Kroki:</b> ", Kroki, "<br>",
                             "<b>Status:</b> ", ifelse(Kroki >= cel, "Cel zrealizowany!", "Poniżej celu")))) +
  geom_hline(yintercept = cel, linetype = "dashed", color = "red", alpha = 0.4) +
  scale_color_manual(values = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71"), 
                     labels = c("Poniżej celu", "Cel osiągnięty")) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%m") +
  theme_bw(base_family = moj_font) +
  labs(title = "Realizacja dziennego celu", x = "Data", y = "Kroki", color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wykres2_html <- ggplotly(p2, tooltip = "text") %>%
  layout(font = list(family = moj_font), hoverlabel = styl)
wykres2_html
saveWidget(wykres2_html, "wykres_cel_lizakowy.html")
