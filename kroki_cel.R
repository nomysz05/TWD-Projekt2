library(ggplot2)
library(dplyr)
library(plotly)


font <- "Century Gothic"
cel_krokow <- 6000
dane <- "https://raw.githubusercontent.com/nomysz05/TWD-Projekt2/refs/heads/main/dane%20magda/DailySteps_2026_01_13_1700.csv"

dane <- read.csv(dane)

dane_magda <- dane %>%
  mutate(
    Data = as.Date(get("Date")), 
    Kroki = as.numeric(get("Steps"))) %>%
  filter(Data >= "2025-12-07") %>%
  arrange(Data)

p <- ggplot(dane_magda, aes(x = Data, y = Kroki)) +
  geom_segment(aes(x = Data, xend = Data, y = 0, yend = Kroki), color = "gray80") +

  geom_point(size = 3, aes(
    color = Kroki >= cel_krokow,
    text = paste0(
      "<b>Data:</b> ", Data, "<br>",
      "<b>Liczba kroków:</b> ", Kroki, "<br>",
      "<b>Status:</b> ", ifelse(Kroki >= cel_krokow, "Cel zrealizowany", "Poniżej celu")))) +
  
  geom_hline(yintercept = cel_krokow, linetype = "dashed", color = "black", alpha = 0.3) +
  scale_color_manual(values = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71")) +
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(breaks = c(0, cel_krokow, 10000, 15000, 20000, 25000, 30000)) +
  theme_bw(base_family = font) +
  labs(title = "Magda - Realizacja celu", x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")

ggplotly(p, tooltip = "text") %>%
  layout(
    font = list(family = font),
    hoverlabel = list(bgcolor = "white", font = list(family = font, size = 13)))
