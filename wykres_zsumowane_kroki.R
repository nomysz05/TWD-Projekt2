library(ggplot2)
library(dplyr)
library(scales)

daneM <- read.csv("C:/Users/klemi/Documents/studia/sem3/twd/projekt2/PacerDataLogs/DailySteps_2025_12_16_2052.csv")
#daneS <- read.csv("C:/Users/klemi/Documents/studia/sem3/twd/projekt2/daneSzymon/DailySteps_2025_12_16_1953.csv")

daneM <- daneM %>%
  mutate(Data = as.Date(get("Date")),Kroki = as.numeric(get("Steps"))) %>%
  arrange(Data) %>% 
  mutate(Suma_Krokow = cumsum(Kroki))

ggplot(daneM, aes(x = Data, y = Suma_Krokow)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "blue", size = 3) +
  scale_x_date(date_breaks = "1 day",
    date_labels = "%Y-%m-%d") +
  theme_bw() + 
  labs(title = "Zsumowana liczba kroków",
    x = "Data",
    y = "liczba kroków") 
