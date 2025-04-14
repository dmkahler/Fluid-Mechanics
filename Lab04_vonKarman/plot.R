library(ggplot2)
library(dplyr)
library(broom)
library(readr)
library(tidyr)
library(lubridate)
library(latex2exp)

x <- read_csv("Lab04_vonKarman/vec20250412122224.csv", col_names = FALSE)
x <- rename(x, time=X2, u=X5, v=X6, w1=X7, w2=X8)

y <- x %>%
     filter(time>10) %>%
     filter(time<=20) %>%
     select(time, u, v, w1, w2)

x_mean = mean(y$u)

ggplot(y) +
     geom_line(aes(x=time,y=v)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     labs(x = "Time (s)", y = "Velocity (m/s)") +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
