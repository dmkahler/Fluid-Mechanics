library(ggplot2)
library(dplyr)
library(broom)
library(readr)
library(tidyr)
library(lubridate)
library(latex2exp)

x <- read_csv("Lab04_vonKarman/vec20250412122224.csv", col_names = FALSE)
x <- rename(x, time=X2, u=X5, v=X6, w1=X7, w2=X8)
recordFrequency <- 60 # from configuration information

begin <- 10
finish <- 20
y <- x %>%
     filter(time > begin) %>%
     filter(time <= finish) %>%
     select(time, u, v, w1, w2)

x_mean = mean(y$u)
print(paste0("Mean streamwise velocity: ", x_mean, " m/s"))

ggplot(y) +
     geom_line(aes(x=time,y=v)) +
     ylim(c(-0.5, 0.5)) +
     labs(x = "Time (s)", y = "Velocity (m/s)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))

v_freq  <- Mod(fft(y$v))
freq <- (seq(0,(length(v_freq)-1))) / (finish - begin)
v_freq <- data.frame(freq, v_freq) %>%
     rename(Frequency = freq, `FFT` = v_freq)

ggplot(v_freq) +
     geom_line(aes(x=Frequency,y=`FFT`)) +
     xlim(c(0,10)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))

# FFT example
# Do not run
t <- seq(0,10, by=(1/60))
s <- sin(t*2*pi)
plot(t,s)
f <- Mod(fft(s))
plot(f, xlim=c(0,20))

t <- seq(0,10, by=(1/60))
s <- sin(t*2*pi*2.4)
plot(t,s)
f <- Mod(fft(s))
plot(f, xlim=c(0,100))



