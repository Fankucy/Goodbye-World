library(tidyverse)
library(nycflights13)
jan1 <- filter(flights, month == 1, day == 1)
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
longest <- arrange(flights, desc(distance))
shortest <- arrange(flights, distance)

