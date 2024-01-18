rm(list = ls())

library(readxl)
library(lubridate)
library(dplyr)

#import Vechi 2023 et al. data
dat <- read_excel('../data/original/Vechi_dat.xlsx')
dat$date <- ymd(dat$date)
dat$month <- month(dat$date)
dat$doy <- as.numeric(as.Date(dat$date) - as.Date("2023-01-01"))
dat$fill <- dat$m3/dat$capacity

dat.count <- data.frame(table(dat$month[!is.na(dat$temp) & !grepl('D', dat$Tank)]) )

rem <- as.numeric(dat.count$Var1[which(dat.count$Freq <= 1)]) # June only one temp -> interpolate instead. 

dat.mod <- dat %>% filter(!grepl('D',Tank), !is.na(temp)) %>% group_by(month) %>% 
  summarise(doy = mean(doy, na.rm = T), 
            temp = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T),
            fill = mean(fill, na.rm = T)
            )

# mean doy of each month
mean_month_doy <- c(15, 45, 76, 106, 137, 167, 198, 228, 259, 289, 320, 350)
dat.mod$mean_month_doy <- mean_month_doy

# interpolate but exclude the June measurement.
dat.mod$temp_mean_month_doy <- approx(x = dat.mod$doy[-rem], y = dat.mod$temp[-rem], xout = dat.mod$mean_month_doy)$y

int_dat <- rbind(dat.mod[c(11:12),],dat.mod[c(1:2),])  
int_dat$doy_cor <- c(int_dat$doy[1:2] - 365, int_dat$doy[3:4]) - min(c(int_dat$doy[1:2] - 365, int_dat$doy[3:4]))
int_dat$mean_month_doy_cor <- c(int_dat$mean_month_doy[1:2] - 365, int_dat$mean_month_doy[3:4]) - min(c(int_dat$mean_month_doy[1:2] - 365, int_dat$mean_month_doy[3:4]))

int_dat$temp_mean_month_doy <- approx(int_dat$doy_cor, int_dat$temp, xout = int_dat$mean_month_doy_cor)$y

dat.mod$temp_mean_month_doy[1] <- int_dat$temp_mean_month_doy[3]
dat.mod$temp_mean_month_doy[12] <- int_dat$temp_mean_month_doy[2]

dat.out <- dat.mod[, (ncol(dat.mod)-1):ncol(dat.mod)]

write.csv(dat.out, "../data/temp_dat.csv", row.names = F)

plot(dat.out$mean_month_doy, dat.out$temp_mean_month_doy)

mean(dat.out$temp_mean_month_doy)



