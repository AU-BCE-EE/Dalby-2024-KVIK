rm(list = ls())

library(data.table)
library(readxl)

dat_VS <- data.table(read_excel('../data/original/Til Anders Peter og Frederik 14122023.xlsx', sheet = 1, skip = 1))
dat_anim <- data.table(read_excel('../data/original/Til Anders Peter og Frederik 14122023.xlsx', sheet = 2, skip = 1))

#do ID names match?
dat_anim[DyrNavn == 'Smågrise', DyrNavn := 'Smågrise, 7,5-30 kg']
dat_anim[DyrNavn == 'Slagtesvin', DyrNavn := 'Slagtesvin, 30-100,0 kg']

dat <- merge.data.table(dat_VS, dat_anim)

old_names <- c('FirstOfAntalDyr', 'Ton gødning ab dyr', 'Tørstof pct ab dyr')
new_names <- c('NDyr', 'GoednabDyr', 'TSabDyr')
setnames(dat, old = old_names, new = new_names)

NDyr_farestald <- dat[StaldID %in% c(64, 65) & !duplicated(StaldID), sum(NDyr)]
NDyr_løbestald <- dat[DyrID == 12 & !duplicated(StaldID) & !StaldID %in% c(64, 65), sum(NDyr)]

NDyr_farestald/(NDyr_løbestald + NDyr_farestald)
