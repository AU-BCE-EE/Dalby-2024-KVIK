rm(list = ls())

library(data.table)
library(readxl)

dat_VS <- data.table(read_excel('../data/original/Til Anders Peter og Frederik 14122023.xlsx', sheet = 1, skip = 1))
dat_anim <- data.table(read_excel('../data/original/Til Anders Peter og Frederik 14122023.xlsx', sheet = 2, skip = 1))

#fix mismatch in names
dat_anim[DyrNavn == 'Smågrise', DyrNavn := 'Smågrise, 7,5-30 kg']
dat_anim[DyrNavn == 'Slagtesvin', DyrNavn := 'Slagtesvin, 30-100,0 kg']

dat <- merge.data.table(dat_VS, dat_anim)

#change some names
old_names <- c('FirstOfAntalDyr', 'Ton gødning ab dyr', 'Tørstof pct ab dyr')
new_names <- c('NDyr', 'GoednabDyr', 'TSabDyr')
setnames(dat, old = old_names, new = new_names)

NDyr_farestald <- dat[StaldID %in% c(64, 65) & !duplicated(StaldID), sum(NDyr)]
NDyr_løbestald <- dat[DyrID == 12 & !duplicated(StaldID) & !StaldID %in% c(64, 65), sum(NDyr)]

frac_farestald <- NDyr_farestald/(NDyr_løbestald + NDyr_farestald)
frac_løbestald <- 1- frac_farestald

#make new coloumn "NDyr_mod" which can be directly multiplied with excretions to get yearly excretion.
dat[, NDyr_mod := NDyr]
dat[StaldID %in% c(64, 65), NDyr_mod := NDyr * 1/frac_farestald]
dat[!StaldID %in% c(64, 65) & DyrID == 12, NDyr_mod := NDyr * 1/frac_løbestald]

#calculate yearly excretions on columns
cols <- c('NabDyr', 'TANabDyr', 'GoednabDyr' )
new_cols <- paste0('Tot', cols)
dat[, (new_cols) := .SD * NDyr_mod, .SDcols = cols]

# forsuring i ringkanalsystemer * udbredelse: dvs. hvis 2.8 % udbredelse af forsuring for malkekvæg ganges med denne faktor for ringkanaler, fordi det hele er koncentreret i ringkanalsystem fro malkekvæg
dat[StaldID %in% c(2,3,4,5,6,7,9,11,12,13,14,49) & GoedningsNavn == 'Gylle', sum(TotGoednabDyr)]/dat[StaldID %in% c(6,13) & GoedningsNavn == 'Gylle' & DyrID %in% c(1,2), sum(TotGoednabDyr)]

total_kvæg <- dat[StaldID %in% c(2,3,4,5,6,7,9,11,12,13,14,49), sum(TotGoednabDyr)]
total_svin <- dat[StaldID %in% c(8,10,15,18,19,20,21,22,46,47,60,62,63,64,65,72,73,78,79,80,81), sum(TotGoednabDyr)]