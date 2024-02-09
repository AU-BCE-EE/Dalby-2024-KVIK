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

#
dyretype_goedning <- dat[, sum(TotGoednabDyr), by = 'DyreType']


#model dat
dat_model_pig <- data.table(t(read_excel('../model/Metanproduktion_Arrhenius_v8_30012024.xlsx')))
names(dat_model_pig) <- as.character(dat_model_pig[1, ])
dat_model_pig <- dat_model_pig[-c(1:2), ]

dat_model_cattle <- data.table(t(read_excel('../model/Metanproduktion_Arrhenius_v8_30012024.xlsx', sheet = 'Tabel_kvæg')))
names(dat_model_cattle) <- as.character(dat_model_cattle[1, ])
dat_model_cattle <- dat_model_cattle[-c(1:2), ]

dat_model <- rbind(dat_model_pig, dat_model_cattle, fill =T)

#fix names in model spreadsheet
old_names <- c('CH4-udledning stald, kg/t gylle ab dyr', 
               'CH4-udledning stald og for/afhent.tank, kg/t gylle ab dyr', 
               'CH4-udledning lager, kg/t gylle ab dyr',
               'CH4-udledning, afgasset gylle, kg/t gylle ab dyr',
               'CH4-produktion, biogasanlæg, kg CH4/t gylle ab dyr',
               'CH4-udledning stald, kg/t gylle ab stald', 
               'CH4-udledning stald og for/afhent.tank, kg/t gylle ab stald', 
               'CH4-udledning lager, kg/t gylle ab stald',
               'CH4-udledning, afgasset gylle, kg/t gylle ab stald',
               'CH4-produktion, biogasanlæg, inkl. halm, kg CH4/t gylle ab stald',
               'NH3-udledning stald tempkorr, kg-N/t gylle ab dyr',
               'NH3-udledning lager tempkorr, kg-N/t gylle ab dyr',
               'N2O-udledning direkte total, kg/t gylle ab dyr',
               'N2O-udledning indirekte total, kg/t gylle ab dyr'
               )

new_names <- c('CH4_dyr_stald', 'CH4_dyr_Stald_aft', 'CH4_dyr_lager', 'CH4_dyr_afg', 'CH4_dyr_biog', 
               'CH4_stald_stald', 'CH4_stald_Stald_aft', 'CH4_stald_lager', 'CH4_stald_afg', 'CH4_stald_biog', 
               'NH3_dyr_stald', 'NH3_dyr_lager', 'N2O_dyr_dir_tot', 'N2O_dyr_indir_tot'
               )

setnames(dat_model, old = old_names, new = new_names)

dat_model <- dat_model[, c((new_names), 'GoedningsID') := lapply(.SD, as.numeric), .SDcols = c(new_names, 'GoedningsID')][, c(..new_names, 'StaldID', 'GoedningsID','Scenarie')]

#rows with a dot in StaldID
.rows <- dat_model[grepl("\\.", StaldID)]

#create replacement rows by splitting StaldID
.rows_rpl <- do.call(rbind, lapply(1:.rows[,.N], function(x) {
     id <- unlist(strsplit(as.character(.rows[x, 'StaldID']), "\\."))
     if(any(grepl("\\D", id))) id[grepl("\\D", id)] <- gsub("\\D", "", id[grepl("\\D", id)])
     x <- rbindlist(replicate(length(id), .rows[x], simplify = FALSE))
     x[, StaldID := ..id]
    return(x)
  }
))

dat_model <- rbind(dat_model[!grepl("\\.", StaldID)], .rows_rpl)[, StaldID := as.numeric(StaldID)]

dat_merged <- merge.data.table(dat, dat_model, by = c('StaldID', 'GoedningsID'), all = T, allow.cartesian=TRUE)

fwrite(dat_merged, '../data/dat_merged.csv', row.names = F)




