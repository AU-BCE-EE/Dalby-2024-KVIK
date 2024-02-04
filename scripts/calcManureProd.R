rm(list = ls())

library(data.table)
library(openxlsx)
library(readxl)

dat <- fread('../data/dat_merged.csv')

Goedn_dyretype <- dat[!duplicated(dat[, .(GoedningsNavn, StaldID, DyrNavn)]), .(TotGoednabDyr = sum(TotGoednabDyr)/1000), by = 'DyreType']

## SVIN
# gylle systemer
toklimastald_smågrise_StaldID <- c(20)
spalter_smågrise_StaldID <- c(46)
spalter_33_67_slagtesvin_StaldID <- c(47)
spalter_50_75_slagtesvin_StaldID <- c(72, 19)
spalter_25_50_slagtesvin_StaldID <- c(73)
løs_individuel_søer_StaldID <- c(60, 63, 8, 10, 80, 79)
farestald_delvis_spalte_StaldID <- c(64)
farestald_fuldspalte_StaldID <- c(65)

# dybstrøelse
svin_dybstrøelse_StaldID <- c(8, 10, 15, 19, 21, 79)

# fast gødning
svin_fastgødning_StaldID <- c(18, 62)

# ajle
svin_ajle_StaldID <- c(18, 62)

# ude
svin_ude_StaldID <- c(22, 78, 81) 

## KVÆG
# gylle systemer
kvæg_ringkanal_StaldID <- c(6, 13) 
kvæg_fast_skrab_StaldID <- c(5, 11)
kvæg_spalter_skrab_StaldID <- c(7, 14)
kvæg_hæld_fast_skrab_StaldID <- c(49)
kvæg_andre_hyppig_StaldID <- c(2, 4) # 2 is spaltegulvbokse, what is that? 

# dybstrøelse
kvæg_dybstrøelse_StaldID <- c(9, 11, 12, 13, 14) # 

# fast gødning
kvæg_fastgødning_StaldID <- c(3)

# ajle
kvæg_ajle_StaldID <- c(3)

gylle <- dat[GoedningsNavn == 'Gylle' & (Scenarie == 'kontrol'|Scenarie == ""| is.na(Scenarie)), .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID','GoedningsNavn')]
dybstrøelse <- dat[grepl('Dybstrøelse', GoedningsNavn), .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
fastgødning <- dat[grepl('Fast gødning', GoedningsNavn), .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
ajle <- dat[GoedningsNavn == 'Ajle', .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
ude <- dat[GoedningsNavn == 'Ude', .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]

TotGoednabDyr_table <- data.table(model_gruppe = c('toklimastald_smågrise',
                                                'spalter_smågrise',
                                                'spalter_33_67_slagtesvin',
                                                'spalter_50_75_slagtesvin',
                                                'spalter_25_50_slagtesvin',
                                                'løs_individuel_søer',
                                                'farestald_delvis_spalte',
                                                'farestald_fuldspalte',
                                                'svin_dybstrøelse',
                                                'svin_fastgødning',
                                                'svin_ajle',
                                                'svin_ude',
                                                'kvæg_ringkanal',
                                                'kvæg_fast_skrab',
                                                'kvæg_spalter_skrab',
                                                'kvæg_hæld_fast_skrab',
                                                'kvæg_andre_hyppig',
                                                'kvæg_dybstrøelse',
                                                'kvæg_fastgødning',
                                                'kvæg_ajle'),
                                  DyreType = c('smågrise',
                                               'smågrise',
                                               'slagtesvin',
                                               'slagtesvin',
                                               'slagtesvin',
                                               'søer',
                                               'søer',
                                               'søer',
                                               'svin',
                                               'svin',
                                               'svin',
                                               'svin',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg',
                                               'kvæg'),
                                  TotGoednabDyr_kt_year = c(gylle[StaldID %in% toklimastald_smågrise_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% spalter_smågrise_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% spalter_33_67_slagtesvin_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% spalter_50_75_slagtesvin_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% spalter_25_50_slagtesvin_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% løs_individuel_søer_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% farestald_delvis_spalte_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% farestald_fuldspalte_StaldID, sum(TotGoednabDyr)],
                                                    dybstrøelse[StaldID %in% svin_dybstrøelse_StaldID, sum(TotGoednabDyr)],
                                                    fastgødning[StaldID %in% svin_fastgødning_StaldID, sum(TotGoednabDyr)],
                                                    ajle[StaldID %in% svin_ajle_StaldID, sum(TotGoednabDyr)],
                                                    ude[StaldID %in% svin_ude_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% kvæg_ringkanal_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% kvæg_fast_skrab_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% kvæg_spalter_skrab_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% kvæg_hæld_fast_skrab_StaldID, sum(TotGoednabDyr)],
                                                    gylle[StaldID %in% kvæg_andre_hyppig_StaldID, sum(TotGoednabDyr)],
                                                    dybstrøelse[StaldID %in% kvæg_dybstrøelse_StaldID, sum(TotGoednabDyr)],
                                                    fastgødning[StaldID %in% kvæg_fastgødning_StaldID, sum(TotGoednabDyr)],
                                                    ajle[StaldID %in% kvæg_ajle_StaldID, sum(TotGoednabDyr)])/1000)


TotGoednabDyr_table[grepl('svin|smågrise|søer|slagtesvin', DyreType), Dyr := 'svin']
TotGoednabDyr_table[is.na(Dyr), Dyr := 'kvæg']
TotGoednabDyr_table <- merge.data.table(TotGoednabDyr_table, Goedn_dyretype, all.x = T)

TotGoednabDyr_table[, udbredelse_pr_dyr := TotGoednabDyr_kt_year/sum(TotGoednabDyr_kt_year) * 100, by= 'Dyr']
TotGoednabDyr_table[, udbredelse_pr_dyretype := TotGoednabDyr_kt_year/TotGoednabDyr * 100, by= 'DyreType']

ab_lager <- setDT(read_excel('../data/ab_lager.xlsx'))[
  , lager_faktor := ab_lager/ab_dyr][, .(model_gruppe, lager_faktor)]

TotGoednabDyr_table <- merge.data.table(TotGoednabDyr_table, ab_lager, by = 'model_gruppe', all.x =  T)
TotGoednabDyr_table <- TotGoednabDyr_table[, TotGoednabLager_kt_year := TotGoednabDyr_kt_year * lager_faktor][order(-DyreType)]
                                                    
# manure is kt produced per year                                                    
write.xlsx(TotGoednabDyr_table, '../output/TotGoedningabDyr.xlsx')



