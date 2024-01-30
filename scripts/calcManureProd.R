rm(list = ls())

library(data.table)
library(openxlsx)

dat <- fread('../data/dat_merged.csv')

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

gylle <- dat[Scenarie == 'kontrol' & 'Gylle' %in% GoedningsNavn, .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
dybstrøelse <- dat[c('Dybstrøelse') %in% GoedningsNavn, .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
fastgødning <- dat[c('Fast gødning') %in% GoedningsNavn, .(TotGoednabDyr = sum(TotGoednabDyr)), by = c('StaldID')]
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
                                  TotGoednabDyr_tons_year = c(gylle[StaldID %in% toklimastald_smågrise_StaldID, sum(TotGoednabDyr)],
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
                                                    ajle[StaldID %in% kvæg_ajle_StaldID, sum(TotGoednabDyr)]))
                                                    
write.xlsx(TotGoednabDyr_table, '../output/TotGoedningabDyr.xlsx')
fwrite(TotGoednabDyr_table, '../output/TotGoedningabDyr.csv')


