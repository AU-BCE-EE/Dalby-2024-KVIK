rm(list = ls())

library(data.table)
library(openxlsx)

dat <- fread('../data/dat_merged.csv')

CO2_eq <- c(CH4 = 28, N2O = 298)

## SVIN
# gylle systemer
toklimastald_smågrise <- c(20)
spalter_smågrise <- c(46)
spalter_33_67_slagtesvin <- c(47)
spalter_50_75_slagtesvin <- c(72, 19)
spalter_25_50_slagtesvin <- c(73)
løs_individuel_søer <- c(60, 63, 8, 10, 80, 79)
farestald_delvis_spalte <- c(64)
farestald_fuldspalte <- c(65)

## KVÆG
# gylle systemer
kvæg_ringkanal <- c(6, 13) 
kvæg_fast_skrab <- c(5, 11)
kvæg_spalter_skrab <- c(7, 14)
kvæg_hæld_fast_skrab <- c(49)
kvæg_andre_hyppig <- c(2, 4) # 2 is spaltegulvbokse, what is that? 

model_gruppe_navne <- c('toklimastald_smågrise', 
                     'spalter_smågrise',
                     'spalter_33_67_slagtesvin',
                     'spalter_50_75_slagtesvin',
                     'spalter_25_50_slagtesvin',
                     'løs_individuel_søer',
                     'farestald_delvis_spalte',
                     'farestald_fuldspalte',
                     'kvæg_ringkanal',
                     'kvæg_fast_skrab',
                     'kvæg_spalter_skrab',
                     'kvæg_hæld_fast_skrab',
                     'kvæg_andre_hyppig')


cols <- c('CH4_dyr_stald', 'CH4_dyr_lager', 'NH3_dyr_stald', 'NH3_dyr_lager')

tot_cols <- paste0('tot', cols)

# Initialize the total columns with 0
dat <- dat[, (tot_cols) := 0][!duplicated(dat)]

biogas <- copy(dat)[Scenarie == 'kontrol'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg)][
           , Scenarie := 'biogas']
hyppig_biogas <- copy(dat)[Scenarie == 'ugentlig' | Scenarie == 'hyppig'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg)][
           , Scenarie := 'hyppig_biogas']

køling_biogas <- copy(dat)[Scenarie == 'køling'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg)][
           , Scenarie := 'køling_biogas']

dat <- rbind(dat, biogas, hyppig_biogas, køling_biogas)

# Multiply each column in 'cols' by 1 (no change), emis units in kg CH4/m3/year or kg CH4/year, kg CO2 eq/m3/year, kg CO2 eq/year
emis<- dat[, (cols) := lapply(.SD, function(x) x * 1), .SDcols = cols, by = c('StaldID', 'Scenarie', 'GoedningsNavn')][
  , (tot_cols) := lapply(.SD, function(x) x * TotGoednabDyr), .SDcols = cols, by = c('StaldID', 'Scenarie')][
    , ":="(CH4_dyr_tot = CH4_dyr_stald + CH4_dyr_lager,
           NH3_dyr_tot = NH3_dyr_stald + NH3_dyr_lager)][
             , CO2_eq := CH4_dyr_tot * ..CO2_eq[['CH4']] + NH3_dyr_tot * 0.01 * ..CO2_eq[['N2O']]][
               , ":="(totCH4_dyr_tot = totCH4_dyr_stald + totCH4_dyr_lager,
                      totNH3_dyr_tot = totNH3_dyr_stald + totNH3_dyr_lager)][
                        , totCO2_eq := totCH4_dyr_tot * ..CO2_eq[['CH4']] + totNH3_dyr_tot * 0.01 * ..CO2_eq[['N2O']]]

emis[, model_gruppe := 'char']

for(i in model_gruppe_navne){
  emis[StaldID %in% eval(parse(text = i)), model_gruppe := i]
}

emis_summary <- emis[, .(CH4_dyr_stald = unique(CH4_dyr_stald), 
                         CH4_dyr_lager = unique(CH4_dyr_lager),
                         CH4_dyr_tot = unique(CH4_dyr_tot),
                         NH3_dyr_tot = unique(NH3_dyr_tot),
                         totCO2_eq = sum(totCO2_eq)), by = c('Scenarie', 'model_gruppe')]

TotGoednabDyr <- fread('../output/TotGoedningabDyr.csv')
Teknologi <- setDT(read_excel('../data/teknologi_udbredelse.xlsx'))

out <- merge.data.table(emis_summary, TotGoednabDyr)[
  , ":="(udbredelse = 0,
         potentiale = 100)][
           model_gruppe == 'løs_individuel_søer' & (Scenarie == 'ugentlig' | Scenarie == 'linespil'), udbredelse := 40]

for(i in Teknologi[, model_gruppe]){
  out[model_gruppe == i & Scenarie == 'køling', udbredelse := ..Teknologi[model_gruppe == i, køling]]
  out[model_gruppe == i & Scenarie == 'forsuring', udbredelse := ..Teknologi[model_gruppe == i, forsuring]]
  out[model_gruppe == i & Scenarie == 'biogas', udbredelse := ..Teknologi[model_gruppe == i, biogas]]
}

out <- out[, totCO2_eq_pot := totCO2_eq * (potentiale-udbredelse)/100][order(Scenarie)][Scenarie != ""]



