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


cols <- c('CH4_dyr_stald', 'CH4_dyr_lager', 'CH4_dyr_biog', 'NH3_dyr_stald', 'NH3_dyr_lager','N2O_dyr_tot')

tot_cols <- paste0('tot', cols)

# Initialize the total columns with 0
dat <- dat[, (tot_cols) := 0][!duplicated(dat)]

# if biogas is used change the CH4 produktion to include emission from prestorage tank and change storage to digestate emission.
# also change N2O emission (based on national inventory report 2023, p431)
biogas <- copy(dat)[Scenarie == 'kontrol'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_tot = N2O_dyr_tot * 0.0006/0.00475)][
           , Scenarie := 'biogas']
ugentlig_biogas <- copy(dat)[Scenarie == 'ugentlig'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_tot = N2O_dyr_tot * 0.0006/0.00475)][
           , Scenarie := 'ugentlig_biogas']

køling_biogas <- copy(dat)[Scenarie == 'køling'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_tot = N2O_dyr_tot * 0.0006/0.00475)][
           , Scenarie := 'køling_biogas']

dat <- rbind(dat, biogas, ugentlig_biogas, køling_biogas)

# Multiply each column in 'cols' by 1 (no change), emis units in kg CH4/m3/year or kg CH4/year, kg CO2 eq/m3/year, kg CO2 eq/year
# totCO2_eq_tot is in kt CO2 eq (so kg CO2 eq multiplied by 10^6)
emis <- dat[, (cols) := lapply(.SD, function(x) x * 1), .SDcols = cols, by = c('StaldID', 'Scenarie', 'GoedningsNavn')][
  , (tot_cols) := lapply(.SD, function(x) x * TotGoednabDyr), .SDcols = cols, by = c('StaldID', 'Scenarie')][
    , ":="(CH4_dyr_tot = CH4_dyr_stald + CH4_dyr_lager,
           NH3_dyr_tot = NH3_dyr_stald + NH3_dyr_lager)][
             , CO2_eq_tot := CH4_dyr_tot * ..CO2_eq[['CH4']] + NH3_dyr_tot * 0.01 * 44/28 * ..CO2_eq[['N2O']] + N2O_dyr_tot * ..CO2_eq[['N2O']]][
               , ":="(totCH4_dyr_tot = totCH4_dyr_stald + totCH4_dyr_lager,
                      totNH3_dyr_tot = totNH3_dyr_stald + totNH3_dyr_lager)][
                        , totCO2_eq_tot := (totCH4_dyr_tot * ..CO2_eq[['CH4']] + 
                          totNH3_dyr_tot * 0.01 * 44/28 * ..CO2_eq[['N2O']] + 
                          totN2O_dyr_tot * ..CO2_eq[['N2O']])/1e+06]

# totCO2_eq_fortræng is in kt CO2 eq (so kg CO2 eq multiplied by 10^6)
emis[, model_gruppe := 'char']
emis[!grepl('biogas', Scenarie), ":="(CH4_dyr_biog = 0, totCH4_dyr_biog = 0)]
emis[, ":="(CO2_eq_fortræng = CH4_dyr_biog * 2.8,
            totCO2_eq_fortræng = (totCH4_dyr_biog * 2.8)/1e+06)][
              , ":="(CO2_eq_tot = CO2_eq_tot - CO2_eq_fortræng,
                     totCO2_eq_tot = totCO2_eq_tot - totCO2_eq_fortræng)
            ]

for(i in model_gruppe_navne){
  emis[StaldID %in% eval(parse(text = i)), model_gruppe := i]
}

emis_summary <- emis[, .(CH4_dyr_stald = unique(CH4_dyr_stald), 
                         CH4_dyr_lager = unique(CH4_dyr_lager),
                         CH4_dyr_tot = unique(CH4_dyr_tot),
                         NH3_dyr_tot = unique(NH3_dyr_tot),
                         N2O_dyr_tot = unique(N2O_dyr_tot),
                         CO2_eq_fortræng = unique(CO2_eq_fortræng),
                         CO2_eq_tot = unique(CO2_eq_tot),
                         totCO2_eq_tot = sum(totCO2_eq_tot)), by = c('Scenarie', 'model_gruppe')]

TotGoednabDyr <- setDT(read_excel('../output/TotGoedningabDyr.xlsx'))
Tech_udb <- setDT(read_excel('../data/teknologi_udbredelse.xlsx'))
Tech_pot <- setDT(read_excel('../data/teknologi_potentiale.xlsx'))

out <- merge.data.table(emis_summary, TotGoednabDyr)[Scenarie != "" & Scenarie != 'char']

techs <- unique(emis_summary[Scenarie != 'kontrol' & Scenarie != 'linespil', Scenarie])
model_gruppe <- unique(out[, model_gruppe])

for(i in model_gruppe){
  for(o in techs){
    if(any(out[model_gruppe == i, Scenarie] == o)){
       out[model_gruppe == i & Scenarie == o, ":="(udbredelse = ..Tech_udb[model_gruppe == i, eval(parse(text = o))],
                                                     potentiale = ..Tech_pot[model_gruppe == i, eval(parse(text = o))])]
    }
  }
}

out <- out[Scenarie != 'kontrol', totCO2_eq_tot := totCO2_eq_tot * (potentiale-udbredelse)/100][order(Scenarie)]
out <- out[, ":="(totCO2_eq_tot_pot_red = (totCO2_eq_tot[Scenarie == 'kontrol'] - totCO2_eq_tot),
                  CO2_eq_tot_red = (CO2_eq_tot[Scenarie == 'kontrol'] - CO2_eq_tot)), 
           by = c('model_gruppe')][order(Scenarie),]


fwrite(out, '../output/emis_table.csv')
write.xlsx(out, '../output/emis_table.xlsx')

