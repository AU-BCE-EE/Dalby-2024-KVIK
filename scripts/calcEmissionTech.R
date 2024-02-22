rm(list = ls())

library(readxl)
library(data.table)
library(openxlsx)

dat <- fread('../data/dat_merged.csv')

CO2_eq <- c(CH4 = 28, N2O = 265)

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
svin_gylle <- c(20,46,47,72,19,73,60,63,8,10,80,79,64,65)

## KVÆG
# gylle systemer
kvæg_ringkanal <- c(6, 13) 
kvæg_fast_skrab <- c(5, 11)
kvæg_spalter_skrab <- c(7, 14)
kvæg_hæld_fast_skrab <- c(49)
kvæg_andre_hyppig <- c(2, 4) # 2 is spaltegulvbokse, what is that? 
kvæg_gylle <- c(6,13,5,11,7,14,49,2,4)

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
                     'kvæg_andre_hyppig'
                     )

cols <- c('CH4_dyr_stald', 'CH4_dyr_lager', 'CH4_dyr_biog', 'NH3_dyr_stald', 'NH3_dyr_lager','N2O_dyr_dir_tot', 'N2O_dyr_indir_tot', 'strøm_CO2_ton')

tot_cols <- paste0('tot', cols)
dat[, strøm_CO2_ton := 0]

# Initialize the total columns with 0
dat <- dat[, (tot_cols) := 0][!duplicated(dat)]

# if biogas is used change the CH4 produktion to include emission from prestorage tank and change storage to digestate emission.
# also change N2O emission (based on national inventory report 2023, p431)
biogas <- copy(dat)[Scenarie == 'kontrol'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_dir_tot = N2O_dyr_dir_tot * 0.0006/0.00475)][
           , Scenarie := 'biogas']
ugentlig_biogas <- copy(dat)[Scenarie == 'ugentlig'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_dir_tot = N2O_dyr_dir_tot * 0.0006/0.00475)][
           , Scenarie := 'ugentlig_biogas']

køling_biogas <- copy(dat)[Scenarie == 'køling'][
  , ":="(CH4_dyr_stald = CH4_dyr_Stald_aft, 
         CH4_dyr_lager = CH4_dyr_afg,
         N2O_dyr_dir_tot = N2O_dyr_dir_tot * 0.0006/0.00475)][
           , Scenarie := 'køling_biogas']

dat <- rbind(dat, biogas, ugentlig_biogas, køling_biogas)

dat[grepl('køling', Scenarie) & DyreType == 'smågrise', strøm_CO2_ton := 1.3]
dat[grepl('køling', Scenarie) & DyreType == 'slagtesvin', strøm_CO2_ton := 1.4]
dat[grepl('køling', Scenarie) & DyreType == 'søer', strøm_CO2_ton := 1.8]

# Multiply each column in 'cols' by 1 (no change), emis units in kg CH4/m3/year or kg CH4/year, kg CO2 eq/m3/year, kg CO2 eq/year
# totCO2_eq_tot is in kt CO2 eq emitted (so kg CO2 eq multiplied by 10^6)
emis <- dat[, (cols) := lapply(.SD, function(x) x * 1), .SDcols = cols, by = c('StaldID', 'Scenarie', 'GoedningsNavn')][
  , (tot_cols) := lapply(.SD, function(x) x * TotGoednabDyr), .SDcols = cols, by = c('StaldID', 'Scenarie')][
    , ":="(CH4_dyr_tot = CH4_dyr_stald + CH4_dyr_lager,
           NH3_dyr_tot = NH3_dyr_stald + NH3_dyr_lager,
           N2O_dyr_tot = N2O_dyr_indir_tot + N2O_dyr_dir_tot)][
             , ":=" (CO2_eq_tot_CH4 = CH4_dyr_tot * ..CO2_eq[['CH4']],
                      CO2_eq_tot_N2O = N2O_dyr_tot * ..CO2_eq[['N2O']])][
                        , CO2_eq_tot := CO2_eq_tot_N2O + CO2_eq_tot_CH4 + strøm_CO2_ton][
               , ":="(totCH4_dyr_tot = totCH4_dyr_stald + totCH4_dyr_lager,
                      totN2O_dyr_tot = totN2O_dyr_dir_tot + totN2O_dyr_indir_tot)][
                        , totCO2_eq_tot := (totCH4_dyr_tot * ..CO2_eq[['CH4']] + 
                          totN2O_dyr_tot * ..CO2_eq[['N2O']] + totstrøm_CO2_ton)/1e+06]

# totCO2_eq_fortræng is in kt CO2 eq (so kg CO2 eq multiplied by 10^6)
emis[, model_gruppe := 'char']
emis[!grepl('biogas', Scenarie), ":="(CH4_dyr_biog = 0, totCH4_dyr_biog = 0)]
emis[, ":="(CO2_eq_fortræng = CH4_dyr_biog * 2.37,
            totCO2_eq_fortræng = (totCH4_dyr_biog * 2.37)/1e+06)][
              , ":="(CO2_eq_tot = CO2_eq_tot - CO2_eq_fortræng,
                     totCO2_eq_tot = totCO2_eq_tot - totCO2_eq_fortræng)
            ]

for(i in model_gruppe_navne){
  emis[StaldID %in% eval(parse(text = i)), model_gruppe := i]
}

emis_summary <- emis[, .(CH4_dyr_stald = unique(CH4_dyr_stald), 
                         CH4_dyr_lager = unique(CH4_dyr_lager),
                         CH4_dyr_tot = unique(CH4_dyr_tot),
                         N2O_dyr_tot = unique(N2O_dyr_tot),
                         CO2_eq_fortræng = unique(CO2_eq_fortræng),
                         CO2_eq_tot = unique(CO2_eq_tot),
                         totCO2_eq_tot = sum(totCO2_eq_tot)), by = c('Scenarie', 'model_gruppe')]

TotGoednabDyr <- setDT(read_excel('../output/TotGoedningabDyr.xlsx'))
Tech_udb <- setDT(read_excel('../data/teknologi_udbredelse.xlsx'))
Tech_pot <- setDT(read_excel('../data/teknologi_potentiale.xlsx'))

out <- merge.data.table(emis_summary, TotGoednabDyr)[Scenarie != "" & Scenarie != 'char']

techs <- unique(emis_summary[Scenarie != 'kontrol', Scenarie])
model_gruppe <- unique(out[, model_gruppe])

for(i in model_gruppe){
  for(o in techs){
    if(any(out[model_gruppe == i, Scenarie] == o)){
       out[model_gruppe == i & Scenarie == o, ":="(udbredelse = ..Tech_udb[model_gruppe == i, eval(parse(text = o))],
                                                     potentiale = ..Tech_pot[model_gruppe == i, eval(parse(text = o))])]
    }
    
  }
}



out <- out[, ":="(reduktion_totCO2_eq_tot = (totCO2_eq_tot[Scenarie == 'kontrol'] - totCO2_eq_tot) * ((potentiale-udbredelse)/100),
                  reduktion_CO2_eq_tot_m3 = CO2_eq_tot[Scenarie == 'kontrol'] - CO2_eq_tot), by = 'model_gruppe'][order(Scenarie)]

out_dyr <- copy(out)
out_dyr <- out_dyr[, .(sum_reduktion_totCO2_eq_tot = sum(reduktion_totCO2_eq_tot),
            mean_reduktion_totCO2_eq_tot_m3 = sum(reduktion_CO2_eq_tot_m3 * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            CH4_dyr_stald = sum(CH4_dyr_stald * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            CH4_dyr_lager = sum(CH4_dyr_lager * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            CH4_dyr_tot = sum(CH4_dyr_tot * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            N2O_dyr_tot = sum(N2O_dyr_tot * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            udbredelse = sum(udbredelse * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year),
            fortrængning = sum(CO2_eq_fortræng * TotGoednabDyr_kt_year)/sum(TotGoednabDyr_kt_year))
        , by = c('Scenarie', 'Dyr')]

#summary_Dyr <- out[`potent. % af modelgruppe` != 0, .(sum_emis = sum(`kt ab dyr pr. år` * 1000 * `kg CO2e netto pr. m3 ab dyr pr. år`), 
#                                                      sum_goedning = sum(`kt ab dyr pr. år` * 1000)), by = c('Dyr','Scenarie')][
#                                                        , .(red = sum_emis/)

units <- data.frame(t(data.frame(units = c('model_gruppe', 
           'Scenarie',  
           'kg CH4 pr. m3 ab dyr pr. år',
           'kg CH4 pr. m3 ab dyr pr. år',
           'kg CH4 pr. m3 ab dyr pr. år',
           'kg N2O pr. m3 ab dyr pr. år',
           'kg CO2e fortrængt pr. m3 ab dyr pr. år',
           'kg CO2e netto pr. m3 ab dyr pr. år',
           'kt CO2e netto pr. år',
           'DyreType',
           'kt ab dyr pr. år',
           'Dyr',
           'kg ab dyr pr. dyr prod/årsdyr',
           '%',
           '%',
           'kg ab lager pr. kg ab dyr',
           'kt ab lager pr. år',
           'udbred. % af modelgruppe',
           'potent. % af modelgruppe',
           'kt CO2e potentient reduceret pr. år',
           'kg CO2e reduceret pr. m3 ab dyr'))))
    
names(units) <- names(out)       
out <- rbind(units, out)
setDT(out)

out_table <- out[, .(model_gruppe, Scenarie, CH4_dyr_stald, CH4_dyr_lager, CH4_dyr_tot, N2O_dyr_tot, 
                     CO2_eq_fortræng, CO2_eq_tot, TotGoednabDyr_kt_year, udbredelse, potentiale, 
                     reduktion_CO2_eq_tot_m3, reduktion_totCO2_eq_tot, Dyr)]
names(out_table) <- c(t(out_table[1,]))
unique(out_table$model_gruppe)
out_table <- out_table[-1,]
out_table[, model_gruppe:= factor(model_gruppe, levels = c('kvæg_ringkanal','kvæg_fast_skrab','kvæg_spalter_skrab',
                                                           'kvæg_hæld_fast_skrab','kvæg_andre_hyppig',
                                                           'spalter_33_67_slagtesvin','spalter_25_50_slagtesvin','spalter_50_75_slagtesvin',
                                                           'løs_individuel_søer','farestald_delvis_spalte', 'farestald_fuldspalte',
                                                           'toklimastald_smågrise','spalter_smågrise'))]
                                                           
setorder(out_table, Scenarie, model_gruppe)

cols_index <- which(!names(out_table) %in% c('model_gruppe', 'Scenarie','Dyr'))
out_table[, (cols_index) := lapply(.SD, function(x) round(as.numeric(x), 3)), .SDcols = cols_index]

#summary_Dyr <- out_table[`potent. % af modelgruppe` != 0, .(sum_emis = sum(`kt ab dyr pr. år` * 1000 * `kg CO2e netto pr. m3 ab dyr pr. år`), 
#                                                     sum_goedning = sum(`kt ab dyr pr. år` * 1000)), by = c('Dyr','Scenarie')][
#                                                       , .(red = sum_emis/)
#                                                                                                                               ]


write.xlsx(out_table, '../output/emis_table_KVIK.xlsx')
write.xlsx(out, '../output/emis_table_full.xlsx')

#how much CO2 eq comes from N2O vs CH4?
CH4_vs_N2O <- out[Scenarie == 'kontrol', .(N2O_CO2_eq = mean(N2O_dyr_tot * ..CO2_eq[['N2O']]), CH4_CO2_eq = mean(CH4_dyr_tot * ..CO2_eq[['CH4']])), by = c('Dyr')][
  , frac_N2O_CO2_eq := N2O_CO2_eq/(N2O_CO2_eq + CH4_CO2_eq)]
