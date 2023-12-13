rm(list = ls())

source('merge_dat.R')

library('tidyr')
library('dplyr')
library('readxl')

# udvalgte teknologiers udbredelse 2020
KF_biogas <- data.frame(read_excel('../data/kf22_landbrug_20220603.xlsx', sheet = 'Tabel 9 Gylle afsat til biogas', skip = 4))
KF_forsur <- data.frame(read_excel('../data/kf22_landbrug_20220603.xlsx', sheet = 'Tabel 7 Miljøteknologi', skip = 10, col_names = T))
KF_gyllekøl <- data.frame(read_excel('../data/kf22_landbrug_20220603.xlsx', sheet = 'Tabel 7 Miljøteknologi', skip = 6, col_names = T))

# forsuring udbredelse, %
kvæg_forsur <- KF_forsur$X2020[KF_forsur$Forsuring.I.stald. == 'Malkekvæg'][1] * sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == 'kvæg'])/1000 / (sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000)
søer_forsur <- KF_forsur$X2020[KF_forsur$Forsuring.I.stald. == 'Søer'][1]
slagtesvin_forsur <- KF_forsur$X2020[KF_forsur$Forsuring.I.stald. == 'Slagtesvin'][1]
smågrise_forsur <- KF_forsur$X2020[KF_forsur$Forsuring.I.stald. == 'Smågrise'][1]

# gyllekøling udbredelse, %
søer_gyllekøl <- KF_gyllekøl$X2020[KF_gyllekøl$Gyllekøling. == 'Søer'][1]
slagtesvin_gyllekøl <- KF_gyllekøl$X2020[KF_gyllekøl$Gyllekøling. == 'Slagtesvin'][1]
smågrise_gyllekøl <- KF_gyllekøl$X2020[KF_gyllekøl$Gyllekøling. == 'Smågrise'][1]

# bioforgasning udbredelse, %
kvæg_biogas <- KF_biogas$X2020[KF_biogas$kt == 'Kvæggylle']/(sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == 'kvæg'])/1000) * 100
svin_biogas <- KF_biogas$X2020[KF_biogas$kt == 'Svinegylle']/(sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == 'svin'])/1000) * 100

# reduktionssats for udvalgte teknologier
tech_red <- c('lavdosis' = 0.7, 'afbrænding' = 0.64, 'oxidation' = 0.4, 'staldforsuring' = 0.7)
Ft <- 2.8 # Fortrængningsfaktor, CO2 kv./produceret CH4
GWP_CH4 <- 28

#gødning_ab_dyr fra dyrekategorier, kt/år
smågrise_ab_dyr <- sum(merge_dat$TotGødnabDyr[(merge_dat$DyrNavn == 'Smågrise, 7,5-30 kg' | merge_dat$DyrNavn == 'Smågrise, øko')])/1000
slagtesvin_ab_dyr <- sum(merge_dat$TotGødnabDyr[(merge_dat$DyrNavn == 'Slagtesvin, 30-100,0 kg' | merge_dat$DyrNavn == 'Slagtesvin, øko')])/1000
søer_ab_dyr <- sum(merge_dat$TotGødnabDyr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko')])/1000
kvæg_ab_dyr <- sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == 'kvæg'])/1000
kvæg_ab_dyr
svin_ab_dyr <- sum(smågrise_ab_dyr, slagtesvin_ab_dyr, søer_ab_dyr)

#udbredelse gødning ab_dyr basis, %
s <- 'sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn =='
stald <- c('\'Sengebåse, spalter, bagskyl/ringkanalanlæg\'',
           '\'Sengebåse, fast gulv, skraberanlæg\'',
           '\'Sengebåse, spalter, skraberanlæg\'',
           '\'Sengebåse, fast gulv, 2% hæld, skrab\'',
           '\'Drænet gulv + spalter (33/67)\'',
           '\'Delvist spaltegulv (25-49 % fast gulv)\'',
           '\'Delvist spaltegulv (50-75 % fast gulv)\'',
           '\'Løbe afd., individuel, delvis spalte\' | merge_dat$StaldNavn == \'Løbe afd., løs, delvis spalte\'',
           '\'Fare afd., kassesti, delvis spalte\'',
           '\'Fare afd., kassesti, fuldspalte\'',
           '\'Toklimastald m. delvis spaltegulv\'',
           '\'Drænet gulv + spalter (50/50)\'')

#gødning_ab_dyr fra staldNavne, kt/år
Gødning_ab_dyr <- NULL
for (i in 1:length(stald)){
  Gødning_ab_dyr[i] <- round(eval(parse(text = paste(s, stald[i],'])/1000',sep = ''))), 3)
}
Gødning_ab_dyr <- as.data.frame(Gødning_ab_dyr )
Gødning_ab_dyr$StaldNavn <- stald

Gødning_ab_dyr$udbredelse <- round(Gødning_ab_dyr$Gødning_ab_dyr/c(kvæg_ab_dyr, kvæg_ab_dyr, kvæg_ab_dyr, kvæg_ab_dyr,
  slagtesvin_ab_dyr, slagtesvin_ab_dyr, slagtesvin_ab_dyr,
  søer_ab_dyr, søer_ab_dyr, søer_ab_dyr,
  smågrise_ab_dyr, smågrise_ab_dyr) * 100, 3)

ab_lager_ab_dyr_F <- c(1.18398628, 1.18398628,1.18398628,1.18398628, 1.098299486, 1.085969015, 
                       1.085969015, 1.711616719, 1.455319149, 1.455319149, 1.423376819, 1.420017483)
                       
Gødning_ab_dyr$Gødning_ab_lager <- round(Gødning_ab_dyr$Gødning_ab_dyr * ab_lager_ab_dyr_F, 3)

hyp_udsl <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                  data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg' & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 0.77, Lager = 2.86, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 0.69, Lager = 2.89, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 0.61, Lager = 2.91, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte'| merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 0.74, Lager = 2.73, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[(merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte') & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 0.92, Lager = 2.68, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte' & merge_dat$GødnNavn == 'Gylle'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                  data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 0.64, Lager = 2.62, Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv' & merge_dat$GødnNavn == 'Gylle'])/1000))


hyp_udsl$Total <- hyp_udsl$Stald + hyp_udsl$Lager
hyp_udsl.mod <- hyp_udsl %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
hyp_udsl.sum <- hyp_udsl.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                 Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Gyllemængde = sum(Gyllemængde))
hyp_udsl.fin <- bind_rows(hyp_udsl.mod, hyp_udsl.sum)

hyp_udsl.fin[, c('Stald','Lager','Total')] <- round(hyp_udsl.fin[, c('Stald','Lager','Total')], 3)
hyp_udsl.fin[, c('Reduktion', 'Reduktion_CO2')] <- round(hyp_udsl.fin[, c('Reduktion', 'Reduktion_CO2')], 3)
hyp_udsl.fin$Reduktion[hyp_udsl.fin$Reference == 'Ja'] <- 0
hyp_udsl.fin$Reduktion_CO2 <- round(hyp_udsl.fin$Reduktion_CO2, 3)
hyp_udsl.fin$Reduktion_CO2[hyp_udsl.fin$Reference == 'Ja'] <- 0
hyp_udsl.fin$Gyllemængde <- round(hyp_udsl.fin$Gyllemængde, 3)

hyp_udsl.fin <- select(hyp_udsl.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
hyp_udsl.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', hyp_udsl.fin$Staldtype)
hyp_udsl.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', hyp_udsl.fin$Staldtype)
hyp_udsl.fin$Dyretype[is.na(hyp_udsl.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

hyp_udsl.fin$udbredelse <- 0
hyp_udsl.fin$udbredelse[hyp_udsl.fin$Staldtype == 'Drægtighedstalde, løs + individuel, delvis spalte'] <- 40
hyp_udsl.fin$udbredelse[hyp_udsl.fin$Dyretype == 'Alle svin'] <- round(40 * sum(hyp_udsl.fin$Gyllemængde[hyp_udsl.fin$Staldtype == 'Drægtighedstalde, løs + individuel, delvis spalte' & hyp_udsl.fin$Reference == 'Nej'], na.rm = T)/sum(hyp_udsl.fin$Gyllemængde[hyp_udsl.fin$Staldtype != 'Drægtighedstalde, løs + individuel, delvis spalte' & hyp_udsl.fin$Dyretype != "Kvæg" & hyp_udsl.fin$Dyretype != "Alle kvæg" & hyp_udsl.fin$Dyretype != "Alle svin"]),3)
hyp_udsl.fin$total_red <- round((100 - hyp_udsl.fin$udbredelse)/100 * hyp_udsl.fin$Gyllemængde * hyp_udsl.fin$Reduktion_CO2/1000,3)


bioforgas <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.80, Lager = 0.30, Total =  0, Fortrængning = 12.55 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.24, Lager = 0.34, Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.24, Lager = 0.34, Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg' ])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.24, Lager = 0.34, Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                   
                   
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 2.09, Lager = 0.11, Total =  0, Fortrængning = 12.18 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.70, Lager = 0.11, Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.28, Lager = 0.12, Total =  0, Fortrængning = 13.53 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 2.05, Lager = 0.10, Total =  0, Fortrængning = 11.56 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.61, Lager = 0.09, Total =  0, Fortrængning = 10.63 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.92, Lager = 0.09, Total =  0, Fortrængning = 10.11 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.65, Lager = 0.1, Total =  0, Fortrængning = 11.50 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59 , Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.73, Lager = 0.09, Total =  0, Fortrængning = 9.71 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))



bioforgas$Total <- bioforgas$Stald + bioforgas$Lager
bioforgas.mod <- bioforgas %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4 + Fortrængning) %>% ungroup()
bioforgas.sum <- bioforgas.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                 Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Gyllemængde = sum(Gyllemængde))
bioforgas.fin <- bind_rows(bioforgas.mod, bioforgas.sum)

bioforgas.fin[, c('Stald','Lager','Total')] <- round(bioforgas.fin[, c('Stald','Lager','Total')], 3)
bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
bioforgas.fin$Reduktion[bioforgas.fin$Reference == 'Ja'] <- 0
bioforgas.fin$Reduktion_CO2 <- round(bioforgas.fin$Reduktion_CO2, 3)
bioforgas.fin$Reduktion_CO2[bioforgas.fin$Reference == 'Ja'] <- 0
bioforgas.fin$Gyllemængde <- round(bioforgas.fin$Gyllemængde, 3)

bioforgas.fin <- select(bioforgas.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Fortrængning', 'Reduktion_CO2', 'Gyllemængde')) 
bioforgas.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', bioforgas.fin$Staldtype)
bioforgas.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', bioforgas.fin$Staldtype)
bioforgas.fin$Dyretype[is.na(bioforgas.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

bioforgas.fin$udbredelse <- 0
bioforgas.fin$udbredelse[bioforgas.fin$Dyretype == 'Kvæg'] = round(kvæg_biogas,3)
bioforgas.fin$udbredelse[bioforgas.fin$Dyretype == 'Alle kvæg'] = round(kvæg_biogas,3)
bioforgas.fin$udbredelse[bioforgas.fin$Dyretype != 'Kvæg' & bioforgas.fin$Dyretype != 'Alle kvæg' ] = round(svin_biogas,3)
bioforgas.fin$total_red <- round((100-bioforgas.fin$udbredelse)/100 * bioforgas.fin$Gyllemængde * bioforgas.fin$Reduktion_CO2/1000,3)


hyp_bioforgas <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 0.24, Lager = 0.34, Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg' & merge_dat$GødnNavn == 'Gylle'])/1000),
                   
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 0.99, Lager = 0.12, Total =  0, Fortrængning = 14.01 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 0.92, Lager = 0.12, Total =  0, Fortrængning = 14.14 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 0.84, Lager = 0.13, Total =  0, Fortrængning = 14.26 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 0.96, Lager = 0.12, Total =  0, Fortrængning = 13.38 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[(merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte') & merge_dat$GødnNavn == 'Gylle'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 1.12, Lager = 0.12, Total =  0, Fortrængning = 13.10 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte' & merge_dat$GødnNavn == 'Gylle'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 0.85, Lager = 0.11, Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv' & merge_dat$GødnNavn == 'Gylle'])/1000))
                   
hyp_bioforgas$Total <- hyp_bioforgas$Stald + hyp_bioforgas$Lager
hyp_bioforgas.mod <- hyp_bioforgas %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4 + Fortrængning) %>% ungroup()
hyp_bioforgas.sum <- hyp_bioforgas.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                   Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Gyllemængde = sum(Gyllemængde))
hyp_bioforgas.fin <- bind_rows(hyp_bioforgas.mod, hyp_bioforgas.sum)

hyp_bioforgas.fin[, c('Stald','Lager','Total')] <- round(hyp_bioforgas.fin[, c('Stald','Lager','Total')], 3)
hyp_bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(hyp_bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
hyp_bioforgas.fin$Reduktion[hyp_bioforgas.fin$Reference == 'Ja'] <- 0
hyp_bioforgas.fin$Reduktion_CO2 <- round(hyp_bioforgas.fin$Reduktion_CO2, 3)
hyp_bioforgas.fin$Reduktion_CO2[hyp_bioforgas.fin$Reference == 'Ja'] <- 0
hyp_bioforgas.fin$Gyllemængde <- round(hyp_bioforgas.fin$Gyllemængde, 3)

hyp_bioforgas.fin <- select(hyp_bioforgas.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Fortrængning', 'Reduktion_CO2', 'Gyllemængde')) 
hyp_bioforgas.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', hyp_bioforgas.fin$Staldtype)
hyp_bioforgas.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', hyp_bioforgas.fin$Staldtype)
hyp_bioforgas.fin$Dyretype[is.na(hyp_bioforgas.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

hyp_bioforgas.fin$udbredelse <- 0
hyp_bioforgas.fin$udbredelse[hyp_bioforgas.fin$Dyretype == 'Kvæg'] = round((kvæg_biogas * 0),3)
hyp_bioforgas.fin$udbredelse[hyp_bioforgas.fin$Dyretype == 'Alle kvæg'] = round((kvæg_biogas * 0),3)
hyp_bioforgas.fin$udbredelse[hyp_bioforgas.fin$Staldtype == 'Drægtighedstalde, løs + individuel, delvis spalte'] <- round(40 * svin_biogas/100,3)
hyp_bioforgas.fin$udbredelse[hyp_bioforgas.fin$Dyretype == 'Alle svin'] <- round(40 * sum(hyp_bioforgas.fin$Gyllemængde[hyp_bioforgas.fin$Staldtype == 'Drægtighedstalde, løs + individuel, delvis spalte' & hyp_bioforgas.fin$Reference == 'Nej'], na.rm = T)/sum(hyp_bioforgas.fin$Gyllemængde[hyp_bioforgas.fin$Staldtype != 'Drægtighedstalde, løs + individuel, delvis spalte' & hyp_bioforgas.fin$Dyretype != "Kvæg" & hyp_bioforgas.fin$Dyretype != "Alle kvæg" & hyp_bioforgas.fin$Dyretype != "Alle svin"]) * svin_biogas/100,3)
hyp_bioforgas.fin$total_red <- round((100-hyp_bioforgas.fin$udbredelse)/100 * hyp_bioforgas.fin$Gyllemængde * hyp_bioforgas.fin$Reduktion_CO2/1000,3)

hyp_afbrænd <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                     data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)',                Stald = 1.91, Lager = 2.49,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)',                Stald = 0.77, Lager = 2.86 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)',       Stald = 1.50, Lager = 2.62,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)',       Stald = 0.69, Lager = 2.89 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)',       Stald = 1.06, Lager = 2.76,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)',       Stald = 0.61, Lager = 2.91 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',         Stald = 1.87, Lager = 2.36,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',         Stald = 0.74, Lager = 2.73 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[(merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte') & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',           Stald = 2.46, Lager = 2.17,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',           Stald = 0.92, Lager = 2.68 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',            Stald = 1.47, Lager = 2.35,                               Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',            Stald = 0.64, Lager = 2.62 * (1-tech_red['afbrænding']),  Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv' & merge_dat$GødnNavn == 'Gylle'])/1000))


hyp_afbrænd$Total <- hyp_afbrænd$Stald + hyp_afbrænd$Lager
hyp_afbrænd.mod <- hyp_afbrænd %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
hyp_afbrænd.sum <- hyp_afbrænd.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                 Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                 Gyllemængde = sum(Gyllemængde))
hyp_afbrænd.fin <- bind_rows(hyp_afbrænd.mod, hyp_afbrænd.sum)

hyp_afbrænd.fin[, c('Stald','Lager','Total')] <- round(hyp_afbrænd.fin[, c('Stald','Lager','Total')], 3)
hyp_afbrænd.fin[, c('Reduktion', 'Reduktion_CO2')] <- round(hyp_afbrænd.fin[, c('Reduktion', 'Reduktion_CO2')], 3)
hyp_afbrænd.fin$Reduktion[hyp_afbrænd.fin$Reference == 'Ja'] <- 0
hyp_afbrænd.fin$Reduktion_CO2 <- round(hyp_afbrænd.fin$Reduktion_CO2, 3)
hyp_afbrænd.fin$Reduktion_CO2[hyp_afbrænd.fin$Reference == 'Ja'] <- 0
hyp_afbrænd.fin$Gyllemængde <- round(hyp_afbrænd.fin$Gyllemængde, 3)

hyp_afbrænd.fin <- select(hyp_afbrænd.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
hyp_afbrænd.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', hyp_afbrænd.fin$Staldtype)
hyp_afbrænd.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', hyp_afbrænd.fin$Staldtype)
hyp_afbrænd.fin$Dyretype[is.na(hyp_afbrænd.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

hyp_afbrænd.fin$udbredelse <- 0
hyp_afbrænd.fin$udbredelse[hyp_afbrænd.fin$Dyretype == 'Kvæg'] = 0
hyp_afbrænd.fin$udbredelse[hyp_afbrænd.fin$Dyretype == 'Alle kvæg'] = 0
hyp_afbrænd.fin$udbredelse[hyp_afbrænd.fin$Dyretype != 'Kvæg' & hyp_afbrænd.fin$Dyretype != 'Alle kvæg'] = 0
hyp_afbrænd.fin$total_red <- round((100-hyp_afbrænd.fin$udbredelse)/100 * hyp_afbrænd.fin$Gyllemængde * hyp_afbrænd.fin$Reduktion_CO2/1000,3)

hyp_ox <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                     data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['oxidation']) , Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 0.77, Lager = 2.86 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 0.69, Lager = 2.89 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 0.61, Lager = 2.91 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 0.74, Lager = 2.73 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[(merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte') & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 0.92, Lager = 2.68 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte' & merge_dat$GødnNavn == 'Gylle'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 0.64, Lager = 2.62 * (1-tech_red['oxidation']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv' & merge_dat$GødnNavn == 'Gylle'])/1000))


hyp_ox$Total <- hyp_ox$Stald + hyp_ox$Lager
hyp_ox.mod <- hyp_ox %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
hyp_ox.sum <- hyp_ox.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                       Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Gyllemængde = sum(Gyllemængde))
hyp_ox.fin <- bind_rows(hyp_ox.mod, hyp_ox.sum)

hyp_ox.fin[, c('Stald','Lager','Total')] <- round(hyp_ox.fin[, c('Stald','Lager','Total')], 3)
hyp_ox.fin[, c('Reduktion', 'Reduktion_CO2')] <- round(hyp_ox.fin[, c('Reduktion', 'Reduktion_CO2')], 3)
hyp_ox.fin$Reduktion[hyp_ox.fin$Reference == 'Ja'] <- 0
hyp_ox.fin$Reduktion_CO2 <- round(hyp_ox.fin$Reduktion_CO2, 3)
hyp_ox.fin$Reduktion_CO2[hyp_ox.fin$Reference == 'Ja'] <- 0
hyp_ox.fin$Gyllemængde <- round(hyp_ox.fin$Gyllemængde, 3)

hyp_ox.fin <- select(hyp_ox.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
hyp_ox.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', hyp_ox.fin$Staldtype)
hyp_ox.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', hyp_ox.fin$Staldtype)
hyp_ox.fin$Dyretype[is.na(hyp_ox.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

hyp_ox.fin$udbredelse <- 0
hyp_ox.fin$udbredelse[hyp_ox.fin$Dyretype == 'Kvæg'] = 0
hyp_ox.fin$udbredelse[hyp_ox.fin$Dyretype == 'Alle kvæg'] = 0
hyp_ox.fin$udbredelse[hyp_ox.fin$Dyretype != 'Kvæg' & hyp_ox.fin$Dyretype != 'Alle kvæg'] = 0
hyp_ox.fin$total_red <- round((100-hyp_ox.fin$udbredelse)/100 * hyp_ox.fin$Gyllemængde * hyp_ox.fin$Reduktion_CO2/1000,3)

hyp_lavdos <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['lavdosis']) , Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg' & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 0.77, Lager = 2.86 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 0.69, Lager = 2.89 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 0.61, Lager = 2.91 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)' & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 0.74, Lager = 2.73 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[(merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte') & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 0.92, Lager = 2.68 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte' & merge_dat$GødnNavn == 'Gylle'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 0.64, Lager = 2.62 * (1-tech_red['lavdosis']), Total =  0, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv' & merge_dat$GødnNavn == 'Gylle'])/1000))


hyp_lavdos$Total <- hyp_lavdos$Stald + hyp_lavdos$Lager
hyp_lavdos.mod <- hyp_lavdos %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
hyp_lavdos.sum <- hyp_lavdos.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                             Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                             Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                             Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                             Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                             Gyllemængde = sum(Gyllemængde))
hyp_lavdos.fin <- bind_rows(hyp_lavdos.mod, hyp_lavdos.sum)

hyp_lavdos.fin[, c('Stald','Lager','Total')] <- round(hyp_lavdos.fin[, c('Stald','Lager','Total')], 3)
hyp_lavdos.fin[, c('Reduktion', 'Reduktion_CO2')] <- round(hyp_lavdos.fin[, c('Reduktion', 'Reduktion_CO2')], 3)
hyp_lavdos.fin$Reduktion[hyp_lavdos.fin$Reference == 'Ja'] <- 0
hyp_lavdos.fin$Reduktion_CO2 <- round(hyp_lavdos.fin$Reduktion_CO2, 3)
hyp_lavdos.fin$Reduktion_CO2[hyp_lavdos.fin$Reference == 'Ja'] <- 0
hyp_lavdos.fin$Gyllemængde <- round(hyp_lavdos.fin$Gyllemængde, 3)

hyp_lavdos.fin <- select(hyp_lavdos.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
hyp_lavdos.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', hyp_lavdos.fin$Staldtype)
hyp_lavdos.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', hyp_lavdos.fin$Staldtype)
hyp_lavdos.fin$Dyretype[is.na(hyp_lavdos.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

hyp_lavdos.fin$udbredelse <- 0
hyp_lavdos.fin$udbredelse[hyp_lavdos.fin$Dyretype == 'Kvæg'] = 0
hyp_lavdos.fin$udbredelse[hyp_lavdos.fin$Dyretype == 'Alle kvæg'] = 0
hyp_lavdos.fin$udbredelse[hyp_lavdos.fin$Dyretype != 'Kvæg' & hyp_lavdos.fin$Dyretype != 'Alle kvæg'] = 0
hyp_lavdos.fin$total_red <- round((100-hyp_lavdos.fin$udbredelse)/100 * hyp_lavdos.fin$Gyllemængde * hyp_lavdos.fin$Reduktion_CO2/1000,3)



lavdos <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 12.55 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg' ])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                   data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                   
                   
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 12.18 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 13.53 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 11.56 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 10.63 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 10.11 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 11.50 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
                   data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98 * (1-tech_red['lavdosis']), Total =  0, Fortrængning = 9.71 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))



lavdos$Total <- lavdos$Stald + lavdos$Lager
lavdos.mod <- lavdos %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
lavdos.sum <- lavdos.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                   Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                   Gyllemængde = sum(Gyllemængde))
lavdos.fin <- bind_rows(lavdos.mod, lavdos.sum)

lavdos.fin[, c('Stald','Lager','Total')] <- round(lavdos.fin[, c('Stald','Lager','Total')], 3)
lavdos.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(lavdos.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
lavdos.fin$Reduktion[lavdos.fin$Reference == 'Ja'] <- 0
lavdos.fin$Reduktion_CO2 <- round(lavdos.fin$Reduktion_CO2, 3)
lavdos.fin$Reduktion_CO2[lavdos.fin$Reference == 'Ja'] <- 0
lavdos.fin$Gyllemængde <- round(lavdos.fin$Gyllemængde, 3)

lavdos.fin <- select(lavdos.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
lavdos.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', lavdos.fin$Staldtype)
lavdos.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', lavdos.fin$Staldtype)
lavdos.fin$Dyretype[is.na(lavdos.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

lavdos.fin$udbredelse <- 0
lavdos.fin$udbredelse[lavdos.fin$Dyretype == 'Kvæg'] = 0
lavdos.fin$udbredelse[lavdos.fin$Dyretype == 'Alle kvæg'] = 0
lavdos.fin$udbredelse[lavdos.fin$Dyretype != 'Kvæg' & lavdos.fin$Dyretype != 'Alle kvæg'] = 0
lavdos.fin$total_red <- round((100-lavdos.fin$udbredelse)/100 * lavdos.fin$Gyllemængde * lavdos.fin$Reduktion_CO2/1000,3)




ox <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 12.55 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg' ])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
                
                
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 12.18 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 13.53 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 11.56 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 10.63 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 10.11 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 11.50 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
                data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98 * (1-tech_red['oxidation']), Total =  0, Fortrængning = 9.71 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))



ox$Total <- ox$Stald + ox$Lager
ox.mod <- ox %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
ox.sum <- ox.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                             Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                             Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                             Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                             Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                             Gyllemængde = sum(Gyllemængde))
ox.fin <- bind_rows(ox.mod, ox.sum)

ox.fin[, c('Stald','Lager','Total')] <- round(ox.fin[, c('Stald','Lager','Total')], 3)
ox.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(ox.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
ox.fin$Reduktion[ox.fin$Reference == 'Ja'] <- 0
ox.fin$Reduktion_CO2 <- round(ox.fin$Reduktion_CO2, 3)
ox.fin$Reduktion_CO2[ox.fin$Reference == 'Ja'] <- 0
ox.fin$Gyllemængde <- round(ox.fin$Gyllemængde, 3)

ox.fin <- select(ox.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
ox.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', ox.fin$Staldtype)
ox.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', ox.fin$Staldtype)
ox.fin$Dyretype[is.na(ox.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

ox.fin$udbredelse <- 0
ox.fin$udbredelse[ox.fin$Dyretype == 'Kvæg'] = 0
ox.fin$udbredelse[ox.fin$Dyretype == 'Alle kvæg'] = 0
ox.fin$udbredelse[ox.fin$Dyretype != 'Kvæg' & ox.fin$Dyretype != 'Alle kvæg'] = 0
ox.fin$total_red <- round((100-ox.fin$udbredelse)/100 * ox.fin$Gyllemængde * ox.fin$Reduktion_CO2/1000,3)

afbrænd <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 12.55 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, skraberanlæg'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, skraberanlæg',   Stald = 0.15, Lager = 0.91 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, skraberanlæg' ])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
            data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, fast gulv, 2% hæld, skrab',   Stald = 0.15, Lager = 0.91 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 14.29 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, fast gulv, 2% hæld, skrab'])/1000),
            
            
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 12.18 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 13.53 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 11.56 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 10.63 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 10.11 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 11.50 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
            data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98 * (1-tech_red['afbrænding']), Total =  0, Fortrængning = 9.71 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))



afbrænd$Total <- afbrænd$Stald + afbrænd$Lager
afbrænd.mod <- afbrænd %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
afbrænd.sum <- afbrænd.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                     Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                     Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                     Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                     Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                     Gyllemængde = sum(Gyllemængde))
afbrænd.fin <- bind_rows(afbrænd.mod, afbrænd.sum)

afbrænd.fin[, c('Stald','Lager','Total')] <- round(afbrænd.fin[, c('Stald','Lager','Total')], 3)
afbrænd.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(afbrænd.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
afbrænd.fin$Reduktion[afbrænd.fin$Reference == 'Ja'] <- 0
afbrænd.fin$Reduktion_CO2 <- round(afbrænd.fin$Reduktion_CO2, 3)
afbrænd.fin$Reduktion_CO2[afbrænd.fin$Reference == 'Ja'] <- 0
afbrænd.fin$Gyllemængde <- round(afbrænd.fin$Gyllemængde, 3)

afbrænd.fin <- select(afbrænd.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
afbrænd.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', afbrænd.fin$Staldtype)
afbrænd.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', afbrænd.fin$Staldtype)
afbrænd.fin$Dyretype[is.na(afbrænd.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

afbrænd.fin$udbredelse <- 0
afbrænd.fin$udbredelse[afbrænd.fin$Dyretype == 'Kvæg'] = 0
afbrænd.fin$udbredelse[afbrænd.fin$Dyretype == 'Alle kvæg'] = 0
afbrænd.fin$udbredelse[afbrænd.fin$Dyretype != 'Kvæg' & afbrænd.fin$Dyretype != 'Alle kvæg'] = 0
afbrænd.fin$total_red <- round((100-afbrænd.fin$udbredelse)/100 * afbrænd.fin$Gyllemængde * afbrænd.fin$Reduktion_CO2/1000,3)

staldforsur <- rbind(data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73, Lager = 0.80, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
                 data.frame(Dyrekategori = 'Kvæg', Dyretype = 'Kvæg',       Staldtype = 'Sengebåse, spalter, bagskyl/ringkanalanlæg',   Stald = 1.73 * (1-tech_red['staldforsuring']), Lager = 0.80 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 12.55 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Sengebåse, spalter, bagskyl/ringkanalanlæg'])/1000),
      
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91 * (1-tech_red['staldforsuring']), Lager = 2.49 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 12.18 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50 * (1-tech_red['staldforsuring']), Lager = 2.62 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 12.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06 * (1-tech_red['staldforsuring']), Lager = 2.76 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 13.53 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87 * (1-tech_red['staldforsuring']), Lager = 2.36 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 11.56 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46 * (1-tech_red['staldforsuring']), Lager = 2.17 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 10.63 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78 * (1-tech_red['staldforsuring']), Lager = 2.06 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 10.11 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47 * (1-tech_red['staldforsuring']), Lager = 2.35 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 11.50 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
                 data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59 * (1-tech_red['staldforsuring']), Lager = 1.98 * (1-tech_red['staldforsuring']), Total =  0, Fortrængning = 9.71 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))



staldforsur$Total <- staldforsur$Stald + staldforsur$Lager
staldforsur.mod <- staldforsur %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4) %>% ungroup()
staldforsur.sum <- staldforsur.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                               Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                               Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                               Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                               Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                               Gyllemængde = sum(Gyllemængde))
staldforsur.fin <- bind_rows(staldforsur.mod, staldforsur.sum)

staldforsur.fin[, c('Stald','Lager','Total')] <- round(staldforsur.fin[, c('Stald','Lager','Total')], 3)
staldforsur.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(staldforsur.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
staldforsur.fin$Reduktion[staldforsur.fin$Reference == 'Ja'] <- 0
staldforsur.fin$Reduktion_CO2 <- round(staldforsur.fin$Reduktion_CO2, 3)
staldforsur.fin$Reduktion_CO2[staldforsur.fin$Reference == 'Ja'] <- 0
staldforsur.fin$Gyllemængde <- round(staldforsur.fin$Gyllemængde, 3)

staldforsur.fin <- select(staldforsur.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Reduktion_CO2', 'Gyllemængde')) 
staldforsur.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', staldforsur.fin$Staldtype)
staldforsur.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', staldforsur.fin$Staldtype)
staldforsur.fin$Dyretype[is.na(staldforsur.fin$Dyretype)] <- c('Alle kvæg', 'Alle kvæg', 'Alle svin', 'Alle svin')

staldforsur.fin$udbredelse <- 0
staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Kvæg'] = round(kvæg_forsur,3)
staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Alle kvæg'] = round(kvæg_forsur,3)

staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Slagtesvin'] = round(slagtesvin_forsur,3)
staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Søer'] = round(søer_forsur,3)
staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Smågrise'] = round(smågrise_forsur,3)
staldforsur.fin$udbredelse[staldforsur.fin$Dyretype == 'Alle svin'] = round((slagtesvin_forsur * slagtesvin_ab_dyr + søer_forsur * søer_ab_dyr + smågrise_forsur * smågrise_ab_dyr)/(svin_ab_dyr),3)

staldforsur.fin$total_red <- round((100-staldforsur.fin$udbredelse)/100 * staldforsur.fin$Gyllemængde * staldforsur.fin$Reduktion_CO2/1000,3)

køl_bioforgas <- rbind(data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.91, Lager = 2.49, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Drænet gulv + spalter (33/67)', Stald = 1.76 , Lager = 0.11 , Total =  0, Fortrængning = 12.68 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (33/67)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.50, Lager = 2.62, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (25-49 % fast gulv)', Stald = 1.42 , Lager = 0.12 , Total =  0, Fortrængning = 13.24 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (25-49 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06, Lager = 2.76, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Slagtesvin', Staldtype = 'Delvist spaltegulv (50-75 % fast gulv)', Stald = 1.06 , Lager = 0.12 , Total =  0, Fortrængning = 13.83 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Delvist spaltegulv (50-75 % fast gulv)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.87, Lager = 2.36, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Løbe afd., løs + individuel, delvis spalte',   Stald = 1.72 , Lager = 0.11 , Total =  0, Fortrængning = 12.05 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Løbe afd., individuel, delvis spalte' | merge_dat$StaldNavn == 'Løbe afd., løs, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.46, Lager = 2.17, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, delvis spalte',   Stald = 2.22 , Lager = 0.10 , Total =  0, Fortrængning = 11.23 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, delvis spalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald = 2.78, Lager = 2.06, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Søer',       Staldtype = 'Fare afd., kassesti, fuldspalte',   Stald =  2.50 , Lager = 0.10 , Total =  0, Fortrængning = 10.76  * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Fare afd., kassesti, fuldspalte'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.47, Lager = 2.35, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Toklimastald m. delvis spaltegulv',   Stald = 1.38 , Lager = 0.11 , Total =  0, Fortrængning = 11.89 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Toklimastald m. delvis spaltegulv'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.59, Lager = 1.98, Total =  0, Fortrængning = 0, Reference = 'Ja', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000),
                     data.frame(Dyrekategori = 'Svin', Dyretype = 'Smågrise',   Staldtype = 'Drænet gulv + spalter (50/50)',   Stald = 2.33 , Lager = 0.11 , Total =  0, Fortrængning = 10.32 * Ft, Reference = 'Nej', Reduktion = 0, Gyllemængde = sum(merge_dat$TotGødnabDyr[merge_dat$StaldNavn == 'Drænet gulv + spalter (50/50)'])/1000))

køl_bioforgas$Total <- køl_bioforgas$Stald + køl_bioforgas$Lager
køl_bioforgas.mod <- køl_bioforgas %>% group_by(Staldtype) %>% mutate(Reduktion = (1 - (Total[Reference == 'Nej']/Total[Reference == 'Ja'])) * 100, Reduktion_CO2 = (Total[Reference == 'Ja'] - Total[Reference == 'Nej']) * GWP_CH4 + Fortrængning) %>% ungroup()
køl_bioforgas.sum <- køl_bioforgas.mod %>% group_by(Dyrekategori, Reference) %>% summarise(Stald = sum(Stald * Gyllemængde)/ sum(Gyllemængde), 
                                                                                       Lager = sum(Lager * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Total = sum(Total * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Reduktion = sum(Reduktion * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Reduktion_CO2 = sum(Reduktion_CO2 * Gyllemængde)/ sum(Gyllemængde),
                                                                                       Gyllemængde = sum(Gyllemængde))
køl_bioforgas.fin <- bind_rows(køl_bioforgas.mod, køl_bioforgas.sum)

køl_bioforgas.fin[, c('Stald','Lager','Total')] <- round(køl_bioforgas.fin[, c('Stald','Lager','Total')], 3)
køl_bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')] <- round(køl_bioforgas.fin[, c('Reduktion', 'Reduktion_CO2', 'Fortrængning')], 3)
køl_bioforgas.fin$Reduktion[køl_bioforgas.fin$Reference == 'Ja'] <- 0
køl_bioforgas.fin$Reduktion_CO2 <- round(køl_bioforgas.fin$Reduktion_CO2, 3)
køl_bioforgas.fin$Reduktion_CO2[køl_bioforgas.fin$Reference == 'Ja'] <- 0
køl_bioforgas.fin$Gyllemængde <- round(køl_bioforgas.fin$Gyllemængde, 3)

køl_bioforgas.fin <- select(køl_bioforgas.fin, c('Dyretype','Staldtype', 'Reference', 'Stald', 'Lager', 'Total', 'Reduktion', 'Fortrængning', 'Reduktion_CO2', 'Gyllemængde')) 
køl_bioforgas.fin$Staldtype <- gsub('Løbe afd.', 'Drægtighedstalde', køl_bioforgas.fin$Staldtype)
køl_bioforgas.fin$Staldtype <- gsub('Fare afd.', 'Farestalde', køl_bioforgas.fin$Staldtype)
køl_bioforgas.fin$Dyretype[is.na(køl_bioforgas.fin$Dyretype)] <- c('Alle svin', 'Alle svin')

køl_bioforgas.fin$udbredelse <- 0
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Kvæg'] = round((kvæg_biogas)*0 ,3) 
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Alle kvæg'] = round((kvæg_biogas)*0 ,3)  
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Slagtesvin'] = round((svin_biogas) * (slagtesvin_gyllekøl/100),3)
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Søer'] = round((svin_biogas) * (søer_gyllekøl/100),3)
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Smågrise'] = round((svin_biogas) * (smågrise_gyllekøl/100),3)
køl_bioforgas.fin$udbredelse[køl_bioforgas.fin$Dyretype == 'Alle svin'] = round((svin_biogas) * ((smågrise_gyllekøl * smågrise_ab_dyr + slagtesvin_gyllekøl * slagtesvin_ab_dyr + søer_gyllekøl * søer_ab_dyr)/svin_ab_dyr/100),3)
køl_bioforgas.fin$total_red <- round(((100-køl_bioforgas.fin$udbredelse)/100) * køl_bioforgas.fin$Gyllemængde * køl_bioforgas.fin$Reduktion_CO2/1000,3)

dat <- c(list('Udbredelse' = Gødning_ab_dyr, "hyppigudslusning" = hyp_udsl.fin, "bioforgasning" = bioforgas.fin, 
              "hyppigudslusning+bioforgasning" = hyp_bioforgas.fin, "køling+bioforgasning" = køl_bioforgas.fin,
              "hyppigudslusning+afbrænding" = hyp_afbrænd.fin, "hyppigudslusning+lavdosis" = hyp_lavdos.fin,
              "hyppigudslusning+oxidation" = hyp_ox.fin, "staldforsuring" = staldforsur.fin,
              "afbrændning" = afbrænd.fin, "oxidation" = ox.fin, "lavdosis" = lavdos.fin))

write.xlsx(dat, '../output/tables.xlsx')

