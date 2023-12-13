# Description: Script for combining activity data
# Author: Frederik Rask Dalby
# Date: 2. oct. 2022

# prepare
rm(list = ls())

library("readxl")
library("openxlsx")

# Import

file <- "Til Anders Peter Adamsen 08092022-3.xlsx"

stald_dat <- read_excel(paste0('../data/', file), sheet = 'N, TAN, TonGødning, VS', skip = 1, col_names = TRUE)
dyr_dat <- read_excel(paste0('../data/', file), sheet = 'Antal dyr', skip = 1, col_names = TRUE)

# remove NA rows
merge_dat <- stald_dat[!is.na(stald_dat$DyrID),]

# add column for N animal
merge_dat$AntalDyr <- NA

# for each row in merge_dat add number of animals by matching with DyrID and StaldID in dyr_dat
for (i in 1:length(merge_dat$StaldID)){
  merge_dat$AntalDyr[i] <- dyr_dat$AntalDyr[dyr_dat$StaldID == merge_dat$StaldID[i] & dyr_dat$DyrID == merge_dat$DyrID[i]]
}

merge_dat$AntalDyr_corr <- merge_dat$AntalDyr
total_søer <- sum(unique(merge_dat$AntalDyr[merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko' ]))
faresøer <- sum(unique(merge_dat$AntalDyr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko')  & merge_dat$StaldID == 64 | merge_dat$StaldID == 65 | merge_dat$StaldID == 78]))

# correct number of farrowing sows
merge_dat$AntalDyr_corr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko') & merge_dat$StaldID == 64 | merge_dat$StaldID == 65 | merge_dat$StaldID == 78 ] <- merge_dat$AntalDyr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko') & merge_dat$StaldID == 64 | merge_dat$StaldID == 65 | merge_dat$StaldID == 78] * total_søer/faresøer
merge_dat$AntalDyr_corr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko') & merge_dat$StaldID != 64 & merge_dat$StaldID != 65 & merge_dat$StaldID != 78] <- merge_dat$AntalDyr[(merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == 'Årssøer, øko') & merge_dat$StaldID != 64 & merge_dat$StaldID != 65 & merge_dat$StaldID != 78] * total_søer/(total_søer-faresøer)

# add totals per year 
merge_dat$TotNabDyr <- merge_dat$NabDyr * merge_dat$AntalDyr_corr
merge_dat$TotTANabDyr <- merge_dat$TANabDyr * merge_dat$AntalDyr_corr
merge_dat$`TotTon gødning ab dyr` <- merge_dat$`Ton gødning ab dyr` * merge_dat$AntalDyr_corr
merge_dat$TotVSStald <- merge_dat$VSStald * merge_dat$AntalDyr_corr 

sum(merge_dat$`TotTon gødning ab dyr`[merge_dat$DyrNavn == 'Årssøer' | merge_dat$DyrNavn == ' Årssøer, øko'])
# change names
colnames(merge_dat) <- c('År','DyrID', 'DyrNavn', 'StaldID', 'StaldNavn', 'GødnID', 'GødnNavn', 'NabDyr', 
                         'TANabDyr', 'GødnabDyr', 'TSabDyr', 'VSStald', 'VSAfgræsning', 'AntalDyr', 'AntalDyr_corr', 'TotNabDyr', 'TotTANabDyr', 'TotGødnabDyr', 'TotVSStald')

# add column for frequent flushing compatibility
merge_dat$Hyppig_udslusning <- 0

# StaldID compatible with frequent flushing
ID_hyp <- c(6, 60, 62, 63, 64, 20, 47, 72, 73)

# add 1 to compatible barns
merge_dat$Hyppig_udslusning[merge_dat$StaldID %in% ID_hyp] <- 1

# add general animal category
merge_dat$DyreType <- 0

ID_svin <- c(12, 13, 14, 40, 41, 42)
ID_kvæg <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 29, 30)

merge_dat$DyreType[which(merge_dat$DyrID %in% ID_svin)] <- "svin"
merge_dat$DyreType[which(merge_dat$DyrID %in% ID_kvæg)] <- "kvæg"

# add coloumn for fraction of barn VS that is compatible with frequent flushing

merge_dat$Hyppig_udslusning_potentiale <- 0 # % VS ab stald ud af total VS ab stald per dyrtype

for (i in 1:length(merge_dat$Hyppig_udslusning)){
  if(merge_dat$DyreType[i] == "svin" & merge_dat$Hyppig_udslusning[i] == 1){
    merge_dat$Hyppig_udslusning_potentiale[i] <- merge_dat$TotGødnabDyr[i]/sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == "svin"]) * 100
  } else if(merge_dat$DyreType[i] == "kvæg" & merge_dat$Hyppig_udslusning[i] == 1){
    merge_dat$Hyppig_udslusning_potentiale[i] <- merge_dat$TotGødnabDyr[i]/sum(merge_dat$TotGødnabDyr[merge_dat$DyreType == "kvæg"]) * 100
  } else{
    merge_dat$Hyppig_udslusning_potentiale[i] <- 0
  }
}

kommentar <- NULL
kommentar$Navn <- c(colnames(merge_dat))
kommentar$enheder <- c('','','','','','','','Kg N per dyr','Kg TAN per dyr', 'Ton per dyr', '%', 'Ton VS per dyr','','','', 
                       'Kg N per år', 'Kg TAN per år', 'Ton per år', 'Ton VS per år','', '', '% af Ton gylle per år per Dyre Type')
kommentar <- as.data.frame(kommentar)

dat <- c(list("data" = merge_dat, "kommentar" = kommentar))

save_file_name <- 'Samlet_data.xlsx'

# save new excel file with merged data
write.xlsx(dat, paste0('../data/', save_file_name), rowNames = TRUE)



