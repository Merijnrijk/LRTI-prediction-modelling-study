## Loading data

setwd('H:/Merijn/ANHA')
anha <- read.csv('9199_UMCU_ELEMENT_CBS9199_ANHA_20250725_ttpCBKV1.csv', header = T, sep = ';')

## Loading packages
library(tidyverse)
library(haven)

## Select relevant cohort for ELEMENT (NB: last 30 days of 2023 for follow-up)
anha$start_epi <- dmy(anha$start_epi)
anha <- filter(anha, leeftijd_epi >= 40 & (start_icpc == 'R78' | start_icpc == 'R81') & (start_epi >= '2022-01-01' & start_epi <= '2023-12-01'))
anha <- anha %>%
  rename(rin = RINPersoon) %>%
  arrange(rin, start_epi) %>%
  group_by(rin) %>%
  mutate(episode = row_number()) %>%
  ungroup()
anha <- filter(anha, episode == 1)

## Extract rin numbers
rin <- subset(anha, select = c('rin', 'start_epi'))

## Load files with hospitalisation and mortality data (hospitalisation also for hospitalisation <1 year)
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2021/')
ho21 <- read.csv('LBZBASIS2021TABV1.csv')
ho21 <- ho21 %>%
  rename(rin = RINPERSOON)
ho21 <- left_join(rin, ho21, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2022/')
ho22 <- read.csv('LBZBASIS2022TABV1.csv')
ho22 <- ho22 %>%
  rename(rin = RINPERSOON)
ho22 <- left_join(rin, ho22, by = 'rin')
setwd('H:/Merijn/ANHA/microdata')
ho23 <- read.csv('ho23.csv', header = T, sep = ';')
ho23 <- ho23 %>%
  rename(rin = RINPERSOON)
ho23 <- left_join(rin, ho23, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2022/')
mo22 <- read.csv('GBAOVERLIJDEN2022TABV1.csv', header = T, sep = ',')
mo22 <- mo22 %>%
  rename(rin = RINPERSOON)
mo22 <- left_join(rin, mo22, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2023/')
mo23 <- read.csv('GBAOVERLIJDEN2023TABV1.csv', header = T, sep = ',')
mo23 <- mo23 %>%
  rename(rin = RINPERSOON)
mo23 <- left_join(rin, mo23, by = 'rin')

## Select only relevant candidate predictors for ANHA
anha <- subset(anha, select = c('rin', 'start_epi', 'leeftijd_epi', 'geslachtx', 'Comorb_DM', 'Comorb_decomp_cardis', 'Comorb_ang_pect', 'Comorb_acuut_myocard', 'Comorb_and_chron_hart', 'Comorb_TIA', 'Comorb_CVA', 'Comorb_Longembolie', 'Comorb_DVT', 'Comorb_boezemfibrill', 'Comorb_claud_intermit', 'Comorb_pneumo', 'Comorb_malign_excl_huid', 'Comorb_COPD', 'Comorb_astma', 'Comorb_dementie', 'Comorb_geheugen_stoorn', 'gv_vacc_dt', 'Chron_med_Syst_glucocort', 'Chron_med_Immunosupp', 'Chron_med_LABA', 'Chron_med_LAMA', 'Chron_med_Combi_LABA_LAMA', 'Chron_med_ICS', 'Chron_med_Combi_ICS_LABA', 'Chron_med_Triple_ICS_LABA_LAMA', 'Voor_epi_AB_J01_ALG', 'Chron_med_Antidepressiva', 'start_icpc', 'smokstatus', 'frailty_index', 'aant_cont_voor_episode'))

## Select hospitalisations within 30 days from indexdate
#2022
ho22$LBZOpnamedatum <- ymd(ho22$LBZOpnamedatum)
ho22 <- ho22 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= start_epi & LBZOpnamedatum - start_epi  <= 30, LBZOpnamedatum, NA))
  )
ho1 <- filter(ho22, !is.na(opnamedatum) == T)
#2023
ho23$LBZOpnamedatum <- ymd(ho23$LBZOpnamedatum)
ho23 <- ho23 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= start_epi & LBZOpnamedatum - start_epi  <= 30, LBZOpnamedatum, NA))
  )
ho2 <- filter(ho23, !is.na(opnamedatum) == T)
# Keep relevant info
ho1 <- subset(ho1, select = c('rin', 'start_epi', 'opnamedatum'))
ho2 <- subset(ho2, select = c('rin', 'start_epi', 'opnamedatum'))
# Merge
ho <- rbind(ho1, ho2)
ho <- ho %>%
  arrange(rin, opnamedatum) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
ho <- filter(ho, n == 1)
ho <- subset(ho, select = -n)
rm(ho1, ho2)

## Select mortality within 30 days from indexdate
#2022
mo22$GBADatumOverlijden <- ymd(mo22$GBADatumOverlijden)
mo22 <- mo22 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= start_epi & GBADatumOverlijden - start_epi <= 30, GBADatumOverlijden, NA))
  )
mo22 <- filter(mo22, !is.na(ovldat) == T)
#2023
mo23$GBADatumOverlijden <- ymd(mo23$GBADatumOverlijden)
mo23 <- mo23 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= start_epi & GBADatumOverlijden - start_epi <= 30, GBADatumOverlijden, NA))
  )
mo23 <- filter(mo23, !is.na(ovldat) == T)
mo <- rbind(mo22, mo23)
mo <- subset(mo, select = c('rin', 'start_epi', 'ovldat'))
mo <- mo %>%
  arrange(rin, ovldat) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
mo <- filter(mo, n == 1)
mo <- subset(mo, select = -n)

## Create candidate predictor hospitalisation <1 year from indexdate
ho21 <- subset(ho21, select = c('rin', 'start_epi', 'LBZOpnamedatum'))
ho22 <- subset(ho22, select = c('rin', 'start_epi', 'LBZOpnamedatum'))
ho23 <- subset(ho23, select = c('rin', 'start_epi', 'LBZOpnamedatum'))
ho21$LBZOpnamedatum <- ymd(ho21$LBZOpnamedatum)
ho_vg <- rbind(ho21, ho22, ho23)
ho_vg <- ho_vg %>%
  mutate(opname_1j = ifelse(LBZOpnamedatum < start_epi & start_epi - LBZOpnamedatum <= 365, 1, 0))
ho_vg <- filter(ho_vg, opname_1j == 1)
ho_vg <- ho_vg %>%
  arrange(rin, start_epi) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
ho_vg <- filter(ho_vg, n == 1)
ho_vg <- subset(ho_vg, select = -c(n, LBZOpnamedatum))

# Merge all dataframes
anha <- left_join(anha, ho_vg, by = c('rin', 'start_epi'))
anha <- left_join(anha, ho, by = c('rin', 'start_epi'))
anha <- left_join(anha, mo, by = c('rin', 'start_epi'))
anha <- anha %>%
  mutate(
    outcome = ifelse(!is.na(opnamedatum) == T | !is.na(ovldat) == T, 1, 0)
  )
anha <- anha %>%
  mutate(
    outcome_dt = case_when(
      !is.na(opnamedatum) & is.na(ovldat) ~ opnamedatum,
      is.na(opnamedatum) & !is.na(ovldat) ~ ovldat,
      !is.na(opnamedatum) & !is.na(ovldat) ~ opnamedatum,
      is.na(opnamedatum) & is.na(ovldat) ~ NA
    )
  )
anha$tto <- as.numeric(anha$outcome_dt-anha$start_epi)
summary(anha$tto)
anha <- anha %>%
  mutate(
    outcome_hosp = ifelse(!is.na(opnamedatum), 1, 0),
    outcome_mort = ifelse(!is.na(ovldat), 1, 0)
  )

## Data cleaning of predictors
anha$ami <- dmy(anha$Comorb_acuut_myocard)
anha$ang_pect <- dmy(anha$Comorb_ang_pect)
anha$overige_chron_hartziekte <- dmy(anha$Comorb_and_chron_hart)
anha$diabetes <- dmy(anha$Comorb_DM)
anha$dec_cordis <- dmy(anha$Comorb_decomp_cardis)
anha$cva <- dmy(anha$Comorb_CVA)
anha$tia <- dmy(anha$Comorb_TIA)
anha$longemb <- dmy(anha$Comorb_Longembolie)
anha$dvt <- dmy(anha$Comorb_DVT)
anha$atriumfib <- dmy(anha$Comorb_boezemfibrill)
anha$claud_intermit <- dmy(anha$Comorb_claud_intermit)
anha$dementie <- dmy(anha$Comorb_dementie)
anha$geheug_stoorn <- dmy(anha$Comorb_geheugen_stoorn)
anha$copd <- dmy(anha$Comorb_COPD)
anha$astma <- dmy(anha$Comorb_astma)
anha$pneumonie <- dmy(anha$Comorb_pneumo)
anha$malign_excl_huid <- dmy(anha$Comorb_malign_excl_huid)
anha$med_syst_glucocort <- dmy(anha$Chron_med_Syst_glucocort)
anha$med_immunosupp <- dmy(anha$Chron_med_Immunosupp)
anha$med_laba <- dmy(anha$Chron_med_LABA)
anha$med_lama <- dmy(anha$Chron_med_LAMA)
anha$med_laba_lama <- dmy(anha$Chron_med_Combi_LABA_LAMA)
anha$med_ics <- dmy(anha$Chron_med_ICS)
anha$med_ics_laba <- dmy(anha$Chron_med_Combi_ICS_LABA)
anha$med_ics_laba_lama <- dmy(anha$Chron_med_Triple_ICS_LABA_LAMA)
anha$gv_vacc_dt <- dmy(anha$gv_vacc_dt)
anha <- anha %>%
  mutate(influenza_vacc = ifelse(gv_vacc_dt < start_epi & start_epi - gv_vacc_dt <= 365, 1, 0))
anha <- anha %>%
  mutate(
    ami = ifelse(!is.na(ami) & ami < start_epi, 1, 0),
    ang_pect = ifelse(!is.na(ang_pect) & ang_pect < start_epi, 1, 0),
    overige_chron_hartziekte = ifelse(!is.na(overige_chron_hartziekte) & overige_chron_hartziekte < start_epi, 1, 0),
    diabetes = ifelse(!is.na(diabetes) & diabetes < start_epi, 1, 0),
    dec_cordis = ifelse(!is.na(dec_cordis) & dec_cordis < start_epi, 1, 0),
    cva = ifelse(!is.na(cva) & cva < start_epi, 1, 0),
    tia = ifelse(!is.na(tia) & tia < start_epi, 1, 0),
    longemb = ifelse(!is.na(longemb) & longemb < start_epi, 1, 0),
    dvt = ifelse(!is.na(dvt) & dvt < start_epi, 1, 0),
    atriumfib = ifelse(!is.na(atriumfib) & atriumfib < start_epi, 1, 0),
    claud_intermit = ifelse(!is.na(claud_intermit) & claud_intermit < start_epi, 1, 0),
    dementie = ifelse(!is.na(dementie) & dementie < start_epi, 1, 0),
    geheug_stoorn = ifelse(!is.na(geheug_stoorn) & geheug_stoorn < start_epi, 1, 0),
    copd = ifelse(!is.na(copd) & copd < start_epi, 1, 0),
    astma = ifelse(!is.na(astma) & astma < start_epi, 1, 0),
    pneumonie = ifelse(!is.na(pneumonie) & pneumonie < start_epi, 1, 0),
    malign_excl_huid = ifelse(!is.na(malign_excl_huid) & malign_excl_huid < start_epi, 1, 0),
    med_syst_glucocort = ifelse(!is.na(med_syst_glucocort) & med_syst_glucocort < start_epi, 1, 0),
    med_immunosupp = ifelse(!is.na(med_immunosupp) & med_immunosupp < start_epi, 1, 0),
    med_laba = ifelse(!is.na(med_laba) & med_laba < start_epi, 1, 0),
    med_lama = ifelse(!is.na(med_lama) & med_lama < start_epi, 1, 0),
    med_laba_lama = ifelse(!is.na(med_laba_lama) & med_laba_lama < start_epi, 1, 0),
    med_ics = ifelse(!is.na(med_ics) & med_ics < start_epi, 1, 0),
    med_ics_laba = ifelse(!is.na(med_ics_laba) & med_ics_laba < start_epi, 1, 0),
    med_ics_laba_lama = ifelse(!is.na(med_ics_laba_lama) & med_ics_laba_lama < start_epi, 1, 0),
  )
anha <- anha %>%
  mutate(
    coronaire_hartziekte = ifelse(ami == 1 | ang_pect == 1 | overige_chron_hartziekte == 1, 1, 0),
    cva_tia = ifelse(cva == 1 | tia == 1, 1, 0),
    le_dvt = ifelse(longemb == 1 | dvt == 1, 1, 0),
    copd_astma = ifelse(copd == 1 | astma == 1, 1, 0),
    dementie = ifelse(dementie == 1 | geheug_stoorn == 1, 1, 0),
    immunosupp = ifelse(med_syst_glucocort == 1 | med_immunosupp == 1, 1, 0),
    inhalatiemed = ifelse(med_laba == 1 | med_lama == 1 | med_laba_lama == 1 | med_ics == 1 | med_ics_laba == 1 | med_ics_laba_lama == 1, 1, 0)
  ) %>%
  mutate(
    aant_hvz = 1*diabetes+1*dec_cordis+1*coronaire_hartziekte+1*cva_tia+1*le_dvt+1*atriumfib+1*claud_intermit
  ) %>%
  mutate(
    aant_hvz = case_when(
      aant_hvz == 0 ~ 0,
      aant_hvz == 1 ~ 1,
      aant_hvz >=2 ~ 2
    ))

# save dataframe
write.csv(anha, 'H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/External_validation_ cohort.csv')
