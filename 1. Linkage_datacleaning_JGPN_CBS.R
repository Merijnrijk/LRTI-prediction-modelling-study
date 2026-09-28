## Loading data

setwd('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1')
E <- read.csv('ELEMENT_PM1_dataset.csv', header = T, sep = ',') # Load cohort file

## Loading packages
library(tidyverse)
library(haven)
library(lubridate)

# Select relevant cohort for ELEMENT
E$start_epi <- dmy(E$start_epi)
E <- filter(E, leeftijd_epi >= 40 & (start_icpc == 'R78' | start_icpc == 'R81') & (start_epi >= '2016-01-01' & start_epi <= '2019-12-31'))
E <- E %>%
  rename(rin = RINPersoon) %>%
  arrange(rin, start_epi) %>%
  group_by(rin) %>%
  mutate(episode = row_number()) %>%
  ungroup()
E <- filter(E, episode == 1) # Only first episode per individual

## Extract rin numbers
rin <- subset(E, select = c('rin', 'indexdate'))
rin$indexdate <- ymd(rin$indexdate)

## Load files with hospitalisation and mortality data (hospitalisation also for hospitalisation <1 year)
# Hospitalisation
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2015')
ho15 <- read.csv('LBZbasis2015TABV2.csv')
ho15 <- ho15 %>%
  rename(rin = RINPERSOON)
ho15 <- left_join(rin, ho15, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2016')
ho16 <- read.csv('LBZbasis2016TABV1.csv')
ho16 <- ho16 %>%
  rename(rin = RINPERSOON)
ho16 <- left_join(rin, ho16, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2017')
ho17 <- read.csv('LBZbasis2017TABV1.csv')
ho17 <- ho17 %>%
  rename(rin = RINPERSOON)
ho17 <- left_join(rin, ho17, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2018')
ho18 <- read.csv('LBZbasis2018TABV2.csv')
ho18 <- ho18 %>%
  rename(rin = RINPERSOON)
ho18 <- left_join(rin, ho18, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2019')
ho19 <- read.csv('LBZbasis2019TABV1.csv')
ho19 <- ho19 %>%
  rename(rin = RINPERSOON)
ho19 <- left_join(rin, ho19, by = 'rin')
setwd('G:/GezondheidWelzijn/LBZBASISTAB/2020')
ho20 <- read.csv('LBZbasis2020TABV1.csv')
ho20 <- ho20 %>%
  rename(rin = RINPERSOON)
ho20 <- left_join(rin, ho20, by = 'rin')
# Mortality
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2016')
mo16 <- read.csv('GBAOVERLIJDENTAB2016V1.csv', header = T, sep = ',')
mo16 <- mo16 %>%
  rename(rin = RINPERSOON)
mo16 <- left_join(rin, mo16, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2017')
mo17 <- read.csv('GBAOVERLIJDEN2017TABV1.csv', header = T, sep = ',')
mo17 <- mo17 %>%
  rename(rin = RINPERSOON)
mo17 <- left_join(rin, mo17, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2018')
mo18 <- read.csv('GBAOVERLIJDEN2018TABV1.csv', header = T, sep = ',')
mo18<- mo18 %>%
  rename(rin = RINPERSOON)
mo18 <- left_join(rin, mo18, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2019')
mo19 <- read.csv('GBAOVERLIJDEN2019TABV1.csv', header = T, sep = ',')
mo19 <- mo19 %>%
  rename(rin = RINPERSOON)
mo19 <- left_join(rin, mo19, by = 'rin')
setwd('G:/Bevolking/GBAOVERLIJDENTAB/2020')
mo20 <- read.csv('GBAOVERLIJDEN2020TABV1.csv', header = T, sep = ',')
mo20 <- mo20 %>%
  rename(rin = RINPERSOON)
mo20 <- left_join(rin, mo20, by = 'rin')

## Select hospitalisations within 30 days from indexdate
# 2016
ho16$LBZOpnamedatum <- ymd(ho16$LBZOpnamedatum)
ho16 <- ho16 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= indexdate & LBZOpnamedatum - indexdate  <= 30, LBZOpnamedatum, NA))
  )
ho1 <- filter(ho16, !is.na(opnamedatum) == T)
# 2017
ho17$LBZOpnamedatum <- ymd(ho17$LBZOpnamedatum)
ho17 <- ho17 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= indexdate & LBZOpnamedatum - indexdate  <= 30, LBZOpnamedatum, NA))
  )
ho2 <- filter(ho17, !is.na(opnamedatum) == T)
# 2018
ho18$LBZOpnamedatum <- ymd(ho18$LBZOpnamedatum)
ho18 <- ho18 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= indexdate & LBZOpnamedatum - indexdate  <= 30, LBZOpnamedatum, NA))
  )
ho3 <- filter(ho18, !is.na(opnamedatum) == T)
# 2019
ho19$LBZOpnamedatum <- ymd(ho19$LBZOpnamedatum)
ho19 <- ho19 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= indexdate & LBZOpnamedatum - indexdate  <= 30, LBZOpnamedatum, NA))
  )
ho4 <- filter(ho19, !is.na(opnamedatum) == T)
# 2020
ho20$LBZOpnamedatum <- ymd(ho20$LBZOpnamedatum)
ho20 <- ho20 %>%
  mutate(
    opnamedatum = as.Date(ifelse(LBZOpnamedatum >= indexdate & LBZOpnamedatum - indexdate  <= 30, LBZOpnamedatum, NA))
  )
ho5 <- filter(ho20, !is.na(opnamedatum) == T)
# Keep complete info for sensitivity analysis
ho_complete <- rbind(ho1, ho2, ho3, ho4, ho5)
# Keep relevant info
ho1 <- subset(ho1, select = c('rin', 'indexdate', 'opnamedatum'))
ho2 <- subset(ho2, select = c('rin', 'indexdate', 'opnamedatum'))
ho3 <- subset(ho3, select = c('rin', 'indexdate', 'opnamedatum'))
ho4 <- subset(ho4, select = c('rin', 'indexdate', 'opnamedatum'))
ho5 <- subset(ho5, select = c('rin', 'indexdate', 'opnamedatum'))
# Merge
ho <- rbind(ho1, ho2, ho3, ho4, ho5)
# Only first hospitalisation per individual
ho <- ho %>%
  arrange(rin, opnamedatum) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
ho <- filter(ho, n == 1)
ho <- subset(ho, select = -n)
rm(ho1, ho2, ho3, ho4, ho5)

## Select mortality within 30 days from indexdate
#2016
mo16$GBADatumOverlijden <- ymd(mo16$GBADatumOverlijden)
mo16 <- mo16 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= indexdate & GBADatumOverlijden - indexdate <= 30, GBADatumOverlijden, NA))
  )
mo16 <- filter(mo16, !is.na(ovldat) == T)
#2017
mo17$GBADatumOverlijden <- ymd(mo17$GBADatumOverlijden)
mo17 <- mo17 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= indexdate & GBADatumOverlijden - indexdate <= 30, GBADatumOverlijden, NA))
  )
mo17 <- filter(mo17, !is.na(ovldat) == T)
#2018
mo18$GBADatumOverlijden <- ymd(mo18$GBADatumOverlijden)
mo18 <- mo18 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= indexdate & GBADatumOverlijden - indexdate <= 30, GBADatumOverlijden, NA))
  )
mo18 <- filter(mo18, !is.na(ovldat) == T)
#2019
mo19$GBADatumOverlijden <- ymd(mo19$GBADatumOverlijden)
mo19 <- mo19 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= indexdate & GBADatumOverlijden - indexdate <= 30, GBADatumOverlijden, NA))
  )
mo19 <- filter(mo19, !is.na(ovldat) == T)
#2020
mo20$GBADatumOverlijden <- ymd(mo20$GBADatumOverlijden)
mo20 <- mo20 %>%
  mutate(
    ovldat = as.Date(ifelse(GBADatumOverlijden >= indexdate & GBADatumOverlijden - indexdate <= 30, GBADatumOverlijden, NA))
  )
mo20 <- filter(mo20, !is.na(ovldat) == T)
mo <- rbind(mo16, mo17, mo18, mo19, mo20)
mo <- subset(mo, select = c('rin', 'indexdate', 'ovldat'))
mo <- mo %>%
  arrange(rin, ovldat) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
mo <- filter(mo, n == 1)
mo <- subset(mo, select = -n)

## Create candidate predictor hospitalisation <1 year from indexdate
ho15 <- subset(ho15, select = c('rin', 'indexdate', 'LBZOpnamedatum'))
ho16 <- subset(ho16, select = c('rin', 'indexdate', 'LBZOpnamedatum'))
ho17 <- subset(ho17, select = c('rin', 'indexdate', 'LBZOpnamedatum'))
ho18 <- subset(ho18, select = c('rin', 'indexdate', 'LBZOpnamedatum'))
ho19 <- subset(ho19, select = c('rin', 'indexdate', 'LBZOpnamedatum'))
ho15$LBZOpnamedatum <- ymd(ho15$LBZOpnamedatum)
ho16$LBZOpnamedatum <- ymd(ho16$LBZOpnamedatum)
ho17$LBZOpnamedatum <- ymd(ho17$LBZOpnamedatum)
ho18$LBZOpnamedatum <- ymd(ho18$LBZOpnamedatum)
ho19$LBZOpnamedatum <- ymd(ho19$LBZOpnamedatum)
ho_vg <- rbind(ho15, ho16, ho17, ho18, ho19)
ho_vg <- ho_vg %>%
  mutate(opname_1j = ifelse(LBZOpnamedatum < indexdate & indexdate - LBZOpnamedatum <= 365, 1, 0))
ho_vg <- filter(ho_vg, opname_1j == 1)
ho_vg <- ho_vg %>%
  arrange(rin, indexdate) %>%
  group_by(rin) %>%
  mutate(n = row_number()) %>%
  ungroup()
ho_vg <- filter(ho_vg, n == 1)
ho_vg <- subset(ho_vg, select = -c(n, LBZOpnamedatum))

# Merge all dataframes
E$indexdate <- ymd(E$indexdate)
E <- left_join(E, ho_vg, by = c('rin', 'indexdate'))
E <- left_join(E, ho, by = c('rin', 'indexdate'))
E <- left_join(E, mo, by = c('rin', 'indexdate'))
E <- E %>%
  mutate(
    model1_outcome = ifelse(!is.na(opnamedatum) == T | !is.na(ovldat) == T, 1, 0)
  )
E <- E %>%
  mutate(
    model1_outcome_date = case_when(
      !is.na(opnamedatum) & is.na(ovldat) ~ opnamedatum,
      is.na(opnamedatum) & !is.na(ovldat) ~ ovldat,
      !is.na(opnamedatum) & !is.na(ovldat) ~ opnamedatum,
      is.na(opnamedatum) & is.na(ovldat) ~ NA
    ),
    model1_outcome_hosp = ifelse(!is.na(opnamedatum) == T, 1, 0),
    model1_outcome_mort = ifelse(!is.na(ovldat) == T, 1, 0),
    model1_outcome_day0 = ifelse(indexdate == model1_outcome_date, 1, 0)
  )
E$tto <- as.numeric(E$model1_outcome_date-E$indexdate)
summary(E$tto)

## Data cleaning of predictors
E$ami <- dmy(E$Comorb_acuut_myocard)
E$ang_pect <- dmy(E$Comorb_ang_pect)
E$overige_chron_hartziekte <- dmy(E$Comorb_and_chron_hart)
E$diabetes <- dmy(E$Comorb_DM)
E$dec_cordis <- dmy(E$Comorb_decomp_cardis)
E$cva <- dmy(E$Comorb_CVA)
E$tia <- dmy(E$Comorb_TIA)
E$longemb <- dmy(E$Comorb_Longembolie)
E$dvt <- dmy(E$Comorb_DVT)
E$atriumfib <- dmy(E$Comorb_boezemfibrill)
E$claud_intermit <- dmy(E$Comorb_claud_intermit)
E$dementie <- dmy(E$Comorb_dementie)
E$geheug_stoorn <- dmy(E$Comorb_geheugen_stoorn)
E$copd <- dmy(E$Comorb_COPD)
E$astma <- dmy(E$Comorb_astma)
E$pneumonie <- dmy(E$Comorb_pneumo)
E$malign_excl_huid <- dmy(E$Comorb_malign_excl_huid)
E$med_syst_glucocort <- dmy(E$Chron_med_Syst_glucocort)
E$med_immunosupp <- dmy(E$Chron_med_Immunosupp)
E$med_laba <- dmy(E$Chron_med_LABA)
E$med_lama <- dmy(E$Chron_med_LAMA)
E$med_laba_lama <- dmy(E$Chron_med_Combi_LABA_LAMA)
E$med_ics <- dmy(E$Chron_med_ICS)
E$med_ics_laba <- dmy(E$Chron_med_Combi_ICS_LABA)
E$med_ics_laba_lama <- dmy(E$Chron_med_Triple_ICS_LABA_LAMA)
E$gv_vacc_dt <- dmy(E$gv_vacc_dt)
E <- E %>%
  mutate(influenza_vacc = ifelse(gv_vacc_dt < start_epi & start_epi - gv_vacc_dt <= 365, 1, 0))
E <- E %>%
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
E <- E %>%
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

## For sensitivity analysis (cardiorespiratory hospitalisations)
# Merge ICD codes of diag5ICD and diag5ICD_imp to columns without '_imp' (variables: diag5ICD, hoofddiagnose, primaire_diagnose)
admissions <- ho_complete
admissions$primaire_diagnose_imp[admissions$primaire_diagnose_imp==0] <- 'N'
admissions$primaire_diagnose_imp[admissions$primaire_diagnose_imp==1] <- 'J'
admissions <- admissions %>%
  mutate(
    diag5ICD = ifelse(admissions$diag5ICD=='' & admissions$diag5ICD_imp!='', admissions$diag5ICD_imp, admissions$diag5ICD),
    hoofddiagnose = ifelse(admissions$hoofddiagnose=='' & admissions$hoofddiagnose_imp!='', admissions$hoofddiagnose_imp, admissions$hoofddiagnose),
    primaire_diagnose = ifelse(admissions$primaire_diagnose=='' & admissions$primaire_diagnose_imp!='', admissions$primaire_diagnose_imp, admissions$primaire_diagnose)
  )

# Include secondary ICD codes of admission to cohort files
# ICD from 'admissions'
icd <- admissions %>%
  select(c(rin, indexdate, opnamenummer, opnamedatum, diag5ICD)) # select relevant variables from admissions
icd <- icd %>%
  group_by(rin, indexdate, opnamenummer) %>%
  mutate(
    volgnummer = row_number()
  ) # Generate sequential number for multiple ICD codes within a single admission
icd <- icd %>%
  pivot_wider(names_from = volgnummer, values_from = diag5ICD) # Pivot dataframe in order to have a single admission per row, multiple ICD codes are numbered using the sequential numbers
colnames(icd)[5:80] <- paste('ICD', colnames(icd)[5:80], sep = '') # Rename column names with ICD codes with prefix 'ICD'
icd$indexdate <- ymd(icd$indexdate) # Change class of indexdate
colnames(icd)[3] <- paste('X1_', colnames(icd)[3], sep = '')

# Create variable to indicate cardiorespiratory hospitalisation based on ICD codes of admissions
#- Any (primary or secondary) ICD codes starting with 'J' (respiratory illness) or 'I' (cardiovascular illness)
## For cohort ##
icd <- icd %>%
  mutate(
    ICD_J = as.integer(
      if_any(5:80, ~ grepl("^[IJ]", .x)))) # Create variable for any ICD code that starts with J or I
table(icd$ICD_J == 1 & icd$model1_outcome_hosp == 1) # Number of admssions with cardiorespiratory hospitalisation or all-cause mortality

## Merge ICD codes with cohort/E files ##
E <- left_join(E, icd, by = c('rin', 'indexdate'), relationship = 'one-to-one')
rm(icd) # remove unecessary object

# save dataframe
write.csv(E, 'H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/ELEMENT_PM1_dataset.csv')
