---
title: "Cohort_construction"
output: html_document
date: '2023-09-15'
---
  
# Step 1: load packages
library(tidyverse)
library(lubridate)
library(expss)

# Step 2: set working directory, load cohort files 
# - 9199_UMCU_ELEMENT_JHN_20230616_1_ttpCBKV1 - kopie.csv = JGPN ELEMENT cohort with RIN numbers 
# - Extern_cohort_11052022.csv = Subjects of JHN extern cohort that could be linked with CBS data, includes mortality data
# - Extern_ho_11052022.csv = Contains all admissions after indexdate of subjects from extern_cohort 
# - Extern_op_11052022.csv = Contains admissions at index date of subjects from extern cohort 
# - Extern_vg_11052022.csv = Contains admissions/medical history before index date in subjects from extern cohort
setwd('H:/Merijn/Extern cohort (JHN)')
extern <- read.csv('9199_UMCU_ELEMENT_JHN_20230616_1_ttpCBKV1 - kopie.csv', header=T, sep=';')
extern_linked <- read.csv('Extern_cohort_11052022 - verrijkt.csv', header=T, sep=',')
admissions <- read.csv('Extern_ho_11052022.csv', header=T, sep=',')
history <- read.csv('Extern_vg_11052022.csv', header=T, sep=',')
index_during_admission <- read.csv('Extern_op_11052022.csv', header=T, sep=',')

# Step 3: 
#- Remove observations without RIN number in extern and extern_linked (could not be linked to hospitalisation/mortality data)
#- Redefine some columns
extern <- subset(extern, !is.na(extern$RINPersoon))
extern_linked <- subset(extern_linked, !is.na(extern_linked$rin))
colnames(extern)[colnames(extern)=='RINPersoon'] = 'rin'
colnames(extern)[colnames(extern)=='start_epi'] = 'indexdate'
extern$indexdate <- dmy(extern$indexdate)
extern_linked$indexdate <- ymd(extern_linked$indexdate)

# Step 5:
# - Merge external cohort (JGPN) with CBS data including hospitalisation and mortality
# - Correct variable class of some variables
cohort <- left_join(extern, extern_linked, by = c('rin', 'indexdate'))
cohort$X1_opnamedatum <- ymd(cohort$X1_opnamedatum)
cohort$op_opnamedatum <- ymd(cohort$op_opnamedatum)
cohort$ovldat <- ymd(cohort$ovldat)
cohort$leeftijd <- as.numeric(cohort$leeftijd)
cohort$indexdate <- ymd(cohort$indexdate)
cohort$eind_epi <- dmy(cohort$eind_epi)
cohort <- cohort %>%
  mutate_each(dmy, ends_with('_dt'))
cohort <- cohort %>%
  mutate_each(dmy, starts_with('ab_j') | starts_with('ab_alg_epi_'))
cohort <- cohort %>%
  mutate_each(ymd, ends_with('datum'))

# Step 6.1:
# Construct variable for:
#- Outcome of model (30-day all-cause hospitalisation or mortality)
#- Hospitalisation within 30 days
#- Mortality within 30 days
cohort <- cohort %>%
  mutate(
    model1_outcome_hosp = ifelse((!is.na(op_opnamedatum) == T & op_opnamedatum == indexdate) | (!is.na(X1_opnamedatum) == T & X1_opnamedatum - indexdate <= 30), 1, 0),
    model1_outcome_mort = ifelse((!is.na(ovldat) == T & ovldat - indexdate <= 30), 1, 0)
  )
cohort <- cohort %>%
  mutate(
    model1_outcome = ifelse(model1_outcome_hosp == 1 | model1_outcome_mort == 1, 1, 0)
  )

# Step 6.2:
#- Create variable with date of model1_outcome
cohort <- cohort %>%
  mutate(
    model1_outcome_date = case_when(
      cohort$model1_outcome == 0 ~ NA,
      cohort$model1_outcome == 1 & cohort$model1_outcome_hosp == 1 & cohort$model1_outcome_day0 == 1 ~ cohort$indexdate,
      cohort$model1_outcome == 1 & cohort$model1_outcome_hosp == 1 & cohort$model1_outcome_day0 == 0  ~ cohort$X1_opnamedatum,
      cohort$model1_outcome == 1 & cohort$model1_outcome_hosp == 0 & cohort$model1_outcome_mort == 1 ~ cohort$ovldat
    )
  )
table(cohort$model1_outcome == 1 & !is.na(cohort$model1_outcome_date) == T)

# Step 7.1: Merge ICD codes of diag5ICD and diag5ICD_imp to columns without '_imp' (variables: diag5ICD, hoofddiagnose, primaire_diagnose)
admissions$primaire_diagnose_imp[admissions$primaire_diagnose_imp==0] <- 'N'
admissions$primaire_diagnose_imp[admissions$primaire_diagnose_imp==1] <- 'J'
admissions <- admissions %>%
  mutate(
    diag5ICD = ifelse(admissions$diag5ICD=='' & admissions$diag5ICD_imp!='', admissions$diag5ICD_imp, admissions$diag5ICD),
    hoofddiagnose = ifelse(admissions$hoofddiagnose=='' & admissions$hoofddiagnose_imp!='', admissions$hoofddiagnose_imp, admissions$hoofddiagnose),
    primaire_diagnose = ifelse(admissions$primaire_diagnose=='' & admissions$primaire_diagnose_imp!='', admissions$primaire_diagnose_imp, admissions$primaire_diagnose)
  )

# Step 7.2: Include secondary ICD codes of admission to cohort files
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

# ICD from 'index_during_admission'
icd2 <- index_during_admission %>%
  select(c(rin, indexdate, opnamenummer, opnamedatum, diag5ICD)) # select relevant variables from admissions
icd2 <- icd2 %>%
  group_by(rin, indexdate, opnamenummer) %>%
  mutate(
    volgnummer = row_number()
  ) # Generate sequential number for multiple ICD codes within a single admission
icd2 <- icd2 %>%
  pivot_wider(names_from = volgnummer, values_from = diag5ICD) # Pivot dataframe in order to have a single admission per row, multiple ICD codes are numbered using the sequential numbers
colnames(icd2)[5:50] <- paste('ICD', colnames(icd2)[5:80], sep = '') # Rename column names with ICD codes with prefix 'ICD'
icd2$indexdate <- ymd(icd2$indexdate) # Change class of indexdate
colnames(icd2)[3] <- paste('X1_', colnames(icd2)[3], sep = '')

# merge two icd dataframes, check for duplicates
icd <- bind_rows(icd, icd2)
rm(icd2)
icd_dups <- icd[c('rin', 'X1_opnamenummer')]
anyDuplicated(icd_dups)
rm(icd_dups)

## Merge ICD codes with cohort/E files ##
cohort <- left_join(cohort, icd, by = c('rin', 'indexdate', 'X1_opnamenummer'), relationship = 'one-to-one')
rm(icd) # remove unecessary object

# Step 7.3: Create variables based on ICD codes of admissions
#- Any (primary or secondary) ICD codes starting with 'J' (respiratory illness) or 'I' (cardiovascular illness)
## For cohort ##
cohort <- cohort %>%
  mutate(
    ICD_J = as.integer(
      if_any(491:566, ~ grepl("^[IJ]", .x)))) # Create variable for any ICD code that starts with J or I
table(cohort$ICD_J == 1 & cohort$model1_outcome_hosp == 1) # Number of admssions with any ICD code for respiratory or circulatory diseases

# Step 8: create variable based on history of admissions
#- Admission within 1 year before indexdate (yes/no)
history$indexdate <- ymd(history$indexdate) # correct variable type to match cohort
history$opnamedatum <- ymd(history$opnamedatum) # correct variable type to match cohort
history_1y <- history %>%
  subset(select = c(rin, indexdate, opnamedatum)) %>% #select necessary columns
  filter(indexdate-opnamedatum <= 365 & indexdate != opnamedatum) %>% # select only hospitalisations <1 year
  mutate(
    opname_1j_voor_episode = 1 # create variable for hospitalisations < 1 year
  ) %>%
  subset(select = -c(opnamedatum)) # remove unecessary column
history_1y_dups <- history_1y[c('rin', 'indexdate')] # remove duplicates (rin + indexdat)
history_1y <- history_1y[!duplicated(history_1y_dups),]# remove duplicates (rin + index)
cohort <- left_join(cohort, history_1y, by = c('rin', 'indexdate')) # merge files
cohort$opname_1j_voor_episode[is.na(cohort$opname_1j_voor_episode)==T] <- 0

# Step 9: Remove unnecessary columns
cohort <- cohort %>%
  subset(select = -c(CBKSoortNr, CBKBeginGeldigheid, CBKEindGeldigheid, CBKMutatieCode, CBKNVerschillen, CBKAfstand, CBKVerschilCode, CBKModel, X, GBAGESLACHT))

# Step 10: Create candidate predictors based on primary care EHR history
cohort <- cohort %>%
  mutate(
    ami = ifelse(!is.na(ami_dt) == T & ami_dt <= indexdate, 1, 0),
    ang_pect = ifelse(!is.na(ang_pect_dt) == T & ang_pect_dt <= indexdate, 1, 0),
    overige_chron_hartziekte = ifelse(!is.na(overige_chron_hartziekte_dt) == T & overige_chron_hartziekte_dt <= indexdate, 1, 0),
    diabetes = ifelse(!is.na(diabetes_dt) == T & diabetes_dt <= indexdate, 1, 0),
    dec_cordis = ifelse(!is.na(dec_cordis_dt) == T & dec_cordis_dt <= indexdate, 1, 0),
    cva = ifelse(!is.na(cva_dt) == T & cva_dt <= indexdate, 1, 0),
    tia = ifelse(!is.na(tia_dt) == T & tia_dt <= indexdate, 1, 0),
    longemb = ifelse(!is.na(longemb_dt) == T & longemb_dt <= indexdate, 1, 0),
    dvt = ifelse(!is.na(dvt_dt) == T & dvt_dt <= indexdate, 1, 0),
    atriumfib = ifelse(!is.na(atriumfib_dt) == T & atriumfib_dt <= indexdate, 1, 0),
    claud_intermit = ifelse(!is.na(claud_intermit_dt) == T & claud_intermit_dt <= indexdate, 1, 0),
    dementie = ifelse(!is.na(dementie_dt) == T & dementie_dt <= indexdate, 1, 0),
    geheug_stoorn = ifelse(!is.na(geheug_stoorn_dt) == T & geheug_stoorn_dt <= indexdate, 1, 0),
    pneumonie = ifelse(!is.na(pneumonie_dt) == T & pneumonie_dt <= indexdate, 1, 0),
    malign_excl_huid = ifelse(!is.na(malign_excl_huid_dt) == T & malign_excl_huid_dt <= indexdate, 1, 0)
  )

# Step 11: Create cohort for model development (E)
E <- subset(cohort, (start_icpc == 'R78' | start_icpc == 'R81') & jaar < 2020 & leeftijd >39)
E$episode <- with(E, ave(as.character(rin), rin, FUN = seq_along)) # sequential number for multiple episodes within one patient
E <- subset(E, episode == 1) # Select only first episode per patient

# Step 12: select only relevant columns for model development of model 1
#- E_PM1: rin, indexdate, age, sex, CVD (diabetes, heart failure, AMI/AP/K76, CVA/TIA, PE/DVT, AF, claudicatio), frailty index, smoking status, malignancy (excl. skin), hospitalisation <1 year, influenza vaccination, corticosteroid use, antibiotic prescription <1 month, diagnosis code, outcome.
#- Rename columns
E_PM1 <- E %>%
  subset(select = c(rin, indexdate, praktnr_crypt, leeftijd, geslachtx, jaar, aant_cont_voor_episode, diabetes, dec_cordis, ami, ang_pect, overige_chron_hartziekte, cva, tia, longemb, dvt, atriumfib, claud_intermit, frailty_index, smokstatus, malign_excl_huid, pneumonie, dementie, geheug_stoorn, opname_1j_voor_episode, Datum_griep_vaccin_1, Datum_griep_vaccin_2, med_syst_glucocort, med_immunosupp, med_laba, med_lama, med_laba_lama, med_ics, med_ics_laba, med_ics_laba_lama, med_antidepr, ab_alg_voor_epi, start_icpc, copd, astma, model1_outcome, model1_outcome_hosp, model1_outcome_mort, model1_outcome_day0, model1_outcome_date, ICD_J))
colnames(E_PM1)[5] <- 'geslacht'

# Step 12: save cohort files
write.csv(cohort, 'ELEMENT_cohort_linked.csv')
write.csv(E, 'ELEMENT_E_linked.csv')
write.csv(codebook_cohort, 'ELEMENT_cohort_linked_codebook.csv')
write.csv(E_PM1, 'ELEMENT_PM1_dataset.csv')
