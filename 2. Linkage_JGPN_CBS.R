## Loading data

setwd('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1')
E <- read.csv('ELEMENT_PM1_dataset.csv', header = T, sep = ',')

## Loading packages
library(tidyverse)
library(haven)

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
# Keep relevant info
ho1 <- subset(ho1, select = c('rin', 'indexdate', 'opnamedatum'))
ho2 <- subset(ho2, select = c('rin', 'indexdate', 'opnamedatum'))
ho3 <- subset(ho3, select = c('rin', 'indexdate', 'opnamedatum'))
ho4 <- subset(ho4, select = c('rin', 'indexdate', 'opnamedatum'))
ho5 <- subset(ho5, select = c('rin', 'indexdate', 'opnamedatum'))
# Merge
ho <- rbind(ho1, ho2, ho3, ho4, ho5)
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
  rename(
    model1_outcome_old = model1_outcome
  ) %>%
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

# save dataframe
write.csv(E, 'H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/ELEMENT_PM1_dataset.csv')
