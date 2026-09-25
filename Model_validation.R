## First run development script up to model development (step 7) ##

## Load validation cohort
library(tidyverse)
setwd('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1')
anha <- read.csv('External_validation_ cohort.csv')

## Some last datacleaning
anha$start_epi <- ymd(anha$start_epi)
anha <- anha %>%
  rename(
    geslacht = geslachtx,
    leeftijd = leeftijd_epi
  ) %>%
  mutate(
    Chron_med_Antidepressiva = ifelse(Chron_med_Antidepressiva == '', NA, Chron_med_Antidepressiva),
    Voor_epi_AB_J01_ALG = ifelse(Voor_epi_AB_J01_ALG == '', NA, Voor_epi_AB_J01_ALG)
  ) %>%
  mutate(
    ab_alg_voor_epi = ifelse(!is.na(Voor_epi_AB_J01_ALG) == T, 1, 0),
    med_antidepr = ifelse(!is.na(Chron_med_Antidepressiva) == T, 1, 0),
    opname_1j = ifelse(!is.na(opname_1j), opname_1j, 0),
    influenza_vacc = ifelse(!is.na(influenza_vacc), influenza_vacc, 0),
    frailty_index = ifelse(is.na(frailty_index) == T, 0, frailty_index),
    smokstatus = case_when(
      smokstatus == 1 ~ 1,
      smokstatus == 2 ~ 2,
      smokstatus == 3 ~ 3,
      TRUE ~ 4
    ),
    aant_cont_voor_episode = ifelse(is.na(aant_cont_voor_episode) == T, 0, aant_cont_voor_episode),
    jaar = year(start_epi),
    outcome_hosp = ifelse(!is.na(opnamedatum) == T, 1, 0),
    outcome_mort = ifelse(!is.na(ovldat) == T, 1, 0)
  )
anha <- anha %>%
  mutate(
    age_group = case_when(
      leeftijd < 61 ~ 1,
      leeftijd >60 & leeftijd <81 ~ 2,
      leeftijd >80 ~ 3
    ))
anha <- anha %>%
  rename(opname_1j_voor_episode = opname_1j)
anha <- anha %>%
  mutate(
    multimorbidity = as.numeric(aant_hvz + malign_excl_huid + copd_astma + dementie)
  )

## Table 1 of validation cohort
library(tableone)
factorvars <- c('geslacht', 'jaar', 'diabetes', 'smokstatus', 'malign_excl_huid', 'pneumonie', 'dementie', 'opname_1j', 'immunosupp', 'inhalatiemed', 'med_antidepr', 'ab_alg_voor_epi', 'start_icpc', 'cva_tia', 'le_dvt', 'copd_astma', 'aant_hvz', 'influenza_vacc', 'outcome_hosp', 'outcome_mort')
nonnormal <- c('leeftijd', 'aant_cont_voor_episode', 'frailty_index')
vars <- c(factorvars, nonnormal)
table1 <- CreateTableOne(vars = vars, data = anha, factorVars = factorvars, strata = 'outcome', addOverall = T)
print(table1, nonnormal = nonnormal)
rm(factorvars, nonnormal, vars)

## Time to outcome
ggplot(anha) +
  geom_histogram(mapping = aes(x = tto), colour = 'coral4', fill = 'coral3', na.rm = T, binwidth = 1) +
  xlim(c(-0.5, 30.5)) +
  ylim(c(0, 270)) +
  xlab('Time to outcome (days)') +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  ylab('Number of patients') +
  theme_economist() +
  theme(axis.title.y = element_text(margin = margin(r = 20)), axis.title.x = element_text(margin = margin(t = 20)))
ggsave('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/Export R/Time_to_outcome_validation_cohort.tiff')
tto_val <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(tto_val) <- c('median', 'q25', 'q75')
tto_val[[1]] <- median(anha$tto, na.rm = T)
tto_val[[2]] <- quantile(anha$tto, probs = 0.25, na.rm = T)
tto_val[[3]] <- quantile(anha$tto, probs = 0.75, na.rm = T)
write.csv(tto_val, 'H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/Export R/time_to_outcome_validation.csv')

## Make predictions in validation cohort
# Create table to store predictions
pred_val <- data.frame(matrix(nrow = nrow(anha), ncol = 2))
colnames(pred_val) <- c('basic', 'clinical')
pred_val$outcome <- anha$outcome
pred_val$outcome_hosp <- anha$outcome_hosp
pred_val$outcome_mort <- anha$outcome_mort
pred_val$start_icpc <- anha$start_icpc
pred_val$age_group <- anha$age_group
pred_val$geslacht <- anha$geslacht
pred_val$copd_astma <- anha$copd_astma
pred_val$jaar <- anha$jaar
pred_val$multimorbidity <- anha$multimorbidity
# Create model matrix
x_v <- model.matrix(outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = anha)[,-1]
x_vb <- x1 <- model.matrix(outcome ~ leeftijd*factor(geslacht), data = anha)[,-1]
# Formal check whether model matrices (development and validation cohort) are identical
dim(x[[5]])
dim(x_v)
identical(colnames(x[[5]]), colnames(x_v)) # True
# Store predictions
pred_val[,2] <- as.numeric(predict(fit[[5]], newx = x_v, type = 'response', s = 'lambda.min'))
pred_val[,1] <- as.numeric(predict(fit[[1]], newx = x_vb, type = 'response', s = 'lambda.min'))

## Calculate performance measures at external validation
# Create table
ext_val <- data.frame(matrix(nrow = 3, ncol = 3))
rownames(ext_val) <- c('point', 'lower', 'upper')
colnames(ext_val) <- c('AUC', 'intercept', 'slope')
# Evaluate performance
perf_ext <- val.prob.ci.2(p = pred_val$clinical, y = pred_val$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
perf_ext_blank <- val.prob.ci.2(p = pred_val$clinical, y = pred_val$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = F, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
ext_val[[1]] <- perf_ext$Cindex
ext_val[[2]] <- perf_ext$Calibration$Intercept
ext_val[[3]] <- perf_ext$Calibration$Slope
# Range of predicted risks
ext_val_range <- data.frame(matrix(nrow = 1, ncol = 3))
colnames(ext_val_range) <- c('median', 'min', 'max')
ext_val_range$median <- median(pred_val$clinical)
ext_val_range$min <- min(pred_val$clinical)
ext_val_range$max <- max(pred_val$clinical)
# Create histogram of predicted risks of clinical model
ggplot(pred_val) +
  geom_histogram(mapping = aes(x = clinical), colour = 'coral4', fill = 'coral3', binwidth = 0.01, center = 0) +
  xlab('Predicted risk') +
  ylab('LRTI patients (n)') +
  xlim(0, 0.65) +
  ylim(0, 900) +
  theme_few()
ggsave('Export R/Predicted_risks_external_validation.tiff')
ggplot(pred_val) +
  geom_histogram(mapping = aes(x = basic), colour = 'coral4', fill = 'coral3', binwidth = 0.01, center = 0) +
  xlab('Predicted risk') +
  ylab('LRTI patients (n)') +
  xlim(0, 0.65) +
  ylim(0, 900) +
  theme_few()
ggsave('Export R/Predicted_risks_external_validation_basic.tiff')

## Performance measures per subgroup
pred_val_pneumonia <- filter(pred_val, start_icpc == 'R81')
perf_ext <- val.prob.ci.2(p = pred_val_pneumonia$clinical, y = pred_val_pneumonia$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_bronchitis <- filter(pred_val, start_icpc == 'R78')
perf_ext <- val.prob.ci.2(p = pred_val_bronchitis$clinical, y = pred_val_bronchitis$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_age1 <- filter(pred_val, age_group == 1)
perf_ext <- val.prob.ci.2(p = pred_val_age1$clinical, y = pred_val_age1$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_age2 <- filter(pred_val, age_group == 2)
perf_ext <- val.prob.ci.2(p = pred_val_age2$clinical, y = pred_val_age2$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_age3 <- filter(pred_val, age_group == 3)
perf_ext <- val.prob.ci.2(p = pred_val_age3$clinical, y = pred_val_age3$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_m <- filter(pred_val, geslacht == 'M')
perf_ext <- val.prob.ci.2(p = pred_val_m$clinical, y = pred_val_m$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_f <- filter(pred_val, geslacht == 'V')
perf_ext <- val.prob.ci.2(p = pred_val_f$clinical, y = pred_val_f$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_copd <- filter(pred_val, copd_astma == 1)
perf_ext <- val.prob.ci.2(p = pred_val_copd$clinical, y = pred_val_copd$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_2022 <- filter(pred_val, jaar == 2022)
perf_ext <- val.prob.ci.2(p = pred_val_2022$clinical, y = pred_val_2022$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_2023 <- filter(pred_val, jaar == 2023)
perf_ext <- val.prob.ci.2(p = pred_val_2023$clinical, y = pred_val_2023$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_multimorbidity0 <- filter(pred_val, multimorbidity == 0)
perf_ext <- val.prob.ci.2(p = pred_val_multimorbidity0$clinical, y = pred_val_multimorbidity0$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_multimorbidity1 <- filter(pred_val, multimorbidity == 1)
perf_ext <- val.prob.ci.2(p = pred_val_multimorbidity1$clinical, y = pred_val_multimorbidity1$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
pred_val_multimorbidity2 <- filter(pred_val, multimorbidity >= 2)
perf_ext <- val.prob.ci.2(p = pred_val_multimorbidity2$clinical, y = pred_val_multimorbidity2$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')

## Calculate performance measures for hospitalisaton and mortality separately
# Create table
ext_val_hosp <- data.frame(matrix(nrow = 3, ncol = 3))
rownames(ext_val_hosp) <- c('point', 'lower', 'upper')
colnames(ext_val_hosp) <- c('AUC', 'intercept', 'slope')
# Evaluate performance
perf_ext <- val.prob.ci.2(p = pred_val$clinical, y = pred_val$outcome_hosp, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
ext_val_hosp[[1]] <- perf_ext$Cindex
ext_val_hosp[[2]] <- perf_ext$Calibration$Intercept
ext_val_hosp[[3]] <- perf_ext$Calibration$Slope
# Create table
ext_val_mort <- data.frame(matrix(nrow = 3, ncol = 3))
rownames(ext_val_mort) <- c('point', 'lower', 'upper')
colnames(ext_val_mort) <- c('AUC', 'intercept', 'slope')
# Evaluate performance
perf_ext <- val.prob.ci.2(p = pred_val$clinical, y = pred_val$outcome_mort, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
ext_val_mort[[1]] <- perf_ext$Cindex
ext_val_mort[[2]] <- perf_ext$Calibration$Intercept
ext_val_mort[[3]] <- perf_ext$Calibration$Slope

## Intercept-only recalibration
pred_val$logit <- as.numeric(predict(fit[[5]], newx = x_v, type = 'link', s = 'lambda.min'))
recal <- glm(
  pred_val$outcome ~ 1,
  family = binomial(),
  offset = pred_val$logit
)
coef(recal)
confint(recal)
pred_val$lp_recal <- pred_val$logit + coef(recal)[1]
pred_val$recal_risk <- plogis(pred_val$lp_recal)
perf_recal <- val.prob.ci.2(p = pred_val$recal_risk, y = pred_val$outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
mean(pred_val$clinical)
mean(pred_val$recal_risk)

## Save tables
setwd('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/Export R')
write.csv(ext_val, 'external_validation_performance.csv')
write.csv(ext_val_range, 'external_validation_range_risks.csv')
write.csv(ext_val_hosp, 'external_validation_performance_hosp.csv')
write.csv(ext_val_mort, 'external_validation_performance_mort.csv')

# Decision curve analysis of original (non-recalibrated) model
dca <- dcurves::dca(
  formula = outcome ~ clinical,
  data = pred_val,
  thresholds = seq(0, 0.5, 0.01),
  label = list(clinical = 'Final model')
) |>
  as_tibble() |>
  filter(!is.na(net_benefit)) |>
  ggplot(aes(
    x = threshold,
    y = net_benefit,
    color = label,
    linetype = label,
    size = label
  )) +
  geom_line() +
  xlim(0, 0.5) +
  scale_y_continuous(
    breaks = seq(-0.02, 0.08, 0.02),
    limits = c(-0.02, 0.08)) +
  scale_color_manual(values = c('coral3', 'chartreuse4', 'sienna1', 'cadetblue4')) +
  scale_linetype_manual(values = c('solid', 'solid', 'solid', 'solid')) +
  scale_size_manual(values = c(1, 1, 1, 1)) +
  labs(
    x = 'Threshold probability',
    y = 'Net benefit',
    color = '',
    linetype = '',
    size = ''
  ) +
  theme_few()
dca
ggsave('H:/Merijn/Extern cohort (JHN)/NLP add-on study/Export R/DCA_clinical.tiff')
# Additional data for clinical interpretation of DCA based on risk thresholds 10%-20%-30%-40%-50%
# Proportion of patients labelled as high-risk
sum(pred_val$clinical>=0.10)/6284
sum(pred_val$clinical>=0.20)/6284
sum(pred_val$clinical>=0.30)/6284
sum(pred_val$clinical>=0.40)/6284
sum(pred_val$clinical>=0.50)/6284
# Proportion of true positives
sum(pred_val$clinical>=0.10 & pred_val$outcome == 1)/6284
sum(pred_val$clinical>=0.20 & pred_val$outcome == 1)/6284
sum(pred_val$clinical>=0.30 & pred_val$outcome == 1)/6284
sum(pred_val$clinical>=0.40 & pred_val$outcome == 1)/6284
sum(pred_val$clinical>=0.50 & pred_val$outcome == 1)/6284
# Proportion of false positives
sum(pred_val$clinical>=0.10 & pred_val$outcome == 0)/6284
sum(pred_val$clinical>=0.20 & pred_val$outcome == 0)/6284
sum(pred_val$clinical>=0.30 & pred_val$outcome == 0)/6284
sum(pred_val$clinical>=0.40 & pred_val$outcome == 0)/6284
sum(pred_val$clinical>=0.50 & pred_val$outcome == 0)/6284
# Proportion of false negatives
sum(pred_val$clinical<0.10 & pred_val$outcome == 1)/6284
sum(pred_val$clinical<0.20 & pred_val$outcome == 1)/6284
sum(pred_val$clinical<0.30 & pred_val$outcome == 1)/6284
sum(pred_val$clinical<0.40 & pred_val$outcome == 1)/6284
sum(pred_val$clinical<0.50 & pred_val$outcome == 1)/6284
# Net benefit at given thresholds for final mode, treat all, and treat none
dca2 <- dcurves::dca( # Create DCA object that is not a ggplot object
  formula = outcome ~ clinical,
  data = pred_val,
  thresholds = seq(0, 0.5, 0.01),
  label = list(clinical = 'Final model')
)
net_benefit_table <- dca2 %>%
  as_tibble() %>%
  filter(threshold %in% seq(0.1, 0.5, by = 0.1)) %>%
  select(variable, threshold, net_benefit) %>%
  pivot_wider(
    id_cols = threshold,
    names_from = variable,
    values_from = net_benefit
  )