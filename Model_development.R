---
  title: "PM1_development"
output: html_document
date: "2023-10-04"
---

###################################################
######### ELEMENT Prediction model study ##########
###################################################
#> Step 1: loading packages and settings
#> Step 2: loading data
#> Step 3: creating new variables
#> Step 4: baseline table of cohort
#> Step 5: formal sample size calculations
#> Step 6: histogram for timing of outcome
#> Step 7: fit penalized models using stepwise approach
#> Step 8: predictions and calculate apparent performance measures
#> Step 9: internal validation of models by bootstrapping
#> Step 10: sensitivity analysis
#> Step 11: assess incremental predictive value of CVD
#> Step 12: internal-external cross-validation (by year)

#Step 1: load packages, settings
library(dplyr)
library(tidyverse)
library(pROC)
library(rms)
library(glmnet)
library(glmnetUtils)
library(boot)
library(tableone)
library(patchwork)
library(pmsampsize)
library(DescTools)
library(expss)
library(metafor)
library(CalibrationCurves)
library(ggthemes)
library(dcurves)
options(scipen=999, digits=3)

# Step 2: load data file
E <- read.csv('H:/Merijn/Extern cohort (JHN)/ELEMENT_PM_1/ELEMENT_PM1_dataset.csv')

# Step 3: create candidate predictors comprising multiple variables
E$Datum_griep_vaccin_1 <- dmy(E$Datum_griep_vaccin_1)
E$Datum_griep_vaccin_2 <- dmy(E$Datum_griep_vaccin_2)
E <- E %>%
  mutate(
    coronaire_hartziekte = ifelse(ami == 1 | ang_pect == 1 | overige_chron_hartziekte == 1, 1, 0),
    cva_tia = ifelse(cva == 1 | tia == 1, 1, 0),
    le_dvt = ifelse(longemb == 1 | dvt == 1, 1, 0),
    copd_astma = ifelse(copd == 1 | astma == 1, 1, 0),
    smokstatus = ifelse(is.na(smokstatus) == T, 4, smokstatus),
    dementie = ifelse(dementie == 1 | geheug_stoorn == 1, 1, 0),
    immunosupp = ifelse(med_syst_glucocort == 1 | med_immunosupp == 1, 1, 0),
    inhalatiemed = ifelse(med_laba == 1 | med_lama == 1 | med_laba_lama == 1 | med_ics == 1 | med_ics_laba == 1 | med_ics_laba_lama == 1, 1, 0),
    influenza_vacc = ifelse(!is.na(Datum_griep_vaccin_1) == T | !is.na(Datum_griep_vaccin_2) == T, 1, 0)
  ) %>%
  mutate(
    aant_hvz = 1*diabetes+1*dec_cordis+1*coronaire_hartziekte+1*cva_tia+1*le_dvt+1*atriumfib+1*claud_intermit
  ) %>%
  mutate(
    aant_hvz = case_when(
      aant_hvz == 0 ~ 0,
      aant_hvz == 1 ~ 1,
      aant_hvz >=2 ~ 2
    ),
    aant_cont_voor_episode = ifelse(is.na(aant_cont_voor_episode) == T, 0, aant_cont_voor_episode)
  )
E$leeftijd <- as.numeric(E$leeftijd)
E <- E %>%
  mutate(
    outcome_sens = ifelse(model1_outcome_mort == 1 | (model1_outcome == 1 & ICD_J == 1), 1, 0) # Outcome for sensitivity analysis
  )

# Step 4: Baseline table
factorvars <- c('geslacht', 'jaar', 'diabetes', 'smokstatus', 'malign_excl_huid', 'pneumonie', 'dementie', 'opname_1j_voor_episode', 'immunosupp', 'inhalatiemed', 'med_antidepr', 'ab_alg_voor_epi', 'start_icpc', 'cva_tia', 'le_dvt', 'copd_astma', 'aant_hvz', 'influenza_vacc', 'model1_outcome_hosp', 'model1_outcome_mort')
nonnormal <- c('leeftijd', 'aant_cont_voor_episode', 'frailty_index')
vars <- c(factorvars, nonnormal)
table1 <- CreateTableOne(vars = vars, data = E, factorVars = factorvars, strata = 'model1_outcome', addOverall = T)
print(table1, nonnormal = nonnormal)
rm(factorvars, nonnormal, vars)

# Step 5: Perform formal sample size calculation:
#- Based on the following assumptions: prevalence = 7.8% in dataset, aiming for c statistic of 0.70 with 10% shrinkage
pmsampsize(type = 'b', parameters = 15, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9)
pmsampsize(type = 'b', parameters = 20, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9)
pmsampsize(type = 'b', parameters = 30, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9)
pmsampsize(type = 'b', parameters = 35, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9)
pmsampsize(type = 'b', parameters = 40, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9)
pmsampsize(type = 'b', parameters = 47, prevalence = 0.078, cstatistic = 0.70, shrinkage = 0.9) # (is maximum)

# Step 6: histogram of timing of outcomes
E$indexdate <- ymd(E$indexdate)
E$model1_outcome_date <- ymd(E$model1_outcome_date)
E <- E %>%
  mutate(
    time_to_outcome = model1_outcome_date - indexdate
  )
summary(E$time_to_outcome)
ggplot(E) +
  geom_histogram(mapping = aes(x = time_to_outcome), colour = 'coral4', fill = 'coral3', na.rm = T, binwidth = 1) +
  xlim(c(-0.5, 30.5)) +
  ylim(c(0, 270)) +
  xlab('Time to outcome (days)') +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  ylab('Number of patients') +
  theme_economist() +
  theme(axis.title.y = element_text(margin = margin(r = 20)), axis.title.x = element_text(margin = margin(t = 20)))
ggsave('Export R/Time_to_outcome.tiff')
quantile(E$time_to_outcome, c(0.25, 0.5, 0.75), na.rm = T)

# Step 7: Fit models using elastic net for regularization
#- x1: basic 1, with age as linear
#- x2: basic 2, with age as RCS
#- x3: CVD 1, with age as linear and CVD as cumulative
#- x4: CVD 2, with age as linear and CVD as separate
#- x5: clinical, with age as linear and CVD as cumulative
#- x6: EHR 1, with age as linear, CVD as cumulative, and frailty index and number of consultations as linear
#- x7: EHR 2, with age as linear, CVD as cumulative, and frailty index and number of consultations as RCS

# Assess relevant quantiles of age in the development cohort
quantile(E$leeftijd, c(0.05, 0.35, 0.65, 0.95))
quantile(E$frailty_index, c(0.05, 0.35, 0.65, 0.95), na.rm = T)
quantile(E$aant_cont_voor_episode, c(0.05, 0.35, 0.65, 0.95), na.rm = T)

# Create model matrices #
x1 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht), data = E)[,-1]
x2 <- model.matrix(model1_outcome ~ rcs(leeftijd, c(43, 58, 70, 86))*factor(geslacht), data = E)[,-1]
x3 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz), data = E)[,-1]
x4 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(diabetes) + factor(dec_cordis) + factor(coronaire_hartziekte) + factor(cva_tia) + factor(le_dvt) + factor(atriumfib) + factor(claud_intermit), data = E)[,-1]
x5 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = E)[,-1]
x6 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + frailty_index + aant_cont_voor_episode, data = E)[,-1]
x7 <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + rcs(frailty_index, c(0.0, 0.001, 0.28, 0.48)) + rcs(aant_cont_voor_episode, c(0.0, 5, 12, 33)), data = E)[,-1]

x <- list(x1, x2, x3, x4, x5, x6, x7)
rm(x1, x2, x3, x4, x5, x6, x7)

# Create list to store model fits
fit <- list()
fit$basic1 <- list()
fit$basic2 <- list()
fit$cvd1 <- list()
fit$cvd2 <- list()
fit$clinical <- list()
fit$ehr1 <- list()
fit$ehr2 <- list()

# Fit models using elastic net with alpha = 0.5
for (i in 1:7) {
  fit[[i]] <- cv.glmnet(x = x[[i]], # Cross-validate to find optimal lambda (hyperparameter tuning)
                        y = E$model1_outcome,
                        data = E,
                        family = 'binomial',
                        alpha = 0.5,
                        type.measure = 'deviance',
                        nfolds = 10,
                        trace.it = 1L)
  plot(fit[[i]], main = i)
}

## Coefficients of the models ##
coef_basic1 <- as.matrix(coef(fit$basic1, s = fit$basic1$lambda.min))
write.csv(coef_basic1, 'Export R/coefficients_basic1_model.csv')
coef_basic2 <- as.matrix(coef(fit$basic2, s = fit$basic2$lambda.min))
write.csv(coef_basic2, 'Export R/coefficients_basic2_model.csv')
coef_cvd1 <- as.matrix(coef(fit$cvd1, s = fit$cvd1$lambda.min))
write.csv(coef_cvd1, 'Export R/coefficients_cvd1_model.csv')
coef_cvd2 <- as.matrix(coef(fit$cvd2, s = fit$cvd2$lambda.min))
write.csv(coef_cvd2, 'Export R/coefficients_cvd2_model.csv')
coef_clinical <- as.matrix(coef(fit$clinical, s = fit$clinical$lambda.min))
write.csv(coef_clinical, 'Export R/coefficients_clinical_model.csv')
coef_ehr1 <- as.matrix(coef(fit$ehr1, s = fit$ehr1$lambda.min))
write.csv(coef_ehr1, 'Export R/coefficients_ehr1_model.csv')
coef_ehr2 <- as.matrix(coef(fit$ehr2, s = fit$ehr2$lambda.min))
write.csv(coef_ehr2, 'Export R/coefficients_ehr2_model.csv')
rm(coef_basic1, coef_basic2, coef_cvd1, coef_cvd2, coef_clinical, coef_ehr1, coef_ehr2)

# Step 8: Make predictons based on fitted models and calculate apparent performance
# Create table to store predictions
table_pred <- data.frame(matrix(nrow = nrow(E), ncol = 8))
colnames(table_pred) <- c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2', 'rin')
table_pred$model1_outcome <- E$model1_outcome
table_pred$rin <- E$rin

# Store predictions based on development data
for (i in 1:7) {
  table_pred[,i] <- as.numeric(predict(fit[[i]], newx = x[[i]], type = 'response', s = 'lambda.min'))
}

# Calculate apparent AUC and R-squared
apparent_performance <- data.frame(matrix(nrow = 10, ncol = 7))
colnames(apparent_performance) <- c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2')
rownames(apparent_performance) <- c('AUC', 'AUC_l', 'AUC_u', 'R-squared', 'intercept', 'intercept_l', 'intercept_u', 'slope', 'slope_l', 'slope_u')

for (i in c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2')) {
  perf <- val.prob.ci.2(p = table_pred[[i]], y = table_pred$model1_outcome)
  apparent_performance[1:3, i] <- perf$Cindex
  apparent_performance[4, i] <- perf$stats[[3]]
  apparent_performance[5:7, i] <- perf$Calibration$Intercept
  apparent_performance[8:10, i] <- perf$Calibration$Slope
}
rm(perf, i)

# Export
write.table(apparent_performance, 'Export R/apparent_performance.csv')
val.prob.ci.2(table_pred$basic1, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$basic2, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$cvd1, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$cvd2, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$clinical, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$ehr1, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')
val.prob.ci.2(table_pred$ehr2, table_pred$model1_outcome, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.65, dist.label = 0.02, dist.label2 = 0.02, legendloc = F, dostats = T, statloc = c(0.47, 0.075), lwd.smooth = 2, xlab = 'Estimated probability')

## Create table with range of predicted risks
range_risks <- data.frame(matrix(ncol = 7, nrow = 3))
colnames(range_risks) <- c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2')
rownames(range_risks) <- c('min', 'median', 'max')
for (i in c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2')) {
  range_risks[1,i] <- min(table_pred[,i])
  range_risks[2,i] <- median(table_pred[,i])
  range_risks[3,i] <- max(table_pred[,i])
}

# Save table with range of predicted risks
write.table(range_risks, 'Export R/range_risks_apparent.csv')

## Create histogram of predicted risks of clinical model
ggplot(table_pred) +
  geom_histogram(mapping = aes(x = clinical), colour = 'coral4', fill = 'coral3', binwidth = 0.01, center = 0) +
  xlab('Predicted risk') +
  ylab('LRTI patients (n)') +
  theme_economist()
ggsave('Export R/Predicted_risks_clinical.tiff')

## cohort of patients with estimated probability >0.2
E20 <- filter(table_pred, clinical >= 0.2)
E20 <- subset(E20, select = c(rin, clinical))
E20 <- left_join(E20, E, by = 'rin')
factorvars <- c('geslacht', 'jaar', 'diabetes', 'smokstatus', 'malign_excl_huid', 'pneumonie', 'dementie', 'opname_1j_voor_episode', 'immunosupp', 'inhalatiemed', 'med_antidepr', 'ab_alg_voor_epi', 'start_icpc', 'cva_tia', 'le_dvt', 'copd_astma', 'aant_hvz', 'influenza_vacc', 'model1_outcome_hosp', 'model1_outcome_mort')
nonnormal <- c('leeftijd', 'aant_cont_voor_episode', 'frailty_index')
vars <- c(factorvars, nonnormal)
tableE20 <- CreateTableOne(vars = vars, data = E20, factorVars = factorvars, addOverall = T)
print(tableE20, nonnormal = nonnormal)
rm(factorvars, nonnormal, vars)

# Step 9.1 Create functions for internal validation (bootstrapping)
# Create list of functions
internal_val <- list()
internal_val$basic1 <- NA
internal_val$basic2 <- NA
internal_val$cvd1 <- NA
internal_val$cvd2 <- NA
internal_val$clinical <- NA
internal_val$ehr1 <- NA
internal_val$ehr2 <- NA

# Write function: basic1 model
internal_val$basic1 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: basic2 model
internal_val$basic2 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ rcs(leeftijd, c(42, 57, 70, 85))*factor(geslacht), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ rcs(leeftijd, c(42, 57, 70, 85))*factor(geslacht), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: cvd1 model
internal_val$cvd1 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: cvd2 model
internal_val$cvd2 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(diabetes) + factor(dec_cordis) + factor(coronaire_hartziekte) + factor(cva_tia) + factor(le_dvt) + factor(atriumfib) + factor(claud_intermit), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(diabetes) + factor(dec_cordis) + factor(coronaire_hartziekte) + factor(cva_tia) + factor(le_dvt) + factor(atriumfib) + factor(claud_intermit), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: clinical model
internal_val$clinical <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: ehr1 model
internal_val$ehr1 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + frailty_index + aant_cont_voor_episode, data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + frailty_index + aant_cont_voor_episode, data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Write function: ehr2 model
internal_val$ehr2 <- function(data, indices) {
  # Create model matrices, x = bootstrap sample, y = original development cohort #
  d <- data[indices,]
  x <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + rcs(frailty_index, c(0, 0.26, 0.46)) + rcs(aant_cont_voor_episode, c(0, 4, 11, 30)), data = d)[,-1]
  y <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc) + factor(smokstatus) + rcs(frailty_index, c(0, 0.26, 0.46)) + rcs(aant_cont_voor_episode, c(0, 4, 11, 30)), data = data)[,-1]
  
  # Fit bootstrap model using elastic net with alpha = 0.5
  fit <- cv.glmnet(x = x, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = d$model1_outcome,
                   data = d,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10)
  
  # Create tables to store predictions (b = bootstrap, o = original)
  table_pred_b <- data.frame(matrix(nrow = nrow(d), ncol = 1))
  table_pred_b$model1_outcome <- d$model1_outcome
  table_pred_o <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  table_pred_o$model1_outcome <- data$model1_outcome
  
  # Store predictions based on bootstrapped data (apparent bootstrap performance)
  table_pred_b[,1] <- as.numeric(predict(fit, newx = x, type = 'response', s = 'lambda.min'))
  
  # Store predictions based on original data
  table_pred_o[,1] <- as.numeric(predict(fit, newx = y, type = 'response', s = 'lambda.min'))
  
  #Calculate performance measures in bootstrapped data (apparent bootstrap performance)
  perf_b <- val.prob.ci.2(p = table_pred_b[,1], y = table_pred_b$model1_outcome)
  auc_b <- perf_b$Cindex[[1]]
  rsq_b <- perf_b$stats[[3]]
  intercept_b <- perf_b$Calibration$Intercept[[1]]
  slope_b <- perf_b$Calibration$Slope[[1]]
  #Calculate performance measures in original data
  perf_o <- val.prob.ci.2(p = table_pred_o[,1], y = table_pred_o$model1_outcome)
  auc_o <- perf_o$Cindex[[1]]
  rsq_o <- perf_o$stats[[3]]
  intercept_o <- perf_o$Calibration$Intercept[[1]]
  slope_o <- perf_o$Calibration$Slope[[1]]
  # Calculate apparent-minus-test optimism per performance estimate
  auc <- auc_o - auc_b
  rsq <- rsq_o - rsq_b
  intercept <- intercept_o - intercept_b
  slope <- slope_o - slope_b
  return(c(auc, rsq, intercept, slope))
}

# Step 9.2: Internal validation for optimism corrected performance (bootstrap 1000 samples)
# Set number of bootstrap samples
n_bootstrap = 1000

# Bootstrapping
set.seed(12321)
boot_basic1 <- boot(data = E, statistic = internal_val$basic1, R = n_bootstrap)
boot_basic2 <- boot(data = E, statistic = internal_val$basic2, R = n_bootstrap)
boot_cvd1 <- boot(data = E, statistic = internal_val$cvd1, R = n_bootstrap)
boot_cvd2 <- boot(data = E, statistic = internal_val$cvd2, R = n_bootstrap)
boot_clinical <- boot(data = E, statistic = internal_val$clinical, R = n_bootstrap)
boot_ehr1 <- boot(data = E, statistic = internal_val$ehr1, R = n_bootstrap)
boot_ehr2 <- boot(data = E, statistic = internal_val$ehr2, R = n_bootstrap)

# Create dataframe to store results and subtract mean optimism from apparent model performance to obtain optimism-corrected performance
internal_validation <- data.frame(matrix(nrow = 7, ncol = 4))
colnames(internal_validation) <- c('auc', 'rsq', 'intercept', 'slope')
rownames(internal_validation) <- c('basic1', 'basic2', 'cvd1', 'cvd2', 'clinical', 'ehr1', 'ehr2')
internal_validation[1,1] <- apparent_performance[1,1] - mean(boot_basic1$t[,1])
internal_validation[1,2] <- apparent_performance[4,1] - mean(boot_basic1$t[,2])
internal_validation[1,3] <- apparent_performance[5,1] - mean(boot_basic1$t[,3])
internal_validation[1,4] <- apparent_performance[8,1] - mean(boot_basic1$t[,4])
internal_validation[2,1] <- apparent_performance[1,2] - mean(boot_basic2$t[,1])
internal_validation[2,2] <- apparent_performance[4,2] - mean(boot_basic2$t[,2])
internal_validation[2,3] <- apparent_performance[5,2] - mean(boot_basic2$t[,3])
internal_validation[2,4] <- apparent_performance[8,2] - mean(boot_basic2$t[,4])
internal_validation[3,1] <- apparent_performance[1,3] - mean(boot_cvd1$t[,1])
internal_validation[3,2] <- apparent_performance[4,3] - mean(boot_cvd1$t[,2])
internal_validation[3,3] <- apparent_performance[5,3] - mean(boot_cvd1$t[,3])
internal_validation[3,4] <- apparent_performance[8,3] - mean(boot_cvd1$t[,4])
internal_validation[4,1] <- apparent_performance[1,4] - mean(boot_cvd2$t[,1])
internal_validation[4,2] <- apparent_performance[4,4] - mean(boot_cvd2$t[,2])
internal_validation[4,3] <- apparent_performance[5,4] - mean(boot_cvd2$t[,3])
internal_validation[4,4] <- apparent_performance[8,4] - mean(boot_cvd2$t[,4])
internal_validation[5,1] <- apparent_performance[1,5] - mean(boot_clinical$t[,1])
internal_validation[5,2] <- apparent_performance[4,5] - mean(boot_clinical$t[,2])
internal_validation[5,3] <- apparent_performance[5,5] - mean(boot_clinical$t[,3])
internal_validation[5,4] <- apparent_performance[8,5] - mean(boot_clinical$t[,4])
internal_validation[6,1] <- apparent_performance[1,6] - mean(boot_ehr1$t[,1])
internal_validation[6,2] <- apparent_performance[4,6] - mean(boot_ehr1$t[,2])
internal_validation[6,3] <- apparent_performance[5,6] - mean(boot_ehr1$t[,3])
internal_validation[6,4] <- apparent_performance[8,6] - mean(boot_ehr1$t[,4])
internal_validation[7,1] <- apparent_performance[1,7] - mean(boot_ehr2$t[,1])
internal_validation[7,2] <- apparent_performance[4,7] - mean(boot_ehr2$t[,2])
internal_validation[7,3] <- apparent_performance[5,7] - mean(boot_ehr2$t[,3])
internal_validation[7,4] <- apparent_performance[8,7] - mean(boot_ehr2$t[,4])
write.csv(internal_validation, 'Export R/internal_validation.csv')

# Step 10: sensitivity analysis. Outcome: respiratory tract or cardiovasculatory related hospitalisation or mortality within 30 days
# Create table to store predictions of original model and outcome for sensitivity analysis (i.e. cardiorespiratory hospitalisation)
table_pred_sens <- data.frame(matrix(nrow = nrow(E), ncol = 1))
colnames(table_pred_sens) <- 'prediction'
table_pred_sens$outcome_sens <- E$outcome_sens

# Store predictions by original model
table_pred_sens[,1] <- as.numeric(predict(fit[[5]], newx = x[[5]], type = 'response', s = 'lambda.min'))

# Calculate apparent AUC and R-squared
apparent_performance_sens <- data.frame(matrix(nrow = 10, ncol = 1))
colnames(apparent_performance_sens) <- c('sensitivity_1')
rownames(apparent_performance_sens) <- c('AUC', 'AUC_l', 'AUC_u', 'R-squared', 'intercept', 'intercept_l', 'intercept_u', 'slope', 'slope_l', 'slope_u')

perf <- val.prob.ci.2(p = table_pred_sens$prediction, y = table_pred_sens$outcome_sens)
apparent_performance_sens[1:3, 1] <- perf$Cindex
apparent_performance_sens[4, 1] <- perf$stats[[3]]
apparent_performance_sens[5:7, 1] <- perf$Calibration$Intercept
apparent_performance_sens[8:10, 1] <- perf$Calibration$Slope

# Export
write.table(apparent_performance_sens, 'Export R/apparent_performance_sens.csv')
val.prob.ci.2(table_pred_sens$prediction, table_pred_sens$outcome_sens, xlim = c(0, 0.7), ylim = c(0, 0.7), line.bins = 0.67, dist.label2 = 0.02, legendloc = F, dostats = T)

# Step 11: assess incremental predictive value of CVD
# delta AUC #
roc_basic1 <- roc(table_pred$model1_outcome, table_pred$basic1)
roc_basic2 <- roc(table_pred$model1_outcome, table_pred$basic2)
roc_cvd1 <- roc(table_pred$model1_outcome, table_pred$cvd1)
roc_cvd2 <- roc(table_pred$model1_outcome, table_pred$cvd2)
roc_clinical <- roc(table_pred$model1_outcome, table_pred$clinical)
roc_ehr1 <- roc(table_pred$model1_outcome, table_pred$ehr1)
roc_ehr2 <- roc(table_pred$model1_outcome, table_pred$ehr2)
roc.test(roc_basic1, roc_cvd1, alternative = 'two.sided', method = 'bootstrap')

# plot ROC curves of all models #
tiff('Export R/AUROC_incremental_all.tiff', width = 800, height = 600)
par(pty='s')
plot.roc(roc_basic1, main = 'ROC curve - Apparent Performance', col = 'steelblue', legacy.axes = T)
plot.roc(roc_basic2, main = 'ROC curve - Apparent Performance', col = 'steelblue1', legacy.axes = T)
plot.roc(roc_cvd1, col = 'lightsalmon', add = T)
plot.roc(roc_cvd2, col = 'tomato3', add = T)
plot.roc(roc_clinical, col = 'olivedrab', add = T)
plot.roc(roc_ehr1, col = 'palevioletred', add = T)
plot.roc(roc_ehr2, col = 'palevioletred4', add = T)
legend('bottomright', c('basic 1 model (auc 0.61)', 'basic 2 model (auc 0.62)', 'CVD 1 model (auc 0.63)', 'CVD 2 model (auc 0.63)', 'clinical model (auc 0.73)', 'EHR 1 model (auc 0.74)', 'EHR 2 model (auc 0.74)'), col = c('steelblue', 'steelblue1', 'lightsalmon', 'tomato3', 'olivedrab', 'palevioletred', 'palevioletred4'), lty = 1, cex = 1.5)
dev.off()

# plot ROC curves of basis/cmd/clinical model #
tiff('Export R/AUROC_incremental_cvd.tiff', width = 800, height = 600)
par(pty='s')
plot.roc(roc_basic1, main = 'ROC curve - Apparent Performance', col = 'orange', legacy.axes = T)
plot.roc(roc_cvd1, col = 'cadetblue', add = T)
plot.roc(roc_clinical, col = 'violetred', add = T)
legend('bottomright', c('basic1 model (auc 0.61)', 'cvd1 model (auc 0.63)', 'clinical model (auc 0.73)'), col = c('orange', 'cadetblue', 'violetred'), lty = 1, cex = 1.5)
dev.off()

## Create matrix with predictions of CVD model ##
pred_cvd <- expand.grid(leeftijd = c(40:100), geslacht = c('M', 'V'), aant_hvz = c(0:2))
matrix_cvd <- model.matrix( ~ leeftijd*factor(geslacht) + factor(aant_hvz), data = pred_cvd)[,-1]
pred_cvd$pred <- as.numeric(predict(fit$cvd1, newx = matrix_cvd, type = 'response', s = 'lambda.min'))
pred_cvd$aant_hvz <- as.character(pred_cvd$aant_hvz)
rm(matrix_cvd)

## Plot predicted probabilities ##
label_geslacht <- c('M' = 'Male',
                    'V' = 'Female')
ggplot(data = pred_cvd) +
  geom_smooth(mapping = aes(x = leeftijd, y = pred, color = factor(aant_hvz)), method = 'loess') +
  scale_color_manual(values = c('cadetblue', 'coral', 'firebrick3'), labels = c('0', '1', '\u2265 2', '1', '0'), name = 'Number of CVD') +
  facet_wrap(~ geslacht, labeller = as_labeller(label_geslacht)) +
  xlab('Age (years)') +
  ylab('Predicted probability')
ggsave('Export R/prediction_plot.tiff')

# Step 12: Internal-external cross-validation based on year (2016 - 2019)
## Create table for results
int_ext_year <- as.data.frame(matrix(nrow = 4, ncol = 13))
rownames(int_ext_year) <- c('2016', '2017', '2018', '2019')
colnames(int_ext_year) <- c('Validation year', 'n', 'outcomes', 'AUC', 'AUC_l', 'AUC_u', 'rsq', 'slope', 'slope_l', 'slope_u', 'intercept', 'intercept_l', 'intercept_u')
int_ext_year$n[1] <- as.numeric(sum(E$jaar == 2016))
int_ext_year$n[2] <- as.numeric(sum(E$jaar == 2017))
int_ext_year$n[3] <- as.numeric(sum(E$jaar == 2018))
int_ext_year$n[4] <- as.numeric(sum(E$jaar == 2019))
int_ext_year$outcomes[1] <- as.numeric(sum(E$model1_outcome == 1 & E$jaar == 2016))
int_ext_year$outcomes[2] <- as.numeric(sum(E$model1_outcome == 1 & E$jaar == 2017))
int_ext_year$outcomes[3] <- as.numeric(sum(E$model1_outcome == 1 & E$jaar == 2018))
int_ext_year$outcomes[4] <- as.numeric(sum(E$model1_outcome == 1 & E$jaar == 2019))

## Loop for internal-external cross-validation
set.seed(1234)
for (i in c(2016, 2017, 2018, 2019)) {
  
  train_data <- subset(E, E$jaar != i)
  test_data <- subset(E, E$jaar == i)
  
  x_train <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = train_data)[,-1]
  
  x_test <- model.matrix(model1_outcome ~ leeftijd*factor(geslacht) + factor(aant_hvz) + opname_1j_voor_episode + pneumonie + malign_excl_huid + copd_astma + dementie + influenza_vacc + immunosupp + inhalatiemed + ab_alg_voor_epi + med_antidepr + factor(start_icpc), data = test_data)[,-1]
  
  # Fit model on training data  
  fit <- cv.glmnet(x = x_train, # Cross-validate to find optimal lambda (hyperparameter tuning)
                   y = train_data$model1_outcome,
                   data = train_data,
                   family = 'binomial',
                   alpha = 0.5,
                   type.measure = 'deviance',
                   nfolds = 10,
                   trace.it = 1L)
  
  # Create table to store predictions
  table_pred <- data.frame(matrix(nrow = nrow(test_data), ncol = 1))
  colnames(table_pred) <- 'Prediction'
  table_pred$model1_outcome <- test_data$model1_outcome
  
  # Store predictions based on development data
  table_pred[,1] <- as.numeric(predict(fit, newx = x_test, type = 'response', s = 'lambda.min'))
  
  # Calculate apparent AUC and R-squared
  perf <- val.prob.ci.2(p = table_pred[,1], y = table_pred$model1_outcome)
  int_ext_year[as.character(i),4:6] <- perf$Cindex
  int_ext_year[as.character(i),7] <- perf$stats[[3]]
  int_ext_year[as.character(i),8:10] <- perf$Calibration$Slope
  int_ext_year[as.character(i),11:13] <- perf$Calibration$Intercept
  int_ext_year[as.character(i),1] <- i
}
rm(test_data, train_data, x_test, x_train, i)
write.csv(int_ext_year, 'Export R/internal-external_year.csv')

# Calculate SE from CI of AUC, slope, and intercept
for (i in 1:4) {
  int_ext_year[i,14] <- (int_ext_year[i,6]-int_ext_year[i,5])/3.92
}
colnames(int_ext_year)[14] <- 'SE_auc'
for (i in 1:4) {
  int_ext_year[i,15] <- (int_ext_year[i,10]-int_ext_year[i,9])/3.92
}
colnames(int_ext_year)[15] <- 'SE_slope'
for (i in 1:4) {
  int_ext_year[i,16] <- (int_ext_year[i,13]-int_ext_year[i,12])/3.92
}
colnames(int_ext_year)[16] <- 'SE_intercept'

# Fit a random-effects model and plot a forest plot
REML_year <- list()
## AUC ##
REML_year$auc <- rma(yi = int_ext_year$AUC, sei = int_ext_year$SE_auc, slab = int_ext_year$`Validation year`)
REML_year$auc
forest(REML_year$auc, xlim = c(-0.3, 1.5), xlab = 'c-statistic', refline = REML_year$auc$b, at = c(0.5, 0.75, 1), ilab = cbind(int_ext_year$n, int_ext_year$outcomes), ilab.xpos = c(0.2, 0.4), header = c('Validation year', 'c-statistic (95% CI)'), showweights = T, psize = 1, col = 'coral3')
text(c(0.2, 0.4), REML_year$auc$k+2, c('n', 'Events'), cex = 1, font = 2)
## Slope ##
REML_year$slope <- rma(yi = int_ext_year$slope, sei = int_ext_year$SE_slope, slab = int_ext_year$`Validation year`)
REML_year$slope
forest(REML_year$slope, xlim = c(-2, 3), xlab = 'Calibration slope', refline = REML_year$slope$b, at = c(0.5, 1, 1.5), ilab = cbind(int_ext_year$n, int_ext_year$outcomes), ilab.xpos = c(-0.7, -0.2), header = c('Validation year', 'Slope (95% CI)'), showweights = T, psize = 1, col = 'coral3')
text(c(-0.7, -0.2), REML_year$slope$k+2, c('n', 'Events'), cex = 1, font = 2)
## Intercept ##
REML_year$intercept <- rma(yi = int_ext_year$intercept, sei = int_ext_year$SE_intercept, slab = int_ext_year$`Validation year`)
REML_year$intercept
forest(REML_year$intercept, xlim = c(-2.25, 1.75), xlab = 'Calibration intercept', refline = REML_year$intercept$b, at = c(-0.5, -0.25, 0, 0.25, 0.5), ilab = cbind(int_ext_year$n, int_ext_year$outcomes), ilab.xpos = c(-1.25, -0.8), header = c('Validation year', 'Intercept (95% CI)'), showweights = T, psize = 1, col = 'coral3')
text(c(-1.25, -0.8), REML_year$intercept$k+2, c('n', 'Events'), cex = 1, font = 2)