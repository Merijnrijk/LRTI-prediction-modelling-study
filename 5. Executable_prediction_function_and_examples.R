# ! PLEASE NOTE that this risk calculating prediction function is not yet Medical Device Regulation (MDR-)approved, 
# and is thus not intended for medical use.

# This script can be used to calculate risk of 30-day all-cause hospitalisation or mortality in patients ≥40 years presenting to general practice with LRTI
# It constructs the final model at full-precision and calculates risk in the three worked cases included in the SUpplementary materials
# For predictor levels: see Tablse S8 (Supplementary materials)
# Lambda value used for the final model: lambda.min

# Set working directory
wd <- # Insert working directory where coefficients_final_model.csv is stored
setwd(wd)

# Load full-precision coefficients
coefs <- read.csv(
  "Coefficients_final_model.csv",
  sep = ';',
  stringsAsFactors = FALSE
)
colnames(coefs) <- c('term', 'coefficient')
coefs$coefficient <- as.numeric(gsub(",", ".", coefs$coefficient))
# Extract intercept and beta coefficients
intercept <- as.numeric(
  coefs$coefficient[coefs$term == "(Intercept)"]
)
beta <- as.numeric(
  coefs$coefficient[coefs$term != "(Intercept)"]
)
names(beta) <- coefs$term[coefs$term != "(Intercept)"]
stopifnot(
  length(intercept) == 1L,
  !anyNA(beta),
  !anyDuplicated(names(beta))
)

# Define full-precision prediction function
predict_risk <- function(X) {
  X <- as.matrix(X)
  
  stopifnot(
    !is.null(colnames(X)),
    setequal(colnames(X), names(beta))
  )
  
  X <- X[, names(beta), drop = FALSE]
  
  lp <- as.numeric(intercept + X %*% beta)
  plogis(lp)
}

# Construct dataframe and model matrix for test cases
test_cases <- data.frame(
  leeftijd = c(76, 50, 88),
  geslacht = c('V', 'M', 'V'),
  aant_hvz = c(1, 0, 2),
  start_icpc = c('R81', 'R78', 'R81'),
  opname_1j_voor_episode = c(0, 0, 1),
  pneumonie = c(1, 0, 1),
  malign_excl_huid = c(0, 0, 0),
  copd_astma = c(1, 0, 0),
  dementie = c(0, 0, 1),
  influenza_vacc = c(1, 0, 1),
  immunosupp = c(1, 0, 0),
  inhalatiemed = c(1, 0, 0),
  ab_alg_voor_epi = c(0, 0, 1),
  med_antidepr = c(0, 0, 0)
)
X_new <- model.matrix(
  ~ leeftijd * factor(geslacht) +
    factor(aant_hvz) +
    opname_1j_voor_episode +
    pneumonie +
    malign_excl_huid +
    copd_astma +
    dementie +
    influenza_vacc +
    immunosupp +
    inhalatiemed +
    ab_alg_voor_epi +
    med_antidepr +
    factor(start_icpc),
  data = test_cases
)[, -1, drop = FALSE]

# Predict risk in test cases
predicted_risk <- predict_risk(X_new)
results <- data.frame(
  predicted_risk = predicted_risk
)
print(results)
#     0.10803411
#     0.02065934
#     0.35171053

# To calculate risk in a new patient:
# Define factor levels
factor_levels <- list(
  geslacht   = c('M', 'V'),
  aant_hvz   = c('0', '1', '2'),
  start_icpc = c('R78', 'R81')
)

# Construct dataframe for new case (adjust values based on patient characteristics)
new_case <- data.frame(
  leeftijd = c(40), # Age, range ≥40
  geslacht = c('V'), # Sex, V for female, M for male
  aant_hvz = c(0), # Number of cardiometabolic diseases (0 = none, 1 = one, 2 = two or more)
  start_icpc = c('R81'), # LRTI diagnosis (R81 for pneumonia, R78 for acute bronchitis)
  opname_1j_voor_episode = c(0), # Hospitalisation within the previous year (0 = no, 1 = yes)
  pneumonie = c(0), # History of pneumonia (0 = no, 1 = yes)
  malign_excl_huid = c(0), # History of malignancies (excluding skin) (0 = no, 1 = yes)
  copd_astma = c(0), # History of COPD or asthma (0 = no, 1 = yes)
  dementie = c(0), # History of dementia (0 = no, 1 = yes)
  influenza_vacc = c(0), # Influenza vaccination in the previous year (0 = no, 1 = yes)
  immunosupp = c(0), # Use of immunosuppressant drugs (0 = no, 1 = yes)
  inhalatiemed = c(0), # Use of inhalation medication (0 = no, 1 = yes)
  ab_alg_voor_epi = c(0), # Antibiotic prescription in previous month (0 = no, 1 = yes)
  med_antidepr = c(0) # Use of antidepressants (0 = no, 1 = yes)
)

# Explicitly assign the original training factor levels
new_case$geslacht <- factor(
  new_case$geslacht,
  levels = factor_levels$geslacht
)

new_case$aant_hvz <- factor(
  as.character(new_case$aant_hvz),
  levels = factor_levels$aant_hvz
)

new_case$start_icpc <- factor(
  new_case$start_icpc,
  levels = factor_levels$start_icpc
)

# Stop if any categorical value is not recognised
stopifnot(
  !anyNA(new_case$geslacht),
  !anyNA(new_case$aant_hvz),
  !anyNA(new_case$start_icpc)
)

# Construct model matrix
X_new_case <- model.matrix(
  ~ leeftijd * factor(geslacht) +
    factor(aant_hvz) +
    opname_1j_voor_episode +
    pneumonie +
    malign_excl_huid +
    copd_astma +
    dementie +
    influenza_vacc +
    immunosupp +
    inhalatiemed +
    ab_alg_voor_epi +
    med_antidepr +
    factor(start_icpc),
  data = new_case
)[, -1, drop = FALSE]

# Check for missing or non-finite predictor values
stopifnot(
  nrow(X_new_case) == 1L,
  !anyNA(X_new_case),
  all(is.finite(X_new_case))
)

# Predict risk in new case
predicted_risk <- predict_risk(X_new_case)
results <- data.frame(
  predicted_risk = predicted_risk
)
print(results)
