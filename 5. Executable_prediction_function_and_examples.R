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

# Store the original model formula
form <- ~ leeftijd * factor(geslacht) +
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
  factor(start_icpc)

# Define factor levels
factor_levels <- list(
  geslacht   = c('M', 'V'),
  aant_hvz   = c('0', '1', '2'),
  start_icpc = c('R78', 'R81')
)

# Specify the original training levels for the factor terms
xlev <- list(
  `factor(geslacht)` = factor_levels$geslacht,
  `factor(aant_hvz)` = factor_levels$aant_hvz,
  `factor(start_icpc)` = factor_levels$start_icpc
)

# Function to construct a model matrix
make_model_matrix <- function(data) {
  mf <- model.frame(
    form,
    data = data,
    xlev = xlev
  )
  
  X <- model.matrix(form, data = mf)
  
  # Remove intercept; it is supplied separately by predict_risk()
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  
  # Confirm that all expected predictors are present
  stopifnot(identical(colnames(X), names(beta)))
  
  X
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

# Predict risk in test cases
X_new <- make_model_matrix(test_cases)
predicted_risk <- predict_risk(X_new)

# Automated test: predicted risks within tolerance of risk predicted by fitted model object (see model validation script)
expected_risk <- c(0.1084, 0.0206, 0.3521)
tolerance <- 0.001
stopifnot(
  length(predicted_risk) == length(expected_risk),
  all(abs(predicted_risk - expected_risk) <= tolerance)
)
message("All predicted risks are within the specified tolerance.")

# Print results
results <- data.frame(
  predicted_risk = predicted_risk
)
print(results)
#     0.10803411
#     0.02065934
#     0.35171052

## To calculate risk in a new patient:
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

# Automated tests for categorical predictors and age-by-sex interaction
# First, generate test cases covering all categorical levels at two ages (50 and 70)
test_grid <- expand.grid(
  leeftijd = c(50, 70),
  geslacht = c("M", "V"),
  aant_hvz = c("0", "1", "2"),
  start_icpc = c("R78", "R81"),
  stringsAsFactors = FALSE
)

# Set all remaining predictors to valid reference/test values
test_grid$opname_1j_voor_episode <- 0
test_grid$pneumonie <- 0
test_grid$malign_excl_huid <- 0
test_grid$copd_astma <- 0
test_grid$dementie <- 0
test_grid$influenza_vacc <- 0
test_grid$immunosupp <- 0
test_grid$inhalatiemed <- 0
test_grid$ab_alg_voor_epi <- 0
test_grid$med_antidepr <- 0

# Construct model matrix
X_test <- make_model_matrix(test_grid)

# Test 1: all categorical combinations are represented
stopifnot(
  nrow(test_grid) == 24L,
  setequal(test_grid$geslacht, factor_levels$geslacht),
  setequal(test_grid$aant_hvz, factor_levels$aant_hvz),
  setequal(test_grid$start_icpc, factor_levels$start_icpc)
)

# Test 2: exact predictor names and column order
stopifnot(
  identical(colnames(X_test), names(beta))
)

# Test 3: reference and non-reference coding for sex
stopifnot(
  all(X_test[, "factor(geslacht)V"] ==
        as.numeric(test_grid$geslacht == "V"))
)

# Test 4: reference and non-reference coding for number of cardiometabolic diseases
stopifnot(
  all(X_test[, "factor(aant_hvz)1"] ==
        as.numeric(test_grid$aant_hvz == "1")),
  all(X_test[, "factor(aant_hvz)2"] ==
        as.numeric(test_grid$aant_hvz == "2"))
)

# Test 5: reference and non-reference coding for LRTI diagnosis code
stopifnot(
  all(X_test[, "factor(start_icpc)R81"] ==
        as.numeric(test_grid$start_icpc == "R81"))
)

# Test 6: age-by-sex interaction
stopifnot(
  all(
    X_test[, "leeftijd:factor(geslacht)V"] ==
      ifelse(test_grid$geslacht == "V",
             test_grid$leeftijd, 0)
  )
)

# Test 7: all predicted risks are finite and between 0 and 1
risk_test <- predict_risk(X_test)

stopifnot(
  length(risk_test) == nrow(test_grid),
  all(is.finite(risk_test)),
  all(risk_test >= 0 & risk_test <= 1)
)
message("All categorical-level and interaction tests passed.")

# Create model matrix for new patient
X_new_case <- make_model_matrix(new_case)

# Predict risk for the new patient
predicted_risk <- predict_risk(X_new_case)
print(predicted_risk)
