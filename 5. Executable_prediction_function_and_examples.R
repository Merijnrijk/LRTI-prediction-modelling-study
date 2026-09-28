# ! Please note that this risk calculating prediction function is not yet Medical Device Regulation (MDR-)approved, 
# and is thus not intended for medical use.

# Minimal executable prediction function of final model
predict_risk <- function(age, female_sex, cmd_1, cmd_2, pneumonia_diagnosis, hospitalisation, pneumonia_history, 
                         malignancy, copd_asthma, dementia, influenza_vaccination, immunosuppressant, inhalation_medication,
                         antibiotic_use, antidepressant) {
  
  # Linear predictor
  lp <- -4.618 +
    0.015 * age +
    0 * female_sex -
    0.001 * age * female_sex +
    0.115 * cmd_1 +
    0.344 * cmd_2 +
    1.015 * pneumonia_diagnosis +
    0.922 * hospitalisation -
    0.137 * pneumonia_history +
    0.393 * malignancy +
    0.112 * copd_asthma +
    0.222 * dementia +
    0.076 * influenza_vaccination +
    0.182 * immunosuppressant +
    0.082 * inhalation_medication +
    0.335 * antibiotic_use +
    0.254 * antidepressant
  
  # Predicted risk
  plogis(lp)
}

# Test cases as described in Supplementary materials
test_cases <- data.frame(
  age = c(76, 50, 88),
  female_sex = c(1, 0, 1),
  cmd_1 = c(1, 0, 0),
  cmd_2 = c(0, 0, 1),
  pneumonia_diagnosis = c(1, 0, 1),
  hospitalisation = c(0, 0, 1),
  pneumonia_history = c(1, 0, 1),
  malignancy = c(0, 0, 0),
  copd_asthma = c(1, 0, 0),
  dementia = c(0, 0, 1),
  influenza_vaccination = c(1, 0, 1),
  immunosuppressant = c(1, 0, 0),
  inhalation_medication = c(1, 0, 0),
  antibiotic_use = c(0, 0, 1),
  antidepressant = c(0, 0, 0)
)

# Predict risk in test cases
test_cases$risk <- with(
  test_cases,
  predict_risk(
    age = age,
    female_sex = female_sex,
    cmd_1 = cmd_1,
    cmd_2 = cmd_2,
    pneumonia_diagnosis = pneumonia_diagnosis,
    hospitalisation = hospitalisation,
    pneumonia_history = pneumonia_history,
    malignancy = malignancy,
    copd_asthma = copd_asthma,
    dementia = dementia,
    influenza_vaccination = influenza_vaccination,
    immunosuppressant = immunosuppressant,
    inhalation_medication = inhalation_medication,
    antibiotic_use = antibiotic_use,
    antidepressant = antidepressant
  )
)

# Calculate risk for any new patient (adjust predictor values based on patient characteristics)
new_patient_risk <- predict_risk(
  age = 75,
  female_sex = 1,
  cmd_1 = 1,
  cmd_2 = 0,
  pneumonia_diagnosis = 1,
  hospitalisation = 0,
  pneumonia_history = 0,
  malignancy = 0,
  copd_asthma = 1,
  dementia = 0,
  influenza_vaccination = 1,
  immunosuppressant = 0,
  inhalation_medication = 1,
  antibiotic_use = 1,
  antidepressant = 0
)
new_patient_risk
