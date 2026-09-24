This repository contains R scripts that were used in a prediction model development and external validation study among patients aged ≥40 years 
presenting to general practice with a lower respiratory tract infection (LRTI).

The publication of the study can be found via this link: (to be determined, currently submitted)

Inclusion criteria:
- Age ≥40 years at index date (i.e. LRTI diagnosis)
- General practitioner (GP) diagnosis of LRTI, defined as registration of either pneumonia (ICPC code R81) or acute bronchitis (ICPC code R78)

Outcome of interest:
- All-cause hospitalisation or mortality within 30 days from LRTI diagnosis

Cohorts derivation:
- Model development: derived from the Julius General Practitioners' Network (JGPN) from the region of Utrecht, the Netherlands. Period: 2016 - 2019.
- Model validation: derived from the Academic Network of General Practitioners Amsterdam (ANHA) from the region of Amsterdam, the Netherlands. Period: 2022 - 2023.

Statistical methods:
Stepwise model development and external validation using elastic net penalized logistic regression. Four models were developed: basic model (age, sex, interaction term),
cardiometabolic model (basic + cardiometabolic diseases), clinical model (cardiometabolic model + other medical history, medication use, and LRTI diagnosis code), and 
electronic health records (EHR) model (clinical model + algorithm-based predictors on smoking status, frailty, and number of consultations in previous year).All models
were internally validated by bootstrapping (1,000 samples). The final model was chosen based on model performance at internal validation and complexity. Temporal internal-
external cross-validation (based on year) was performed for the final model. The final model was then externally validated in the ANHA cohort. Model performance was assessed
based on measures of discrimination (c-statistic), calibration (intercept and slope), distribution of predicted risks, and decision-curve analysis (upon external validation).

Information on R scripts published on this repository:
- (development cohort preparation)
- (validation cohort preparation)
- (model development)
- (model validation)
- Executable_prediction_function_and_examples.R: R script that provides an executable prediction function to predict individual risk, including calculation of predicted
  risks for three example patients (corresponding to the examples included in the Supplementary materials)

  ! Please note that this risk calculating prediction function is not yet Medical Device Regulation (MDR-)approved, and is thus not intended for medical use.
