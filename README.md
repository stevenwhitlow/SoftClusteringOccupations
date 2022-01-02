# Soft clustering of occupations
Soft categorization of occupations using on-the-job task data

### Files
#### Grade of Membership
`gom_estimation.r` contains functions to fit models to data. `gom_cv.r` performs the cross-validation procedure for GoM model selection and `gom_model.r` estimates the model selected by CV procedure. The outputs of this model are saved in `/model-output/'.
#### Regression
`stata_code.do` generates income ranks by population using 2000 US Census data. `cleaning.r` combines the various data sources and prepares the data. `regressions.r` and `figures.r` performs the main regression analysis and creates figures respectively.
