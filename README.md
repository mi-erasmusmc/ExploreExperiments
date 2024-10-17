# EXPLORE: Learning Interpretable Rules for Patient-Level Prediction  

This Repository contains the code to run the experiments on (other) databases mapped to the Observational Medical Outcomes Partnership Common Data Model (OMOP CDM).

Main dependencies:
- PatientLevelPrediction R package, see: https://github.com/ohdsi/patientlevelprediction
(for this study use branch: https://github.com/mi-erasmusmc/PatientLevelPrediction/tree/explore)
- Explore R package, see: https://github.com/mi-erasmusmc/explore

Steps:
1. Cohort generation in database mapped to OMOP CDM
2. Execute experiments using analysis/code-to-run.R
3. Visualize results using shiny application or analysis/code_figures_paper.R
