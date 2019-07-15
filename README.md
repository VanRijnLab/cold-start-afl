
## Contents

Processed data files are located in the [`data`](data) folder.
They are saved as `.fst` files, which can be read using the `fst` R package (see https://www.fstpackage.org/).

Analysis scripts (R / Rmd) are located in the [`analysis`](analysis) directory.
They are numbered in the intended order of execution.

Main scripts:
- [`01_collect_data.R`](analysis/01_collect_data.R) reads in the raw CSV files and creates usable data frames for the rest of the analysis.
- [`02_calculate_model_estimates.R`](analysis/02_calculate_model_estimates.R) reconstructs the relevant parameter values of the adaptive fact learning model (incl. rate of forgetting).
- [`03_descriptive_stats_session1.Rmd`](analysis/03_descriptive_stats_session1.Rmd) is a notebook with descriptives of performance in the first experimental session.
- [`04_alpha_analysis.Rmd`](analysis/04_alpha_analysis.Rmd) is a notebook analysing the rate of forgetting estimates obtained in the first session.
- [`05_generate_fact_predictions.R`](analysis/05_generate_fact_predictions.R) makes fact- and domain-level predictions of rate of forgetting to be used in the second session.
- [`06_plot_examples.R`](analysis/06_plot_examples.R) creates some visualisations of session 1 rate of forgetting estimates and model predictions.
- [`07_make_session2_predictions.R`](analysis/07_make_session2_predictions.R) uses the data from session 1 and the first block of session 2 to reconstruct the predictions used in the second block of session 2.
- [`08_visualise_predictions.R`](analysis/08_visualise_predictions.R) plots the prediction matrix and map shown on the poster.
- [`09_session2_H1_learning_outcomes.Rmd`](analysis/09_session2_H1_learning_outcomes.Rmd) is a notebook testing Hypothesis 1 (improved learning outcomes).
- [`10_session2_H2_prediction_accuracy_1.Rmd`](analysis/10_session2_H2_prediction_accuracy_1.Rmd) is a notebook testing Hypothesis 2 (more accurate predictions when combining learner and fact information).
- [`11_session2_H3_prediction_accuracy_2.Rmd`](analysis/11_session2_H3_prediction_accuracy_2.Rmd) is a notebook testing Hypothesis 3 (more accurate predictions when predicting individual facts and/or learners instead of making a domain-level prediction).

Supporting scripts:
- [`bayes_funs.R`](analysis/bayes_funs.R) implements the Bayesian model functions.
- [`geom_flat_violin.R`](analysis/geom_flat_violin.R) implements the half-violin plot that is used in some of the figures.
- [`slimstampen_model_funs.R`](analysis/slimstampen_model_funs.R) implements the adaptive fact learning model functions.
  

Figures generated by the analysis scripts can be found in the [`output`](output) folder.

The stimuli used in both sessions are located in the [`stimuli`](stimuli) directory.

## OSF project page
[https://osf.io/snfyz/](https://osf.io/snfyz/)