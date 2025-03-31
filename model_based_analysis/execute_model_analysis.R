source(here::here("model_based_analysis", "analysis", "summarize_utility.R"))
source(here::here("model_based_analysis", "preprocess", "MLE_preprocess.R"))
source(here::here("model_based_analysis", "model", "model_definition.R"))
source(here::here("model_based_analysis", "model", "model_utility.R"))
source(here::here("model_based_analysis", "model", "model_prediction.R"))
source(here::here("model_based_analysis", "analysis", "anova_kun.R"))

# Run model fitting (simulation with pararell computation will be executed)
source(here::here("model_based_analysis", "analysis", "MLE.R"))

# parameter analysis
source(here::here("model_based_analysis", "analysis", "param_analysis.R"))
print("param_analysis.R is done")

# time series analysis
source(here::here("model_based_analysis", "analysis", "temporal_analysis.R"))
print("temporal_analysis.R is done")

# model behavior analysis
source(here::here("model_based_analysis", "analysis", "anova_plots.R"))
print("anova_plots.R is done")

# parameter recovery test (simulation with pararell computation will be executed)
source(here::here("model_based_analysis", "model", "parameter_recovery.R"))
print("parameter_recovery.R is done")

# correlation analysis
source(here::here("model_based_analysis", "analysis", "Visualize_correlation.R"))
print("Visualize_correlation.R is done")

# simulation with modified parameters (simulation with pararell computation will be executed)
source(here::here("model_based_analysis", "model", "simulation.R"))
print("simulation.R is done")
