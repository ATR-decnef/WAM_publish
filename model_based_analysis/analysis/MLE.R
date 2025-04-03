library(rstatix)
library(boot)
library(estimatr)
library(patchwork)
library(latex2exp)
library(tidyverse)
library(ggsignif)
library(ggrepel)
library(ggforce)
library(doParallel)
library(rio)
library(corrplot)
library(rlang)
library(ggthemes)
library(GGally)

source(here::here("model_based_analysis", "model", "model_definition.R"))
source(here::here("model_based_analysis", "model", "model_utility.R"))
source(here::here("model_based_analysis", "preprocess", "MLE_preprocess.R"))

model_obj_list <- c(
  # alpha_GB_beta_GB_gamma_sign_model_obj = alpha_GB_beta_GB_gamma_sign_model_obj
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  # binary_sign_true_twice_theta_model_obj = binary_sign_true_theta_model_obj #####
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)



# cluster <- makeCluster(min(10, getOption("mc.cores", detectCores())), outfile = "")
# registerDoParallel(cluster)
# for(col in c("state_error", "state_error_2", "scaledTransformedError", "TransformedError", "anomaly_error")){
# for(col in c("anomaly_error")){
# for(col in c("error_binary_2x","error_binary_3x")){
for (col in c("Distance")) {
  print(paste0("a loop for the input ", col, " started"))
  for (model_name in names(model_obj_list)) {
    print(paste0("a loop for the model ", model_name, " started"))
    model_obj <- model_obj_list[[model_name]]

    df_rule_hit_modify <- df_rule_hit %>% mutate(threshold = 1 * true_threshold)
    estimate_model(df_rule_hit_modify, model_obj, model_name, col, "random")

    print(paste0("a loop for the model ", model_name, " ended"))
  }
  print(paste0("a loop for the input ", col, " ended"))
}
# stopCluster(cluster)
print(paste0("MLE ended"))
