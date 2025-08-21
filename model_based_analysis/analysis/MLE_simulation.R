library(tidyverse)
library(rio)
library(here)

# load model definitions and utility functions
source(here("model_based_analysis", "model", "model_definition.R"))
source(here("model_based_analysis", "model", "model_utility.R"))
source(here("model_based_analysis", "model", "model_prediction.R"))
# source(here("model_based_analysis", "model", "simulation.R"))
# data preprocessing provides df_rule_hit
source(here("model_based_analysis", "preprocess", "MLE_preprocess.R"))

run_simulation_and_mle <- function(simulation_model_name, model_obj_list, df_rule_hit) {
  param_file <- fs::path("R_result", paste0(simulation_model_name, "_Distance_random_estimation.rds"))
  sim_df <- df_rule_hit %>%
    predict_from_file(param_file, c("Distance")) %>%
    mutate(behaviour = as.numeric(EstRule == "random"), input = "Distance") %>%
    select(
      PlayerID, TrialID,
      TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom,
      behaviour, pred, input,
      zConfidence, EstRuleConfidence,
      DisplayScore,
      TrueRule, EstRule,
      any_of(c("state", "error")),
      Distance,
      true_threshold,
      all_of(c("Distance")),
      data
    ) %>%
    select(
      "PlayerID", "TrialID",
      "TrialsAfterSwitchToSkill", "TrialsAfterSwitchToRandom", "pred", "DisplayScore",
      "TrueRule", "Distance", "true_threshold"
    ) %>%
    mutate(
      pred_adj = if_else(abs(pred) < 1, pred, pred - 10^(-5)),
      entropy = (pred_adj * log(pred_adj) + (1 - pred_adj) * log(1 - pred_adj)),
      EstRule = if_else(pred_adj > 0.5, "random", "skill"),
      threshold = true_threshold
    ) %>%
    group_by(PlayerID) %>%
    mutate(
      zConfidence = scale(entropy, center = TRUE, scale = TRUE)
    ) %>%
    ungroup() %>%
    select(
      PlayerID, TrialID,
      TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom,
      DisplayScore,
      TrueRule, EstRule, pred,
      Distance, threshold,
      zConfidence
    )
  print(paste0("MLE simulation started for ", simulation_model_name))
  for (model_name in names(model_obj_list)) {
    cat(sprintf("fitting %s\n", model_name))
    model_obj <- model_obj_list[[model_name]]
    estimate_model(
      sim_df,
      model_obj,
      model_name,
      "Distance",
      "random",
      suffix = simulation_model_name
    )
  }
  print(paste0("MLE simulation ended for ", simulation_model_name))
}

model_obj_list <- c(
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

simulation_model_names <- names(model_obj_list)

for (sim_model in simulation_model_names) {
  run_simulation_and_mle(sim_model, model_obj_list, df_rule_hit)
}

# pattern_str <- paste0("binary_sign.*Distance_random_", sim_model, "_estimation\\.rds")
pattern_str <- paste0("binary_sign.*Distance_random_.*_estimation\\.rds")

est_result_files <- list.files(
  here("model_based_analysis", "R_result"),
  pattern = pattern_str,
  full.names = TRUE
)

est_results <-
  map(est_result_files, ~ {
    df <- rio::import(.x, trust = TRUE)
    file <- basename(.x)
    df$name <- stringr::str_extract(file, ".*(?=_Distance_random_)")
    df$simulation <- stringr::str_extract(file, "(?<=_Distance_random_).*(?=_estimation)")
    df
  }) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(PlayerID = as.factor(PlayerID)) %>%
  mutate(
    name = case_when(
      name == "binary_sign_model_obj" ~ "full",
      name == "binary_sign_no_gamma_model_obj" ~ "no accumulation",
      name == "binary_sign_common_beta_model_obj" ~ "common constant term",
      name == "binary_sign_common_alpha_model_obj" ~ "common error sensitivity",
      name == "binary_sign_common_alpha_common_beta_model_obj" ~ "common error sensitivity, constant term",
      name == "binary_sign_true_theta_model_obj" ~ "true threshold",
      TRUE ~ name
    ),
    simulation = case_when(
      simulation == "binary_sign_model_obj" ~ "full model",
      simulation == "binary_sign_no_gamma_model_obj" ~ "no accumulation",
      simulation == "binary_sign_common_beta_model_obj" ~ "common constant term",
      simulation == "binary_sign_common_alpha_model_obj" ~ "common error sensitivity",
      simulation == "binary_sign_common_alpha_common_beta_model_obj" ~ "common error sensitivity, constant term",
      simulation == "binary_sign_true_theta_model_obj" ~ "true threshold",
      TRUE ~ simulation
    )
  )

# specify the order of models for plotting
model_order <- c(
  "full",
  "common constant term",
  "common error sensitivity",
  "common error sensitivity, constant term",
  "true threshold",
  "no accumulation"
)

# extract the model with the minimum AIC for each simulation
min_aic_points <- est_results %>%
  group_by(simulation, name) %>%
  summarise(AIC = mean(AIC, na.rm = TRUE)) %>%
  group_by(simulation) %>%
  filter(AIC == min(AIC, na.rm = TRUE)) %>%
  ungroup()

# Supplementary Figure 9
p_AIC_boxplot_across_models_for_simulated <- est_results %>%
  ggplot(aes(x = AIC, y = factor(name, levels = model_order), group = name, fill = name)) +
  stat_summary(
    fun = mean,
    width = 0.8,
    geom = "col",
    fill = "black",
    color = "black",
    alpha = 0.2
  ) +
  stat_summary(
    fun = mean, geom = "text",
    aes(
      label = sprintf("%.1f", after_stat(x)),
      hjust = ifelse(after_stat(x) < 150, -0.2, 1.2),
      color = ifelse(after_stat(x) < 150, "black", "white")
    ),
    size = 5
  ) +
  theme_fig_base +
  theme(aspect.ratio = 1 / 1.414, legend.position = "none") +
  # scale_y_discrete(
  #   labels = function(x) paste(x, "model")
  # ) +
  ylab("") +
  xlab("AIC") +
  facet_wrap(~simulation, ncol = 2) +
  # remove the boxline and color of the facet ribbon
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  # scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
  scale_color_identity()

p_AIC_boxplot_across_models_for_simulated %>%
  save_svg_figure("AIC boxplot across models for each simulated data",
    width = 1000,
    height = 1200,
    scaling = fig_anova_scale, unit = "mm",
    analysis_group = "simulated_data"
  )
