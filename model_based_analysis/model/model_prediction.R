library(stringr)
library(ggplot2)
library(tidyverse)
library(rio)
library(magrittr)

library(doParallel)

source(here::here("model_based_analysis", "analysis", "summarize_utility.R"))

predict_from_model <- function(df_params, model, df_input, input_list) {
  # browser()
  df_params %>%
    inner_join(df_input %>%
      select(
        PlayerID,
        TrialID,
        TrialsAfterSwitch,
        TrialsAfterSwitchToRandom,
        TrialsAfterSwitchToSkill,
        DisplayScore,
        TrueRule,
        EstRule,
        zConfidence,
        EstRuleConfidence,
        any_of(c("state")),
        true_threshold,
        Distance,
        .env$input_list
      ) %>%
      group_by(PlayerID) %>%
      nest(), by = "PlayerID") %>%
    rename("df" = data) %>%
    group_by(PlayerID) %>%
    nest(data = c(-number_of_params, -log_likelihood, -AIC, -df, -PlayerID)) %>%
    mutate(
      prediction =
        map2(
          data, df,
          ~ model$predict_choice(
            .y %>% `$`(!!input_list) %>% as.vector(),
            .x %>% as.list() %>% unlist(),
            .y$DisplayScore %>% as.vector()
          ) # %>% `>`(0.5) %>% as.numeric()
        ),
      pred_data =
        map2(
          df, prediction,
          ~ .x %>%
            add_column(pred = .y %>% as.vector())
        )
    ) %>%
    unnest(cols = c(pred_data))
}

calc_Z_bias_from_p <- function(p_vector) {
  # apply logit to p_vector
  logit_p <- logit(p_vector)
  logit_p
}

calc_Z_from_Z_bias <- function(Z_bias, bias) {
  Z <- Z_bias - bias
  Z
}

calc_bias <- function(b_G, b_B, score) {
  bias <- if_else(is_score_positive(score), b_G, b_B)
  bias
}

predict_from_file <- function(df_rule_hit, estimation_filename, input_list) {
  estimation_filename <- here::here("model_based_analysis", estimation_filename)
  model <- estimation_filename %>%
    sub("estimation", "model", .) %>%
    import()
  rule_dir <- estimation_filename %>%
    `[`(str_detect(., pattern = ".*obj.*estimation.rds")) %>%
    str_extract(pattern = "(?<=_obj_).*(?=_estimation)") %>%
    str_match(pattern = "[^_]*$")
  input <- estimation_filename %>%
    `[`(str_detect(., pattern = ".*obj.*estimation.rds")) %>%
    str_extract(pattern = "(?<=_obj_).*(?=_estimation)") %>%
    str_match(pattern = "^.*(?=_)")

  df_estimation <- estimation_filename %>%
    import() %>%
    as_tibble() %>%
    mutate(PlayerID = PlayerID %>% as.factor())

  # print(head(df_estimation))

  if (!("threshold" %in% (df_estimation %>% colnames()))) {
    # print("true_threshold not found in estimation file, adding from df_rule_hit")
    # browser()
    df_estimation <- df_estimation %>%
      inner_join(
        df_rule_hit %>%
          select(PlayerID, true_threshold) %>%
          group_by(PlayerID) %>%
          summarise(true_threshold = unique(true_threshold), .groups = "drop"),
        by = "PlayerID"
      ) %>%
      mutate(threshold = true_threshold) %>%
      select(-true_threshold)

    # print(head(df_estimation %>% select(PlayerID, threshold)))
  }
  print(rule_dir)
  print(input)

  # prediction from model and parameters
  df_prediction <- predict_from_model(df_estimation, model, df_rule_hit, input_list)

  df_prediction
}

makePredictionDf <- function(df_rule_hit, model_name, error_var) {
  foreach(str_ = error_var, .combine = rbind) %do% {
    # estimation_filename = sprintf("R_result/alpha_GB_beta_GB_gamma_sign_accum_model_obj_%s_random_estimation.rds", str_)
    estimation_filename <- sprintf("R_result/%s_obj_%s_random_estimation.rds", model_name, str_)
    df_rule_hit %>%
      predict_from_file(
        estimation_filename,
        error_var
      ) %>%
      mutate(behaviour = as.numeric(EstRule == "random"), input = str_) %>%
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
        all_of(error_var),
        data
      )
  }
}
