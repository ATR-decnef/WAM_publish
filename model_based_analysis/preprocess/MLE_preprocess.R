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
# source(here::here("MLE_functions.R"))
source(here::here("model_based_analysis", "model", "anomaly_calculation.R"))
source(here::here("model_based_analysis", "preprocess", "CheckCriteria.R"))

find_true_threshold <- function(df, id = PlayerID, state_col = TrueRule, score_col = DisplayScore, distance_col = Distance, skill_sym = "skill") {
  res <- df %>%
    mutate(
      {{ score_col }} := case_when(
        # 大文字小文字を無視して "positive" または "good" の場合
        grepl("positive|good", {{ score_col }}, ignore.case = TRUE) ~ 1,
        # 大文字小文字を無視して "negative" または "bad" の場合
        grepl("negative|bad", {{ score_col }}, ignore.case = TRUE) ~ 0,
        # 数値が1以上の場合
        suppressWarnings(as.numeric({{ score_col }})) > 0 ~ 1,
        # 数値が0以下の場合
        suppressWarnings(as.numeric({{ score_col }})) <= 0 ~ 0,
        # 条件に一致しない場合はエラーを発生
        TRUE ~ NA
      )
    ) %>%
    filter({{ state_col }} == skill_sym) %>%
    group_by({{ id }}, {{ score_col }}) %>%
    summarise(
      min_distance = min({{ distance_col }}),
      max_distance = max({{ distance_col }})
    ) %>%
    pivot_longer(names_to = "distance_type", values_to = "distance", cols = c(min_distance, max_distance)) %>%
    mutate(factor = interaction(distance_type, {{ score_col }})) %>%
    select({{ id }}, factor, distance) %>%
    pivot_wider(names_from = factor, values_from = distance) %>%
    select({{ id }}, max_distance.1, min_distance.0) %>%
    mutate(
      true_threshold = (min_distance.0 + max_distance.1) / 2,
      error_of_thr = min_distance.0 - max_distance.1
    ) %>%
    select({{ id }}, error_of_thr, true_threshold)

  stopifnot(all(!is.na(res %>% pull(true_threshold))))
  stopifnot(all(res %>% pull(error_of_thr) >= 0))
  res %>% select({{ id }}, true_threshold)
}

add_true_threshold <- function(df) {
  df %>% inner_join(
    df %>% find_true_threshold()
  )
}


df_score_hit <- read_csv(here::here("data/df_score_hit.csv"))

df_median <- df_score_hit %>%
  group_by(PlayerID) %>%
  summarise(threshold = median(Distance))

df_rule_hit <- read_csv(here::here("data/df_rule_hit_switch.csv")) %>%
  mutate(PlayerID = as.factor(PlayerID)) %>%
  mutate(
    EstRule = str_replace_all(EstRule, pattern = c("luck" = "random")),
    TrueRule = str_replace_all(TrueRule, pattern = c("luck" = "random"))
  ) %>%
  mutate(
    TrueScore = if_else(TrueScore < 0, 0, TrueScore),
    DisplayScore = if_else(DisplayScore < 0, 0, DisplayScore)
  ) %>%
  group_by(PlayerID) %>%
  nest() %>%
  mutate(
    median_distance =
      map2_dbl(
        data, PlayerID,
        ~ (.x$median_distance <- (df_median %>% filter(PlayerID == .y))[["threshold"]])
      )
  ) %>%
  unnest(cols = c(data, median_distance)) %>%
  add_true_threshold() %>%
  excludeDfRuleHit() %>%
  mutate(
    TransformedDistance_shifted = log(Distance + 1) - log(median_distance) - 1,
    TransformedError_shifted =
      if_else(Distance > median_distance, abs(DisplayScore - 0), abs(DisplayScore - 1)),
    # if_else(DisplayScore > 0,
    #         log(Distance + 1) / (log(median_distance + 1))/2,
    #         1 -log(Distance + 1) / (log(median_distance + 1))/2)
    sigmoid_distance = sigmoid(Distance / median_distance) * 2 - 1,
    sigmoid_error = abs(1 - sigmoid_distance - as.numeric(DisplayScore)),
    sigmoid_binary_error = sigmoid_error > 0.2,
    error_binary = abs(as.numeric(Distance < (true_threshold)) - as.numeric(DisplayScore)),
    error_binary_2x = abs((as.numeric(Distance < (true_threshold * 2)) - as.numeric(DisplayScore))),
    error_binary_3x = abs(as.numeric(Distance < (true_threshold * 3)) - as.numeric(DisplayScore)),
    error_binary_5x = abs(as.numeric(Distance < (true_threshold * 5)) - as.numeric(DisplayScore)),
  ) %>%
  ungroup() %>%
  add_anomaly_col(df_score_hit) %>%
  add_true_threshold()

df_rule_hit_performance <-
  df_rule_hit %>%
  group_by(PlayerID) %>%
  summarise(
    performance = mean(Correct),
    dis_var = var(Distance),
    true_threshold = unique(true_threshold)
  )
