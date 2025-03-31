source(here::here("behaviour", "analysis_scripts", "CheckCriteria.R"))

preprocessDfRuleHit <- function(df_rule_hit, df_median, thre_col = "true_threshold") {
  # df_rule_hit <- df_rule_hit %>%
  #   group_by(PlayerID) %>%
  #   nest() %>%
  #   mutate(true_threshold = map2_dbl(data, PlayerID, ~ (.x$threshold <- (df_median %>% filter(PlayerID == .y))[[thre_col]]))) %>%
  #   unnest(cols = c(data, threshold))

  # df_rule_hit <- df_rule_hit %>% mutate(signed = if_else(DisplayScore == 1, -1, 1), TransformedDistance = log(Distance + 1) - log(threshold), TransformedError = signed * (log(Distance + 1) - log(threshold)))

  df_rule_hit <- df_rule_hit %>%
    mutate(EstRuleResponseTime = dmy_hms(EstRuleResponseTime, tz = "Asia/Tokyo"), EstRuleMessagePopupTime = dmy_hms(EstRuleMessagePopupTime, tz = "Asia/Tokyo")) %>%
    mutate(ruleEstimationRT = EstRuleMessagePopupTime %--% EstRuleResponseTime %>% time_length())


  # df_rule_hit <- df_rule_hit %>%
  #   mutate(normalizedDistance = Distance * 0.5 / threshold, normalizedError = 1 - normalizedDistance - DisplayScore) %>%
  #   mutate(normalizedAbsError = ifelse(DisplayScore > 0, -normalizedError, normalizedError))
  df_rule_hit
}

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

# p_criteria = CheckCriteria(df_rule_hit_origin)
#
# df_rule_hit = excludeDfRuleHit(df_rule_hit_origin) %>% preprocessDfRuleHit()



# base_data  <- df_rule_hit
# switch_data <- df_rule_hit
