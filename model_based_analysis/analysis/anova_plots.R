library(lme4)
library(lmerTest)
library(broom.mixed)
library(coin)
source(here::here("model_based_analysis", "model", "model_prediction.R"))

# load fitted parameters and behaviour data, and make prediction ----
res <-
  df_rule_hit %>%
  makePredictionDf("binary_sign_model", "Distance") %>%
  select(-Distance) %>%
  pivot_longer(
    cols =
      c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom),
    names_to = "switch",
    values_to = "num of trials"
  ) %>%
  mutate(
    score = if_else(DisplayScore > 0, "positive", "negative"),
    PlayerID = as.factor(PlayerID)
  )

# calculate the performance of the prediction ----
df_pre_model_state <-
  res %>%
  # filter(input == "state_error") %>%
  # filter(input == "scaledTransformedError") %>%
  pivot_wider(names_from = switch, values_from = `num of trials`) %>%
  mutate(
    PlayerID = as.factor(PlayerID),
    Correct = if_else(TrueRule == "random", pred, 1 - pred),
    output = pred > 0.5,
    pred_adj = if_else(abs(pred) < 1, pred, pred - 10^(-5)),
    entropy = (pred_adj * log(pred_adj) + (1 - pred_adj) * log(1 - pred_adj))
  ) %>%
  group_by(input, PlayerID) %>%
  mutate(z_entropy = scale(entropy)) %>%
  ungroup()

stopifnot(
  df_pre_model_state %>% dim() %>% `[`(1) ==
    (560) * length(df_rule_hit %>% pull(PlayerID) %>% unique())
)

# input_list =  c("error_binary", "error_binary_2x", "error_binary_3x", "state_error")
# input_list =  c("scaledTransformedError", "anomaly_error", "state_error")
input_list <- c("Distance")

# plot the performance of the prediction (x: task state, y: p(correct state inference), group: score) (Figure 2 E)----
p_model_accuracy_score_truerule <- df_pre_model_state %>%
  filter(input %in% input_list) %>%
  ungroup() %>%
  group_by(input) %>%
  summarize_performance(TrueRule, Correct, score) %>%
  mutate(condition = interaction(TrueRule, score) %>% as.character()) %>%
  ggplot(aes(x = TrueRule, y = mean_Correct, group = interaction(TrueRule, score), fill = TrueRule, alpha = score)) +
  # geom_boxplot(
  #   aes(
  #     color = after_scale(
  #       alpha(if_else(alpha > 0.5, "black", fill), 1))
  #   ),
  #   outlier.shape = NA) + geom_sina(alpha = 0.2, color = "black", show.legend = FALSE) +
  gg_filled_box_sina() +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = "none") +
  # facet_wrap(.~input) +
  labs(alpha = "score") +
  xlab("task state") +
  ylab("p(correcte state inference)") +
  theme_fig_anova +
  theme(legend.position = "right")

p_model_accuracy_score_truerule %>%
  save_svg_figure(paste0("p_distance_model_accuracy_score_truerule"),
    scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
  )

# plot the performance of the prediction (x: task state, y: -(entropy), group: score) (Figure 5 B)----
p_model_entropy_score_truerule <- df_pre_model_state %>%
  filter(input %in% input_list) %>%
  ungroup() %>%
  group_by(input) %>%
  summarize_performance(TrueRule, z_entropy, score) %>%
  mutate(condition = interaction(TrueRule, score) %>% as.character()) %>%
  ggplot(aes(x = TrueRule, y = mean_z_entropy, group = interaction(TrueRule, score), fill = TrueRule, alpha = score)) +
  gg_filled_box_sina() +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = "none") +
  # facet_wrap(.~input) +
  labs(alpha = "score") +
  xlab("task state") +
  ylab("-(entropy)") +
  theme_fig_anova +
  theme(legend.position = "right")

p_model_entropy_score_truerule %>%
  save_svg_figure(paste0("p_distance_model_entropy_score_truerule"),
    scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
  )


# anova and posthoc test for the performance of the prediction ----
# Figure 2 E
df_pre_model_state %>%
  summarize_performance(score, Correct, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_Correct) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_model_acc_score_truerule", analysis_group = "anova")

df_pre_model_state %>%
  summarize_performance(score, Correct, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_Correct) %>%
  posthoc_wilcox_test2(mean_Correct, TrueRule, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_acc_score_truerule", analysis_group = "posthoc_wilcox_test")

# Figure 5 B
df_pre_model_state %>%
  summarize_performance(score, entropy, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_entropy) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_model_entropy_score_truerule", analysis_group = "anova")

df_pre_model_state %>%
  filter(input == "Distance") %>%
  group_by(PlayerID) %>%
  mutate(scaled_entropy = scale(entropy, center = FALSE)) %>%
  ungroup() %>%
  lmerTest::lmer(scaled_entropy ~ TrueRule * score + (1 | PlayerID), data = .) %T>%
  output_lme_results("lme_model_scaled_entropy_score_truerule", analysis_group = "lme")

df_pre_model_state %>%
  summarize_performance(score, entropy, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_entropy) %>%
  posthoc_wilcox_test2(mean_entropy, TrueRule, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_entropy_score_truerule", analysis_group = "posthoc_wilcox_test")

# calculate the switch probability of the model ----
df_model_switch_anova <-
  df_pre_model_state %>%
  mutate(
    score = if_else(DisplayScore > 0, "positive", "negative"),
    output = if_else(output == 1, "random", "skill")
  ) %>%
  group_by(PlayerID, input) %>%
  mutate(prev_output = lag(output)) %>%
  drop_na(prev_output) %>%
  mutate(
    is_switch = (output != prev_output),
    binary_conf = if_else(z_entropy > 0, "high", "low")
  ) %>%
  ungroup() %>%
  drop_na(prev_output)

stopifnot(
  df_model_switch_anova %>% dim() %>% `[`(1) ==
    (560 - 1) * length(df_rule_hit %>% pull(PlayerID) %>% unique())
)

# plot the switch probability of the model (x: previous choice, y: switch probability, group: score) (Figure 3 D) ----
p_model_prev_choice_switch <-
  df_model_switch_anova %>%
  group_by(PlayerID, input, score, prev_output) %>%
  summarise(switch_prob = mean(is_switch)) %>%
  ggplot(aes(x = prev_output, y = switch_prob, group = interaction(prev_output, score), fill = prev_output, alpha = score)) +
  gg_filled_box_sina() +
  theme_fig_anova +
  ylab("switch probability") +
  xlab("previous choice") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(alpha = "score", fill = "") +
  theme(legend.position = "right") +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = FALSE)

df_model_switch_anova %>% dim()
(560 - 1) * length(df_rule_hit %>% pull(PlayerID) %>% unique())


p_model_prev_choice_switch %>%
  save_svg_figure("p_distance_model_prev_choice_switch",
    scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
  )

# plot the switch probability of the model (x: confidence, y: switch probability, group: score) (Figure 5 F) ----
p_model_conf_switch <-
  df_model_switch_anova %>%
  group_by(PlayerID, input, score, binary_conf) %>%
  summarise(switch_prob = mean(is_switch)) %>%
  mutate(binary_conf = factor(binary_conf, levels = c("low", "high"))) %>%
  ggplot(aes(x = binary_conf, y = switch_prob, group = interaction(binary_conf, score), fill = "black", alpha = score)) +
  gg_filled_box_sina() +
  theme_fig_anova +
  ylab("switch probability") +
  xlab("confidence") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(alpha = "score") +
  theme(legend.position = "right") +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = FALSE)

p_model_conf_switch %>%
  save_svg_figure("p_distance_model_conf_switch",
    scaling = fig_anova_scale,
    width = fig_anova_width,
    height = fig_anova_height, unit = "mm"
  )

# anova and posthoc test for the switch probability of the model ----
df_model_switch_anova %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, prev_output) %>%
  select(PlayerID, score, prev_output, mean_is_switch) %>%
  replace_na(list(mean_is_switch = 0)) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis("anova_model_prev_choice_switch", analysis_group = "anova")

df_model_switch_anova %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, prev_output) %>%
  select(PlayerID, score, mean_is_switch, prev_output) %>%
  posthoc_wilcox_test2(mean_is_switch, prev_output, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_prev_choice_switch", analysis_group = "posthoc_wilcox_test")


df_model_switch_anova %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, binary_conf) %>%
  select(PlayerID, score, binary_conf, mean_is_switch) %>%
  mutate(PlayerID = as.character(PlayerID)) %>%
  replace_na(list(mean_is_switch = 0)) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %T>% print() %>%
  sink_analysis("anova_distance_model_conf_switch", analysis_group = "anova")

df_model_switch_anova %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, binary_conf) %>%
  select(PlayerID, score, mean_is_switch, binary_conf) %>%
  posthoc_wilcox_test2(mean_is_switch, binary_conf, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_conf_switch", analysis_group = "posthoc_wilcox_test")


# plot the relationship between model and behaviour accuracy (Supplementary Figure 6) ----

df_model_anova_acc <- df_pre_model_state %>%
  filter(input %in% input_list) %>%
  ungroup() %>%
  group_by(input) %>%
  summarize_performance(TrueRule, Correct, score) %>%
  select(PlayerID, score, TrueRule, mean_Correct)

df_behaviour_anova_acc <- df_rule_hit %>%
  mutate(score = case_when(
    DisplayScore > 0 ~ "positive",
    DisplayScore <= 0 ~ "negative",
    TRUE ~ "neutral"
  )) %>%
  summarize_performance(TrueRule, Correct, score) %>%
  select(PlayerID, score, TrueRule, mean_Correct)

p_model_behaviour_accuracy_score_truerule_facet <- # Supplementary Figure 6
  df_model_anova_acc %>%
  inner_join(
    df_behaviour_anova_acc,
    by = c("PlayerID", "score", "TrueRule"),
    suffix = c("_model", "_behaviour")
  ) %>%
  ggplot(aes(x = mean_Correct_behaviour, y = mean_Correct_model, color = TrueRule)) +
  geom_point(
    aes(
      shape = score,
    ),
    size = 2.5,
    alpha = 0.8,
    # key_glyph = draw_key_dotplot,
  ) +
  geom_line(
    aes(
      group = interaction(score, TrueRule),
      linetype = score
    ),
    stat = "smooth",
    se = FALSE,
    method = lm_robust,
    size = 1.2
  ) +
  xlab("p(correct behaviour inference)") +
  ylab("p(correct model inference)") +
  theme_fig_base +
  theme(legend.position = "right") +
  labs(color = "task state", shape = "score", linetype = "") +
  scale_shape_manual(values = c(
    `positive` = 16,
    `negative` = 21
  )) +
  scale_linetype_manual(values = c(
    `positive` = "solid",
    `negative` = "dashed"
  )) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        shape = c(`positive` = 16, `negative` = 21),
        linewidth = 0.5
      )
    )
  ) +
  facet_wrap(. ~ interaction(score, TrueRule)) +
  theme(panel.spacing.x = unit(2, "lines"))

p_model_behaviour_accuracy_score_truerule_facet %>%
  save_svg_figure("p_model_behaviour_accuracy_score_truerule_facet",
    scaling = fig_anova_scale, width = fig_anova_width * 1.5, height = fig_anova_height * 1.2, unit = "mm"
  )

# spearman correlation for model and behaviour accuracy
corr_model_behaviour_accuracy_score_truerule_group <-
  df_model_anova_acc %>%
  inner_join(
    df_behaviour_anova_acc,
    by = c("PlayerID", "score", "TrueRule"),
    suffix = c("_model", "_behaviour")
  ) %>%
  group_by(score, TrueRule) %>%
  do({
    test <- spearman_test(mean_Correct_model ~ mean_Correct_behaviour, data = .)
    data.frame(
      # value of correlation
      estimate = cor(.$mean_Correct_model, .$mean_Correct_behaviour, method = "spearman"),
      statistic = statistic(test),
      p.value = pvalue(test),
      score = unique(.$score),
      TrueRule = unique(.$TrueRule)
    )
  }) %>%
  ungroup()

corr_model_behaviour_accuracy_score_truerule_group %>%
  rio::export(
    here::here(
      "results",
      "corr_model_behaviour_accuracy_score_truerule_group.csv"
    )
  )


# plot the relationship between model and behaviour confidence (Supplementary Figure 11) ----
df_model_anova_conf <- df_pre_model_state %>%
  filter(input %in% input_list) %>%
  ungroup() %>%
  group_by(input) %>%
  summarize_performance(TrueRule, z_entropy, score) %>%
  select(PlayerID, score, TrueRule, mean_z_entropy)

df_behaviour_anova_conf <- df_rule_hit %>%
  mutate(score = case_when(
    DisplayScore > 0 ~ "positive",
    DisplayScore <= 0 ~ "negative",
    TRUE ~ "neutral"
  )) %>%
  summarize_performance(TrueRule, zConfidence, score) %>%
  select(PlayerID, score, TrueRule, mean_zConfidence)

p_model_behaviour_entropy_score_truerule_facet <- # Supplementary Figure 11
  df_model_anova_conf %>%
  inner_join(
    df_behaviour_anova_conf,
    by = c("PlayerID", "score", "TrueRule"),
    suffix = c("_model", "_behaviour")
  ) %>%
  ggplot(aes(
    x = mean_zConfidence, y = mean_z_entropy,
    color = TrueRule
  )) +
  geom_point(
    aes(
      shape = score,
    ),
    size = 2.5,
    # key_glyph = draw_key_dotplot,
  ) +
  # geom_smooth(method = lm_robust, se = FALSE, size = 1.5) +
  geom_line(
    aes(
      group = interaction(score, TrueRule),
      linetype = score
    ),
    stat = "smooth",
    se = FALSE,
    method = lm_robust,
    size = 1.2
  ) +
  xlab("participants' confidence") +
  ylab("model confidence") +
  theme_fig_base +
  theme(legend.position = "right") +
  labs(color = "task state", shape = "score", linetype = "") +
  scale_shape_manual(values = c(
    `positive` = 16,
    `negative` = 21
  )) +
  scale_linetype_manual(values = c(
    `positive` = "solid",
    `negative` = "dashed"
  )) +
  guides(
    # linetype = "none",  # linetype の凡例を非表示にする
    linetype = guide_legend(
      override.aes = list(
        shape = c(`positive` = 16, `negative` = 21),
        linewidth = 0.5
      )
    ),
  ) +
  facet_wrap(. ~ interaction(score, TrueRule))

p_model_behaviour_entropy_score_truerule_facet %>%
  save_svg_figure("p_model_behaviour_entropy_score_truerule_facet",
    scaling = fig_anova_scale, width = fig_anova_width * 1.5, height = fig_anova_height * 1.2, unit = "mm"
  )

# spearman correlation for model and behaviour confidence
corr_model_behaviour_entropy_score_truerule_group <-
  df_model_anova_conf %>%
  inner_join(
    df_behaviour_anova_conf,
    by = c("PlayerID", "score", "TrueRule"),
    suffix = c("_model", "_behaviour")
  ) %>%
  group_by(score, TrueRule) %>%
  do({
    test <- spearman_test(mean_z_entropy ~ mean_zConfidence, data = .)
    data.frame(
      # value of correlation
      estimate = cor(.$mean_z_entropy, .$mean_zConfidence, method = "spearman"),
      statistic = statistic(test),
      p.value = pvalue(test),
      score = unique(.$score),
      TrueRule = unique(.$TrueRule)
    )
  }) %>%
  ungroup()

corr_model_behaviour_entropy_score_truerule_group %>%
  rio::export(
    here::here(
      "results",
      "corr_model_behaviour_entropy_score_truerule_group.csv"
    )
  )
