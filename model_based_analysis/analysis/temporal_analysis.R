error_var <- c("Distance")
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
    PlayerID = as.factor(PlayerID),
    input = if_else(input == "Distance", "binary model", input)
  )

# comparison of the prediction between the direction of the switch ----
res %>%
  pivot_wider(names_from = input, values_from = pred) %>%
  pivot_longer(
    # cols = c(behaviour, `binary model`),
    cols = c(`binary model`),
    names_to = "model",
    values_to = "prob"
  ) %>%
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(prob = mean(prob), performance = mean(if_else(TrueRule == "random", prob, 1 - prob))) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, performance) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
      "skill to random",
      "random to skill"
    )
  ) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>%
  pivot_wider(names_from = switch, values_from = performance) %>%
  group_by(`num of trials`) %>%
  nest() %>%
  mutate(
    wilcoxsign =
      map(
        .x = data, .f =
          ~ .x %>%
            coin::wilcoxsign_test(`skill to random` ~ `random to skill`, data = ., distribution = "exact", zero.method = "Wilcoxon")
      ),
    Z = sapply(wilcoxsign, coin::statistic),
    p_coin = sapply(wilcoxsign, coin::pvalue)
  ) %>%
  unnest(cols = c(p_coin, Z)) %>%
  ungroup() %>%
  adjust_pvalue(p.col = "p_coin", method = "fdr") %>%
  mutate(star = if_else(p_coin.adj < 0.001, "***",
    if_else(p_coin.adj < 0.01, "**",
      if_else(p_coin.adj < 0.05, "*", "")
    )
  )) %>%
  output_posthoc_result(
    "posthoc_distance_model_switch_acc",
    analysis_group = ""
  ) 
# do anova for the prediction ----
res %>%
  pivot_wider(names_from = input, values_from = pred) %>%
  pivot_longer(
    # cols = c(behaviour, `binary model`),
    cols = c(`binary model`),
    names_to = "model",
    values_to = "prob"
  ) %>%
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(prob = mean(prob), performance = mean(if_else(TrueRule == "random", prob, 1 - prob))) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, performance) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
      "skill to random",
      "random to skill"
    )
  ) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>% 
  select(-model) %>% 
  anovakun("sAB",
    long=TRUE,
    gg=TRUE) %>% 
    print()%>% 
  sink_analysis(
    "anova_distance_model_switch_acc",
    analysis_group = ""
  )


  # plot the time-series of prediction (Figure 3 B) ----

p_model_switch_acc <-
  res %>%
  pivot_wider(names_from = input, values_from = pred) %>%
  pivot_longer(
    # cols = c(behaviour, `binary model`),
    cols = c(`binary model`),
    names_to = "model",
    values_to = "prob"
  ) %>%
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(prob = mean(prob), performance = mean(if_else(TrueRule == "random", prob, 1 - prob))) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, prob, performance) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
      "skill to random",
      "random to skill"
    )
  ) %>%
  ggplot(aes(x = `num of trials` + 1, y = performance, color = model, linetype = switch)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 1.0,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-3, 8), ylim = c(0.3, 0.8)) +
  xlab("trials from the switch") +
  ylab("p(correct state inference)") +
  theme_fig_timeseries +
  theme(axis.title.y = element_text(size = 46 / fig_anova_scale))

p_model_switch_acc %>%
  save_svg_figure("p_distance_model_switch_acc", scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height, unit = "mm")

# plot the timeseries of entropy (Figure 3 C) ----
p_model_switch_conf <-
  res %>%
  mutate(entropy = (pred * log(pred) + (1 - pred) * log(1 - pred)), behaviour = zConfidence) %>%
  select(-pred) %>%
  pivot_wider(names_from = input, values_from = entropy) %>%
  pivot_longer(
    # cols = c(behaviour, `binary model`),
    cols = c(`binary model`),
    names_to = "model",
    values_to = "entropy"
  ) %>%
  group_by(PlayerID, model) %>%
  mutate(entropy = scale(entropy)) %>%
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(entropy = mean(entropy)) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, entropy) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
      "skill to random",
      "random to skill"
    )
  ) %>%
  ggplot(aes(x = `num of trials` + 1, y = entropy, color = model, linetype = switch, shape = model)) +
  # geom_point() +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 1.0,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-3, 8), ylim = c(-0.35, 0.25)) +
  xlab("trials from the switch") +
  ylab("confidence\n(negative entropy)") +
  theme_fig_timeseries +
  theme(axis.title.y = element_text(size = 46 / fig_anova_scale))

p_model_switch_conf %>%
  save_svg_figure("p_distance_model_switch_conf", scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height, unit = "mm")

p_model_switch_conf_15 <-
  res %>%
  mutate(entropy = (pred * log(pred) + (1 - pred) * log(1 - pred)), behaviour = zConfidence) %>%
  select(-pred) %>%
  pivot_wider(names_from = input, values_from = entropy) %>%
  pivot_longer(
    # cols = c(behaviour, `binary model`),
    cols = c(`binary model`),
    names_to = "model",
    values_to = "entropy"
  ) %>%
  group_by(PlayerID, model) %>%
  mutate(entropy = scale(entropy)) %>%
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(entropy = mean(entropy)) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, entropy) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
      "skill to random",
      "random to skill"
    )
  ) %>%
  ggplot(aes(x = `num of trials` + 1, y = entropy, linetype = switch, group = interaction(switch, model))) +
  # geom_point() +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 1.0,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-15, 15), ylim = c(-0.35, 0.25)) +
  xlab("trials from the switch") +
  ylab("confidence\n(negative entropy)") +
  theme_fig_timeseries +
  tick_for_time_series_15 + 
  # change legend position to top right inside the plot and make it transparent
  theme(legend.position = c(0.2, 0.22), legend.background = element_rect(fill = "transparent")) +
  labs(
    linetype = ""  
    ) +
  theme(axis.title.y = element_text(size = 46 / fig_anova_scale))
  
p_model_switch_conf_15 %>%
  save_svg_figure("p_distance_model_switch_conf_15", scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height, unit = "mm")

# res %>%
#   process_for_summary_anova() %>%
#   do_anova(prev_choice, DisplayScore, pred) %>%
#   print_anova_sentences()
# df_rule_hit %>%
#   process_for_summary_anova() %>%
#   do_anova(conf_binary, DisplayScore, switch) %>%
#   print_anova_sentences()
# df_rule_hit %>%
#   process_for_summary_anova() %>%
#   do_anova(prev_conf_binary, DisplayScore, switch) %>%
#   print_anova_sentences()

# res %>%
#   process_for_summary_anova() %>%
#   plot_grouped_summary(prev_choice, pred, DisplayScore)

# plot correlation between confidence and negative entropy
(res %>%
  mutate(entropy = (pred * log(pred) + (1 - pred) * log(1 - pred))) %>%
  group_by(PlayerID, input) %>%
  mutate(entropy = scale(entropy)) %>%
  ungroup() %>%
  ggplot(aes(x = zConfidence, y = entropy, color = factor(DisplayScore))) +
  geom_point() +
  geom_smooth(method = lm_robust, se = FALSE, color = "black") +
  theme_fig_base +
  theme(legend.position = "none") +
  xlab("z-scored confidence") +
  ylab("negative entropy")) %>%
  save_svg_figure("p_distance_conf_entropy", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")

# mixed effect model for entropy ====
res %>%
  mutate(entropy = (pred * log(pred) + (1 - pred) * log(1 - pred))) %>%
  group_by(PlayerID, input) %>%
  mutate(entropy = scale(entropy)) %>%
  ungroup() %>%
  lmerTest::lmer(entropy ~ zConfidence + (zConfidence | PlayerID), data = .) %>%
  summary() %>%
  print() %>% 
  sink_analysis(
    "lmer_entropy_conf",
    analysis_group = ""
  )
