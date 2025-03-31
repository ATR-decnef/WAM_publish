df_score_hit_performance = df_score_hit %>% group_by(PlayerID) %>% summarise(dis_var_score = var(Distance), score_acc = var(EstScore - TrueScore), score_diff = mean(EstScore - TrueScore)) %>% mutate(PlayerID = as.factor(PlayerID))

df_rule_hit  %>%
mutate(score = if_else(DisplayScore > 0, "positive", "negative")) %>% 
  summarize_performance(TrueRule, zConfidence, score) %>% 
  inner_join(df_MLE_result_binary %>%
               inner_join(df_rule_hit %>%
                            group_by(PlayerID) %>%
                            summarise(true_threshold = median(threshold)), by = "PlayerID") %>% 
               pivot_wider(names_from = params, values_from = value) %>% mutate(large_threshold = threshold > true_threshold) %>% select(PlayerID, threshold, true_threshold, large_threshold), by="PlayerID") %>% mutate(condition = interaction(TrueRule, score) %>% as.character()) %>%
ggplot(aes(x = TrueRule, y = mean_zConfidence, group = interaction(TrueRule, score), fill = TrueRule, alpha = score)) +
geom_boxplot(
aes(
color = after_scale(
alpha(if_else(alpha > 0.5, "black", fill), 1))
),
outlier.shape = NA) + geom_sina(aes(size = threshold / true_threshold), alpha = 0.2,) +
# facet_wrap(.~input) +
labs(alpha = "score") +
xlab("task state") +
ylab("-(entropy)") +
theme_fig_anova  +
theme(legend.position = "right")  + scale_color_continuous()

df_MLE_result_binary %>% pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_rule_hit_performance, by="PlayerID") %>% ggplot(aes(x = threshold / true_threshold, y = a_G)) + 
  geom_point() + stat_smooth(method = lm_robust) + theme_fig_base

df_MLE_result_binary %>% pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_rule_hit_performance, by="PlayerID") %>% mutate(threshold_ratio = threshold / true_threshold) %>% 
  lm_robust(a_G ~ threshold_ratio, data = .) %>% summary()

df_MLE_result_binary %>% pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  ggplot(aes(x = threshold / true_threshold, y = a_G)) +
  geom_point() + stat_smooth(method = lm_robust) + theme_fig_base

df_MLE_result_binary %>% pivot_wider(names_from = params, values_from = value) %>% inner_join(df_rule_hit_performance, by="PlayerID") %>% mutate(threshold_ratio = threshold / true_threshold) %>% lm_robust(a_G ~ threshold_ratio, data = .) %>% summary()


df_MLE_result_binary %>% ungroup() %>% 
  pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_rule_hit_performance %>% ungroup(), by = "PlayerID") %>% 
  mutate(threshold_ratio = threshold / true_threshold) %>% 
  select(-PlayerID, -number_of_params, -name, -log_likelihood) %>% 
  ggpairs()

df_MLE_result_binary %>% ungroup() %>% 
  pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_score_hit_performance, by = "PlayerID") %>% 
  inner_join(df_rule_hit_performance %>% select(PlayerID, true_threshold), by = "PlayerID") %>% 
  mutate(threshold_ratio = threshold / true_threshold) %>% 
  select(-PlayerID, -number_of_params, -name, -log_likelihood) %>% 
  ggpairs()

df_MLE_result_binary %>% 
  pivot_wider(names_from = params, values_from = value) %>% inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  ggplot(aes(x = threshold / true_threshold, y = a_G)) + geom_point() + stat_smooth(method = lm_robust) + theme_fig_base

df_MLE_result_binary %>% 
  pivot_wider(names_from = params, values_from = value) %>% 
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  ggplot(aes(x = threshold / true_threshold, y = performance)) + geom_point() + stat_smooth() + theme_fig_base

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "skill") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_ent = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -PlayerID, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  mutate(thre_ratio = threshold / true_threshold) %>% 
  ggpairs()

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "random") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_ent = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -PlayerID, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  filter(a_G < 15 & a_B < 15 & b_G < 12) %>%
  mutate(thre_ratio = threshold / true_threshold) %>% 
  select(positive, negative, thre_ratio, a_G, a_B, b_G, b_B) %>% 
  ggpairs(title="conf in random")

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "skill") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_ent = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -PlayerID, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  filter(a_G < 15 & a_B < 15 & b_G < 12) %>%
  mutate(thre_ratio = threshold / true_threshold) %>% 
  select(positive, negative, thre_ratio, a_G, a_B, b_G, b_B) %>%
  ggpairs(title = "conf in skill")

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "random") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_conf = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -PlayerID, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  # filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  
  mutate(thre_ratio = threshold / true_threshold) %>% 
  ggplot(aes(y = a_B, x =  negative)) + geom_point() + stat_smooth(method = lm_robust)

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "skill") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_conf = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -PlayerID, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  # filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  
  mutate(thre_ratio = threshold / true_threshold) %>% 
  ggplot(aes(y = a_B, x =  negative)) + geom_point() + stat_smooth(method = lm_robust)



df_score_conf_lme = 
  tibble(
    PlayerID = df_score_hit %>% pull(PlayerID) %>% unique() %>% as.character(),
    mean_abs_error = df_score_hit %>% 
      ungroup() %>% 
      mutate(Error = abs(EstScore - TrueScore)) %>% group_by(PlayerID) %>% summarise(mean = mean(Error)) %>% ungroup() %>% pull(),
    mean_error = df_score_hit %>% 
      ungroup() %>% 
      mutate(Error = (EstScore - TrueScore)) %>% group_by(PlayerID) %>% summarise(mean = mean(Error)) %>% ungroup() %>% pull(),
    estimate = 
      df_score_hit %>% 
      ungroup() %>% 
      mutate(Error = abs(EstScore - TrueScore)) %>% 
      group_by(PlayerID) %>% 
      mutate(zConfidence = scale(EstScoreConfidence), 
             scaledError = scale(Error)) %>% 
      ungroup() %>% 
      lmer(scaledError ~ zConfidence + (1 + zConfidence | PlayerID), data = .) %>% coef() %>% 
      `$`(PlayerID) %>% 
      `$`(zConfidence)
  ) %>% inner_join(df_rule_hit_performance %>% mutate(PlayerID = as.character(PlayerID)) %>% select(PlayerID, true_threshold))

df_score_conf_lme_param = df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "random") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_conf = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  # filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  
  mutate(thre_ratio = threshold / true_threshold) %>% select(PlayerID, thre_ratio) %>% inner_join(df_score_conf_lme)
(df_score_conf_lme_param %>% 
  ggplot(aes(x = thre_ratio, y = mean_error)) + 
    geom_point() +
    stat_smooth(method = lm_robust, color = "black") + 
    xlab("threshold ratio") + 
    ylab("mean of error of score prediction") + 
    theme_fig_base) %>% 
  save_svg_figure("score estimation error and threshold ratio")

df_score_conf_lme_param %>% 
  lm_robust(mean_error ~ thre_ratio, data = .) %>% summary() %>% print() %>% 
  sink_analysis("lm_robust", analysis_group = "score estimation error and threshold ratio")

(df_score_conf_lme_param %>% 
  ggplot(aes(x = thre_ratio, y = mean_abs_error)) + 
    geom_point() +
    stat_smooth(method = lm_robust, color = "black") + 
    xlab("threshold ratio") + 
    ylab("mean of absolute error of score prediction") + 
    theme_fig_base) %>% 
  save_svg_figure("score estimation abs error and threshold ratio")
df_score_conf_lme_param %>% 
  lm_robust(mean_abs_error ~ thre_ratio, data = .) %>% summary() %>% print() %>% 
  sink_analysis("lm_robust", analysis_group = "score estimation abs error and threshold ratio")

(df_score_conf_lme_param %>% 
    ggplot(aes(x = thre_ratio, y = estimate)) + 
    geom_point() +
    stat_smooth(method = lm_robust, color = "black") + 
    xlab("threshold ratio") + 
    ylab("estimate") + 
    theme_fig_base) %>% 
  save_svg_figure("estimate and threshold ratio")

df_score_conf_lme_param %>% 
  lm_robust(estimate ~ thre_ratio, data = .) %>% summary() %>% print() %>% 
  sink_analysis("lm_robust", analysis_group = "estimate and threshold ratio")

  df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "random") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_conf = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  # filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  
  mutate(thre_ratio = threshold / true_threshold) %>% select(PlayerID, thre_ratio) %>% inner_join(df_score_conf_lme) %>% 
  lm_robust(mean_abs_error ~ thre_ratio, data = .) %>% summary()

df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit_performance, by="PlayerID") %>% 
  inner_join(df_rule_hit %>% mutate(DisplayScore  = 
                                      if_else(DisplayScore > 0, "positive", "negative")) %>%
               group_by(PlayerID, DisplayScore, TrueRule) %>%
               summarise(mean_conf = mean(zConfidence)) %>% filter(TrueRule == "random") %>%  
               pivot_wider(names_from = DisplayScore, values_from = mean_conf) %>% 
               mutate(diff_conf = positive - negative) %>% ungroup(), 
             by ="PlayerID") %>% 
  ungroup() %>% select(-name, -AIC, -number_of_params, -log_likelihood, -TrueRule) %>% 
  # filter(a_G < 15 & a_B < 15 & b_G < 12) %>%  
  mutate(thre_ratio = threshold / true_threshold) %>% select(PlayerID, thre_ratio) %>% inner_join(df_score_conf_lme) %>% 
  lm_robust(mean_error ~ thre_ratio, data = .) %>% summary()
