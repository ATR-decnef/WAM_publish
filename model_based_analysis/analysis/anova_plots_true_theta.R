library(lme4)
library(lmerTest)
library(broom.mixed)
source(here::here("model_based_analysis", "model", "model_prediction.R"))
res_true_theta = 
  df_rule_hit %>% 
  makePredictionDf("binary_sign_true_theta_model", "Distance") %>% 
  select(-Distance) %>% 
  pivot_longer(cols = 
                 c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), 
               names_to = "switch", 
               values_to = "num of trials") %>% 
  mutate(
    score = if_else(DisplayScore > 0, "positive", "negative"),
    PlayerID = as.factor(PlayerID)
    )


df_pre_model_state_truetheta = 
  res_true_theta %>% 
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

res_true_theta %>% 
  mutate(model = "binary model") %>% 
  group_by(PlayerID, model, switch, `num of trials`) %>%
  summarise(prob = mean(pred), performance = mean(if_else(TrueRule == "random", prob, 1 - prob))) %>%
  dplyr::select(PlayerID, model, switch, `num of trials`, prob, performance) %>%
  mutate(
    switch = if_else(switch == "TrialsAfterSwitchToRandom",
                     "skill to random",
                     "random to skill")) %>%
  # dplyr::filter(switch == "skill to random") %>%
  # dplyr::filter(PlayerID == 12) %>%
  ggplot(aes(x = `num of trials` + 1, y = performance, color=model, linetype = switch)) +
  # geom_point() +
  # geom_line(aes(group = interaction(PlayerID, model, switch)), color = "grey") + 
  stat_summary(fun = mean, geom = "line",position=position_dodge(width=0.2)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 1.0,
               size = 0.8,
               alpha = 0.8,
               position=position_dodge(width=0.2)) +
  geom_vline(xintercept = 1 -0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim=c(-3, 8), ylim = c(0.3, 0.8)) +
  xlab("trials after the switch") +
  ylab("p(correct state inference)") + theme_fig_timeseries + 
  theme(axis.title.y = element_text(size = 46 / fig_anova_scale))

stopifnot(
  df_pre_model_state_truetheta %>% dim() %>% `[`(1)== 
    (560) * length(df_rule_hit %>% pull(PlayerID) %>% unique()))

# input_list =  c("error_binary", "error_binary_2x", "error_binary_3x", "state_error")
# input_list =  c("scaledTransformedError", "anomaly_error", "state_error")
input_list = c("Distance")
df_pre_model_state_truetheta %>% 
  filter(input %in% input_list) %>% 
  group_by(PlayerID, score, input) %>% 
  summarise(mean_Correct = mean(Correct)) %>% 
  ggplot(aes(x = score, y = mean_Correct, group = score , alpha = score, fill = "black")) +
  gg_filled_box_sina() + 
  geom_signif(
    comparisons = list(c("negative","positive")), 
    map_signif_level = T, 
    test = "wilcox.test",
    test.args = list(paired = TRUE), 
    color="black", alpha=1, textsize = 6) + 
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black")))) +
  xlab("score") + 
  ylab("p(correcte state inference)") + 
  coord_cartesian(ylim = c(0.12, 1)) + 
  # facet_wra/p(.~input) + 
  # theme_fig_anova
  theme_fig_anova

df_pre_model_state_truetheta %>% 
  filter(input %in% input_list) %>% 
  group_by(PlayerID, score, input) %>% 
  summarise(mean_entropy = mean(entropy)) %>% 
  ggplot(aes(x = score, y = mean_entropy, group = score , alpha = score, fill = "black")) +
  gg_filled_box_sina() + 
  geom_signif(
    comparisons = list(c("negative","positive")), 
    map_signif_level = T, 
    test = "wilcox.test",
    test.args = list(paired = TRUE), 
    color="black", alpha=1, textsize = 6) +
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black")))) + 
  xlab("score") + 
  ylab("-(entropy)") + 
  # facet_wrap(.~input) + 
  theme_fig_anova + 
  coord_cartesian(ylim=c(-0.8, 0.1))

p_model_true_theta_accuracy_score_truerule = 
  df_pre_model_state_truetheta %>% 
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
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black"))), fill = "none") + 
  # facet_wrap(.~input) + 
  labs(alpha = "score") + 
  xlab("task state") + 
  ylab("p(correcte state inference)") +
  theme_fig_anova + 
  theme(legend.position = "right") 

p_model_true_theta_accuracy_score_truerule %>%
  save_svg_figure(paste0("p_model_true_theta_accuracy_score_truerule"), analysis_group = fs::path("model_true_theta"), 
                  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit="mm")

p_model_true_theta_entropy_score_truerule = 
  df_pre_model_state_truetheta %>% 
  filter(input %in% input_list) %>% 
  ungroup() %>% 
  group_by(input) %>% 
  summarize_performance(TrueRule, z_entropy, score) %>% 
  mutate(condition = interaction(TrueRule, score) %>% as.character()) %>% 
  ggplot(aes(x = TrueRule, y = mean_z_entropy, group = interaction(TrueRule, score), fill = TrueRule, alpha = score)) +
  # geom_boxplot(
  #   aes(
  #     color = after_scale(
  #       alpha(if_else(alpha > 0.5, "black", fill), 1))
  #   ),
  #   outlier.shape = NA) + geom_sina(alpha = 0.2, color = "black", show.legend = FALSE) + 
  gg_filled_box_sina() + 
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black"))), fill = "none") + 
  # facet_wrap(.~input) + 
  labs(alpha = "score") + 
  xlab("task state") + 
  ylab("-(entropy)") + 
  theme_fig_anova  +
  theme(legend.position = "right") 

p_model_true_theta_entropy_score_truerule %>%
  save_svg_figure(paste0("p_model_true_theta_entropy_score_truerule"), analysis_group = fs::path("model_true_theta"),
                  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit="mm")


df_pre_model_state_truetheta %>%
  summarize_performance(score, Correct, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_Correct) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>% 
  sink_analysis(filename = "anova_model_true_theta_acc_score_truerule", analysis_group = "model_true_theta")

df_pre_model_state_truetheta %>% filter(input == "Distance") %>% 
  lmerTest::lmer(Correct ~ TrueRule * score + (1 | PlayerID), data = .) %T>% 
  output_lme_results("lme_model_true_theta_accuracy_score_truerule", analysis_group = "model_true_theta")

df_pre_model_state_truetheta %>%
  summarize_performance(score, Correct, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_Correct) %>%
  posthoc_wilcox_test2(mean_Correct, TrueRule, score) %>% 
  output_posthoc_result("posthoc_wilcox_test_model_true_theta_acc_score_truerule", analysis_group = "model_true_theta")

df_pre_model_state_truetheta %>%
  summarize_performance(score, entropy, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_entropy) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>% 
  sink_analysis(filename = "anova_model_true_theta_entropy_score_truerule", analysis_group = "model_true_theta")

df_pre_model_state_truetheta %>% filter(input == "Distance") %>%
  lmerTest::lmer(entropy ~ TrueRule * score + (1 | PlayerID), data = .) %T>%
  output_lme_results("lme_model_true_theta_entropy_score_truerule", analysis_group = "model_true_theta")
df_pre_model_state_truetheta %>% filter(input == "Distance") %>% group_by(PlayerID) %>% mutate(scaled_entropy = scale(entropy, center = FALSE)) %>% ungroup() %>%
  lmerTest::lmer(scaled_entropy ~ TrueRule * score + (1 | PlayerID), data = .) %T>%
  output_lme_results("lme_model_true_theta_scaled_entropy_score_truerule", analysis_group = "model_true_theta")

df_pre_model_state_truetheta %>%
  summarize_performance(score, entropy, TrueRule) %>%
  select(PlayerID, score, TrueRule, mean_entropy) %>%
  posthoc_wilcox_test2(mean_entropy, TrueRule, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_true_theta_entropy_score_truerule", analysis_group = "model_true_theta")


df_model_switch_anova_truetheta = 
  df_pre_model_state_truetheta %>% 
  mutate(
    score = if_else(DisplayScore > 0, "positive", "negative"), 
    output = if_else(output == 1, "random", "skill")) %>% 
  group_by(PlayerID,input) %>% 
  mutate(prev_output = lag(output)) %>% 
  drop_na(prev_output) %>% 
  mutate(
         is_switch = (output != prev_output), 
         binary_conf = if_else(z_entropy > 0, "high", "low")) %>% 
  ungroup() %>% 
  drop_na(prev_output)

stopifnot(
  df_model_switch_anova_truetheta %>% dim() %>% `[`(1)== 
  (560 - 1) * length(df_rule_hit %>% pull(PlayerID) %>% unique()))

p_model_true_theta_prev_choice_switch = 
  df_model_switch_anova_truetheta %>% 
  group_by(PlayerID, input, score, prev_output) %>% 
  summarise(switch_prob = mean(is_switch)) %>% 
  ggplot(aes(x = prev_output, y = switch_prob, group = interaction(prev_output, score), fill = prev_output, alpha = score)) + 
  gg_filled_box_sina() + 
  theme_fig_anova + 
  ylab("switch probability") + xlab("previous choice") + 
  coord_cartesian(ylim = c(0, 1)) + 
  labs(alpha = "score", fill = "") + 
  theme(legend.position = "right") + 
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black"))), fill = FALSE)

df_model_switch_anova_truetheta %>% dim()
(560 - 1) * length(df_rule_hit %>% pull(PlayerID) %>% unique())


# p_model_true_theta_prev_choice_switch %>% 
#   save_svg_figure("p_model_true_theta_prev_choice_switch", analysis_group = fs::path("model_true_theta"), 
#                   scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")  

p_model_true_theta_conf_switch  = 
  df_model_switch_anova_truetheta %>% 
  group_by(PlayerID, input, score, binary_conf) %>% 
  summarise(switch_prob = mean(is_switch)) %>% 
  mutate(binary_conf = factor(binary_conf, levels = c("low", "high"))) %>% 
  ggplot(aes(x = binary_conf, y = switch_prob, group = interaction(binary_conf, score), fill = "black", alpha = score)) + 
  gg_filled_box_sina() + 
  theme_fig_anova + 
  ylab("switch probability") + xlab("confidence") +
   coord_cartesian(ylim = c(0, 1)) + 
  labs(alpha = "score") + 
  theme(legend.position = "right") + 
  guides(alpha=guide_legend(override.aes = list(color="black", fill = c("black"))), fill = FALSE)

p_model_true_theta_conf_switch %>%
  save_svg_figure("p_model_true_theta_conf_switch", analysis_group = fs::path("model_true_theta"),
                  scaling = fig_anova_scale,
                  width = fig_anova_width,
                  height = fig_anova_height, unit = "mm")

df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, prev_output) %>%
  select(PlayerID, score, prev_output, mean_is_switch) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis("anova_model_true_theta_prev_choice_switch", analysis_group = "model_true_theta")

df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  lme4::glmer(is_switch ~ score * prev_output + (1 | PlayerID), data = ., family = binomial) %>%
  output_lme_results("lme_model_true_theta_prev_choice_switch", analysis_group = "model_true_theta")

df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, prev_output) %>%
  select(PlayerID, score, mean_is_switch, prev_output) %>%
  posthoc_wilcox_test2(mean_is_switch, prev_output, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_true_theta_prev_choice_switch", analysis_group = "model_true_theta")


df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, binary_conf) %>%
  select(PlayerID, score, binary_conf, mean_is_switch) %>%
  mutate(PlayerID = as.character(PlayerID)) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %T>% print() %>%
  sink_analysis("anova_distance_model_conf_switch", analysis_group = "model_true_theta")

df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  lme4::glmer(is_switch ~ score * binary_conf + (1 | PlayerID), data = ., family = binomial) %>%
  output_lme_results("lme_model_true_theta_conf_switch", analysis_group = "model_true_theta")

df_model_switch_anova_truetheta %>%
  filter(input == "Distance") %>%
  summarize_performance(score, is_switch, binary_conf) %>%
  select(PlayerID, score, mean_is_switch, binary_conf) %>%
  posthoc_wilcox_test2(mean_is_switch, binary_conf, score) %>%
  output_posthoc_result("posthoc_wilcox_test_model_true_theta_conf_switch", analysis_group = "model_true_theta")


df_MLE_result_binary_true_theta = df_MLE_result %>% filter(name == "binary_sign_true_theta_Distance_random_estimation")
p_weights_true_theta= 
  df_MLE_result_binary_true_theta %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  filter(str_detect(params, "a_")) %>%
  # mutate(params = convert_label(params)) %>% 
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") + 
  geom_boxplot(
    width = boxwidth_weights_biases,
    outlier.shape = NA) + 
  geom_sina_fitted(alpha = 0.2, color = "black", show.legend = FALSE, 
                   maxwidth = (boxwidth_weights_biases / 0.75) * 0.5, scale = "width", 
                   dodge_width= (boxwidth_weights_biases / 0.75) *  0.8)  + 
  geom_signif(
    comparisons = list(c("a_B", "a_G")),
    test = "wilcox.test",
    test.args = list(paired = TRUE, exact = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.05,
    vjust = 0.5
  )  +
  ylim(c(-12, 25)) + 
  theme_fig_anova + 
  xlab("") + 
  ylab("parameter estimate")

p_biases_true_theta =
  df_MLE_result_binary_true_theta %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  filter(str_detect(params, "b_")) %>% 
  # mutate(params = convert_label(params)) %>% 
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") + 
  geom_boxplot(
    width = boxwidth_weights_biases,
    outlier.shape = NA) + 
  geom_sina_fitted(alpha = 0.2, color = "black", show.legend = FALSE, 
                   maxwidth = (boxwidth_weights_biases / 0.75) * 0.5, scale = "width", 
                   dodge_width= (boxwidth_weights_biases / 0.75) *  0.8)  + 
  geom_signif(
    comparisons = list(c("b_B", "b_G")),
    test = "wilcox.test",
    test.args = list(paired = TRUE, exact = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.05,
    vjust = 0.5
  )  +
  theme_fig_anova + 
  # ylim(c(-7, 16)) + 
  xlab("") + 
  ylab("parameter estimate")

# plot forgetting factor ----
df_wilcox_param_gamma_true_theta = df_MLE_result_binary %>% 
  inner_join(df_rule_hit %>% 
               group_by(PlayerID) %>% 
               summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>% 
  filter(params =="gamma") %>% rstatix::wilcox_test(value ~ 1, mu = 0, exact = TRUE)

p_val_wilcox_param_gamma_true_theta= df_wilcox_param_gamma_true_theta %>% pull(p)

p_forgetting_factor_true_theta = df_MLE_result_binary_true_theta %>% 
  inner_join(df_rule_hit %>% 
               group_by(PlayerID) %>% 
               summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>% 
  filter(params =="gamma") %>% 
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") + 
  geom_boxplot(
    width = boxwidth_gamma_thr,
    outlier.shape = NA) + 
  geom_sina_fitted(alpha = 0.2, color = "black", show.legend = FALSE, 
                   maxwidth = (boxwidth_gamma_thr / 0.75) * 0.5, scale = "width", 
                   dodge_width= (boxwidth_gamma_thr / 0.75) *  0.8)  + 
  annotate(geom = "text", x = "gamma", y = 1.05, label = p_val_wilcox_param_gamma_true_theta %>% p_val2star(), size = 6) + 
  theme_fig_anova + 
  xlab("") + 
  ylab("parameter estimate")

# do the same test done in the figure and output the result
df_MLE_result_binary_true_theta %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  filter(params == "gamma") %>% 
  rstatix::wilcox_test(value ~ 1, mu = 0, data = ., exact = TRUE) %>% 
  output_posthoc_result("wilcox_test_gamma_true_theta", analysis_group = "model_true_theta")

# do the same test done in the figure and output the result
df_MLE_result_binary_true_theta %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  filter(str_detect(params, "a_")) %>%
  rstatix::wilcox_test(value ~ params, data = ., paired = TRUE, exact = TRUE) %>%
  output_posthoc_result("wilcox_test_weights_true_theta", analysis_group = "model_true_theta")

# do the same test done in the figure and output the result
df_MLE_result_binary_true_theta %>% 
  filter(params != "number_of_params" & params != "AIC") %>% 
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% 
  filter(str_detect(params, "b_")) %>%
  rstatix::wilcox_test(value ~ params, data = ., paired = TRUE, exact = TRUE) %>%
  output_posthoc_result("wilcox_test_biases_true_theta", analysis_group = "model_true_theta")



p_param_all_true_theta = ((p_weights_true_theta | p_biases_true_theta | p_forgetting_factor_true_theta) + plot_layout(width = c(2, 2, 1)))
p_param_all_true_theta %>% save_svg_figure("p_param_all_true_theta", analysis_group = "model_true_theta",
                                           scaling = fig_anova_scale,
                                           width = fig_AIC_diff_width,
                                           height = fig_AIC_height,
                                           unit = "mm")

p_weights_true_theta %>% save_svg_figure("p_weights_true_theta", analysis_group = "model_true_theta",
                                         scaling = fig_anova_scale,
                                         width = fig_2box_width,
                                         height = fig_2box_height,
                                         unit = "mm")

p_biases_true_theta %>% save_svg_figure("p_biases_true_theta", analysis_group = "model_true_theta",
                                        scaling = fig_anova_scale,
                                        width = fig_2box_width,
                                        height = fig_2box_height,
                                        unit = "mm")

p_forgetting_factor_true_theta %>% save_svg_figure("p_forgetting_factor_true_theta", analysis_group = "model_true_theta",
                                                   scaling = fig_anova_scale,
                                                   width = fig_1box_width,
                                                   height = fig_1box_height,
                                                   unit = "mm")

# df_rule_hit %>% 
#   mutate(DisplayScore = if_else(DisplayScore ==1, "positive", "negative")) %>% 
#   mutate(PlayerID = as.factor(PlayerID)) %>% 
#   mutate(Correct = as.numeric(Correct)) %>% 
#   summarize_performance(DisplayScore, zConfidence, TrueRule) %>% select(PlayerID, DisplayScore, TrueRule, mean_zConfidence) %>% 
#   filter(TrueRule == "random") %>% 
#   pivot_wider(names_from = DisplayScore, values_from = mean_zConfidence) %>% 
#   mutate(conf_diff = positive - negative) %>% 
#   inner_join(
#     df_MLE_result %>% filter(grepl("binary_sign_Distance_random", name)) %>%
#       filter(params != "number_of_params" & params != "AIC") %>%
#       mutate(PlayerID = as.factor(PlayerID)) %>% 
#       inner_join(df_rule_hit %>% 
#                    group_by(PlayerID) %>% 
#                    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>% 
#       mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
#       mutate(params = if_else(params == "threshold", "threshold_ratio", params)) %>% 
#       pivot_wider(names_from = params, values_from = value) %>% 
#       select(PlayerID, threshold_ratio)
#   ) %>% 
#   lm_robust(threshold_ratio ~ conf_diff, data = .) %>% 
#   summary()
  # ggplot(aes(x = conf_diff, y =threshold_ratio)) + geom_point() + 
  # stat_smooth(method = lm_robust, geom = "line")



