library(R6)
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
library(ggcorrplot)

# utility definition ----

fig_2box_width_param <- fig_2box_width * 0.6
fig_1box_width_param <- fig_1box_width * 0.6
fig_AIC_width <- 460
fig_AIC_diff_width <- 400
fig_AIC_height <- 301

convert_label <- function(label) {
  label <- str_replace_all(label, c("(?<=^|_)a(?=^|_)" = "alpha", "(?<=^|_)b(?=^|_)" = "beta", "(?<=^|_)gamma(?=^|_)" = "gamma"))
  label <- str_replace(label, "_G$", "\\[~pos\\]")
  label <- str_replace(label, "_B$", "\\[~neg\\]")

  # expressionに変換するための文字列を作成
  # parsed_label <- sapply(label, function(.x) parse(text = .x))
  parsed_label <- label
  print(parsed_label)
  return(parsed_label)
}

sanitize_model_name <- function(df) {
  df %>%
    mutate(
      name =
        str_replace_all(
          name,
          "binary_sign_Distance_random", "full"
        ) %>%
          str_replace_all(
            "binary_sign_no_gamma_Distance_random", "no accumulation"
          ) %>%
          str_replace_all(
            "binary_sign_common_beta_Distance_random", "common bias"
          ) %>%
          str_replace_all(
            "binary_sign_common_alpha_Distance_random", "common weight"
          ) %>%
          str_replace_all(
            "binary_sign_common_alpha_common_beta_Distance_random", "symmetric"
          ) %>%
          str_replace_all(
            "binary_sign_true_theta_Distance_random", "fixed threshold"
          )
    ) %>%
    mutate(name = gsub("(_estimation|model_|obj_)", "", name)) %>%
    mutate(name = gsub(" $", "", name))
}

# load fitted data ====
file_lists <- list.files(here::here("model_based_analysis", "R_result/"), pattern = "binary_sign_.*Distance.*estimation.rds", full.names = TRUE)
df_MLE_result <- foreach(file = file_lists, .combine = rbind) %do%
  {
    print(file)
    data_name <- sub("\\.[^.]*", "", file) %>%
      sub("[^.]*//", "", .) %>%
      sub("_model_obj", "", .)
    df_MLE_result <- import(file)
    df_MLE_result %>%
      pivot_longer(cols = c(-PlayerID, -log_likelihood), names_to = "params", values_to = "value") %>%
      mutate(name = data_name)
  } %>% mutate(PlayerID = as.factor(PlayerID))

# calculate information criteria (Supplementary Table 2) ====
df_MLE_result %>%
  sanitize_model_name() %>%
  pivot_wider(names_from = params, values_from = value) %>%
  filter(!str_detect(name, "twice")) %>% # filter unrelated data
  mutate(
    name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)
  ) %>%
  group_by(name) %>%
  mutate(
    BIC = -2 * log_likelihood + log(560) * number_of_params ### magic number to represent the numnber of trials
  ) %>%
  select(PlayerID, name, AIC, BIC, log_likelihood) %>%
  group_by(name) %>%
  summarise(
    mean_AIC = mean(AIC),
    median_AIC = median(AIC),
    sd_AIC = sd(AIC),
    min_AIC = min(AIC),
    max_AIC = max(AIC),
    mean_BIC = mean(BIC),
    median_BIC = median(BIC),
    sd_BIC = sd(BIC),
    min_BIC = min(BIC),
    max_BIC = max(BIC),
    mean_log_likelihood = mean(log_likelihood),
    sd_log_likelihood = sd(log_likelihood),
    median_log_likelihood = median(log_likelihood),
    min_log_likelihood = min(log_likelihood),
    max_log_likelihood = max(log_likelihood)
  ) %>%
  export(here::here("results", "IC_summary.csv"))

df_AIC <- df_MLE_result %>%
  sanitize_model_name() %>%
  pivot_wider(names_from = params, values_from = value) %>%
  # filter(number_of_params == 5 & !str_detect(name, pattern="accum")) %>%
  filter(!str_detect(name, "twice")) %>% # filter unrelated data
  mutate(
    name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)
  ) %>%
  select(PlayerID, name, AIC)
df_AIC_summary <-
  df_AIC %>%
  group_by(name) %>%
  summarise(mean = mean(AIC), sd = sd(AIC), min = min(AIC), max = max(AIC), median = median(AIC))
df_AIC_summary %>%
  export(here::here("results", "AIC_summary.csv"))

# plot AIC (Supplementary Figure 5) ====
(df_MLE_result %>%
  sanitize_model_name() %>%
  filter(!str_detect(name, "twice")) %>% # filter unrelated data
  pivot_wider(names_from = params, values_from = value) %>%
  ggplot(aes(x = AIC, y = name %>% reorder(AIC, FUN = mean), group = name)) +
  # geom_sina() +
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
    aes(label = sprintf("%.1f", after_stat(x))),
    hjust = 1.2,
    size = 5,
    color = "white"
  ) +
  geom_signif(
    comparisons = list(
      # comparison between full and others
      c("full", "common bias"),
      c("full", "common weight"),
      c("full", "symmetric"),
      c("full", "fixed threshold"),
      c("full", "no accumulation")
    ),
    test = "wilcox.test",
    test.args = list(paired = TRUE, exact = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    # margin_top = 0.15,
    vjust = 0.5,
    step_increase = 0.1,
    y_position = 520
  ) +
  theme_fig_base +
  theme(aspect.ratio = 1 / 1.414) +
  scale_y_discrete(
    labels = function(x) {
      x <- paste(x, "model")
      return(x)
    }
  ) +
  ylab("") +
  xlab("AIC")) %>%
  save_svg_figure(
    "p_AIC_horizontal",
    analysis_group = "model_comparison",
    width = fig_AIC_width * 2,
    height = fig_AIC_height,
    scaling = fig_anova_scale,
    unit = "mm"
  )

# post-hoc test for AIC comparison
df_AIC %>%
  rstatix::wilcox_test(
    AIC ~ name,
    exact = TRUE,
    paired = TRUE,
    ref.group = "full",
    p.adjust.method = "fdr"
  ) %>%
  output_posthoc_result(
    "posthoc_test_AIC",
    analysis_group = "model_comparison"
  )

# pick up the full model used in main analyses ----
df_MLE_result_binary <- df_MLE_result %>% filter(name == "binary_sign_Distance_random_estimation")
stopifnot(df_MLE_result_binary %>% dim() == c(length(df_rule_hit %>% pull(PlayerID) %>% unique()) * 8, 5))

# save table the result
df_MLE_result_binary %>%
  pivot_wider(names_from = params, values_from = value) %>%
  rio::export(
    fs::path(
      "results",
      "binary_sign_Distance_random_estimation_parameters",
      ext = "csv"
    )
  )

boxwidth_weights_biases <- 0.75
boxwidth_gamma_thr <- 0.75

# plot weights and biases (Figure 2 C)----
p_weights_biases <- df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(params != "gamma" & params != "threshold") %>%
  # mutate(params = convert_label(params)) %>%
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") +
  geom_boxplot(
    width = boxwidth_weights_biases,
    outlier.shape = NA
  ) +
  geom_sina_fitted(
    alpha = 0.2, color = "black", show.legend = FALSE,
    maxwidth = (boxwidth_weights_biases / 0.75) * 0.5, scale = "width",
    dodge_width = (boxwidth_weights_biases / 0.75) * 0.8
  ) +
  geom_signif(
    comparisons = list(c("a_B", "a_G"), c("b_B", "b_G")),
    test = "wilcox.test",
    test.args = list(paired = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.15
  ) +
  theme_fig_anova +
  xlab("")

p_weights <-
  df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(str_detect(params, "a_")) %>%
  # mutate(params = convert_label(params)) %>%
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") +
  geom_boxplot(
    width = boxwidth_weights_biases,
    outlier.shape = NA
  ) +
  geom_sina_fitted(
    alpha = 0.2, color = "black", show.legend = FALSE,
    maxwidth = (boxwidth_weights_biases / 0.75) * 0.5, scale = "width",
    dodge_width = (boxwidth_weights_biases / 0.75) * 0.8
  ) +
  geom_signif(
    comparisons = list(c("a_B", "a_G")),
    test = "wilcox.test",
    test.args = list(paired = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.05
  ) +
  ylim(c(-12, 25)) +
  theme_fig_anova +
  xlab("") +
  ylab("parameter estimate")

p_biases <-
  df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(str_detect(params, "b_")) %>%
  # mutate(params = convert_label(params)) %>%
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") +
  geom_boxplot(
    width = boxwidth_weights_biases,
    outlier.shape = NA
  ) +
  geom_sina_fitted(
    alpha = 0.2, color = "black", show.legend = FALSE,
    maxwidth = (boxwidth_weights_biases / 0.75) * 0.5, scale = "width",
    dodge_width = (boxwidth_weights_biases / 0.75) * 0.8
  ) +
  geom_signif(
    comparisons = list(c("b_B", "b_G")),
    test = "wilcox.test",
    test.args = list(paired = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.05
  ) +
  theme_fig_anova +
  # ylim(c(-7, 16)) +
  xlab("") +
  ylab("parameter estimate")

# wilcoxson signed rank test for weights ----
df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>%
    gsub("(lpha|eta|amma)", "", .)) %>%
  filter(str_detect(params, "a_")) %>%
  mutate(params = as.factor(params)) %>%
  coin::wilcoxsign_test(
    value ~ params | PlayerID,
    distribution = "exact", zero.method = "Wilcoxon", data = .
  ) %>%
  print() %>%
  sink_analysis("wilcox_test_model_weights",
    analysis_group = "parameter_analysis"
  )

# wilcoxson signed rank test for biases ----
df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>%
    gsub("(lpha|eta|amma)", "", .)) %>%
  filter(str_detect(params, "b_")) %>%
  mutate(params = as.factor(params)) %>%
  coin::wilcoxsign_test(
    value ~ params | PlayerID,
    distribution = "exact", zero.method = "Wilcoxon", data = .
  ) %>%
  print() %>%
  sink_analysis("wilcox_test_model_biases",
    analysis_group = "parameter_analysis"
  )

df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(str_detect(params, "a_")) %>%
  mutate(params = as.factor(params)) %>%
  rstatix::wilcox_test(value ~ params, exact = TRUE, paired = TRUE)

df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(str_detect(params, "b_")) %>%
  mutate(params = as.factor(params)) %>%
  rstatix::wilcox_test(value ~ params, exact = TRUE, paired = TRUE)

df_wilcox_param_gamma <- df_MLE_result_binary %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
  filter(params == "gamma") %>%
  rstatix::wilcox_test(value ~ 1, mu = 0)

# wilcoxson signed rank test for forgetting factor against zero ----
df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(params == "gamma") %>%
  mutate(
    params = as.factor(params),
    zero = 0
  ) %>%
  coin::wilcoxsign_test(
    value ~ zero,
    distribution = "exact", zero.method = "Wilcoxon", data = .,
    alternative = "greater"
  ) %>%
  print() %>%
  sink_analysis("wilcox_test_model_gamma_against_zero",
    analysis_group = "parameter_analysis"
  )


df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(params == "gamma") %>%
  mutate(params = as.factor(params)) %>%
  rstatix::wilcox_test(value ~ 1, mu = 0, exact = TRUE, alternative = "greater")

# wilcoxson signed rank test for forgetting factor against one ----
df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(params == "gamma") %>%
  mutate(
    params = as.factor(params),
    one = 1
  ) %>%
  coin::wilcoxsign_test(
    value ~ one,
    distribution = "exact", zero.method = "Wilcoxon", data = .,
    alternative = "less"
  ) %>%
  print() %>%
  sink_analysis("wilcox_test_model_gamma_against_one",
    analysis_group = "parameter_analysis"
  )


df_MLE_result_binary %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  filter(params == "gamma") %>%
  mutate(params = as.factor(params)) %>%
  rstatix::wilcox_test(value ~ 1, mu = 1, exact = TRUE, alternative = "less")

p_val_wilcox_param_gamma <- df_wilcox_param_gamma %>% pull(p)

# plot forgetting factor (Figure 2 C) ----
p_forgetting_factor <- df_MLE_result_binary %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
  filter(params == "gamma") %>%
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") +
  geom_boxplot(
    width = boxwidth_gamma_thr,
    outlier.shape = NA
  ) +
  geom_sina_fitted(
    alpha = 0.2, color = "black", show.legend = FALSE,
    maxwidth = (boxwidth_gamma_thr / 0.75) * 0.5, scale = "width",
    dodge_width = (boxwidth_gamma_thr / 0.75) * 0.8
  ) +
  annotate(geom = "text", x = "gamma", y = 1.05, label = p_val_wilcox_param_gamma %>% p_val2star(), size = 6) +
  theme_fig_anova +
  geom_signif(
    comparisons = list(c("gamma")),
    test = "wilcox.test",
    test.args = list(mu = 0)
  ) +
  xlab("") +
  ylab("parameter estimate")

# test threshold ratio against 1 ----
df_MLE_result_binary %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
  filter(params == "threshold") %>%
  mutate(
    params = as.factor(params),
    one = 1
  ) %>%
  coin::wilcoxsign_test(
    value ~ one,
    distribution = "exact", zero.method = "Wilcoxon", data = .
  ) %>%
  print() %>%
  sink_analysis("wilcox_test_model_threshold_against_one",
    analysis_group = "parameter_analysis"
  )

# plot threshold ratio (Figure 2 C)----
df_wilcox_param_threshold <- df_MLE_result_binary %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
  filter(params == "threshold") %>%
  rstatix::wilcox_test(value ~ 1, mu = 1, exact = TRUE)
df_wilcox_param_threshold %>%
  rio::export(here::here("results", "wilcox_test_against_chance_thrshold.csv"))
p_val_wilcox_param_threshold <- df_wilcox_param_threshold %>% pull(p)

p_threshold <- df_MLE_result_binary %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  filter(params != "number_of_params" & params != "AIC") %>%
  mutate(name = gsub("(_estimation|model_|obj_)", "", name) %>% gsub("(lpha|eta|amma)", "", .)) %>% # rename the model names
  mutate(value = if_else(params == "threshold", value / true_threshold, value)) %>%
  filter(params == "threshold") %>%
  ggplot(aes(x = params, y = value, group = params, ), fill = "black", alpha = "black", color = "black") +
  geom_boxplot(
    width = boxwidth_gamma_thr,
    outlier.shape = NA
  ) +
  geom_sina_fitted(
    alpha = 1,
    color = "white",
    fill = "black",
    show.legend = FALSE,
    maxwidth = (boxwidth_gamma_thr / 0.75) * 0.5,
    scale = "width",
    shape = 21,
    dodge_width = (boxwidth_gamma_thr / 0.75) * 0.8
  ) +
  geom_hline(yintercept = 1, lty = "dashed") +
  annotate(geom = "text", x = "threshold", y = 5.5, label = p_val_wilcox_param_threshold %>% p_val2star(), size = 6) +
  scale_x_discrete(labels = function(x) {
    # "threshold" -> "θ"
    ifelse(x == "threshold", expression(theta), x)
  }) +
  theme_fig_anova +
  xlab("") +
  ylab("est. threshold / true threshold")

p_param_all <- ((p_weights_biases | p_forgetting_factor | p_threshold) + plot_layout(width = c(4, 1, 1)))
p_param_all %>% save_svg_figure("p_param_all")

p_weights %>% save_svg_figure("p_weights", width = fig_2box_width_param, height = fig_2box_height, scaling = fig_anova_scale, unit = "mm")
p_biases %>% save_svg_figure("p_biases", width = fig_2box_width_param, height = fig_2box_height, scaling = fig_anova_scale, unit = "mm")
p_forgetting_factor %>% save_svg_figure("p_gamma", width = fig_1box_width_param, height = fig_1box_height, scaling = fig_anova_scale, unit = "mm")
p_threshold %>% save_svg_figure("p_threshold", width = fig_1box_width_param, height = fig_1box_height, scaling = fig_anova_scale, unit = "mm")


# parameter correlation (Figure 2 D)----
corr_method <- "spearman"
# corr_method = "pearson"
p_param_corr <-
  df_MLE_result_binary %>%
  select(-log_likelihood, -name) %>%
  pivot_wider(names_from = params, values_from = value) %>%
  select(-PlayerID, -number_of_params, -AIC) %>%
  cor(method = corr_method) %>%
  ggcorrplot(
    lab = TRUE, type = "lower",
    legend.title = corr_method,
    ggtheme = ggplot2::theme_minimal() + theme(axis.title = element_text(), legend.key.height = unit(dev.size()[2] / 8, "inches"))
  )

p_param_corr %>%
  save_svg_figure("p_param_corr",
    width = fig_anova_width,
    height = fig_anova_height, units = "mm",
    scale = 0.5
  )

# correlation between weights and threshold ratio (Supplementary Figure 16)----
df_MLE_result_binary %>%
  select(-log_likelihood, -name) %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  select(-PlayerID, -number_of_params, -AIC) %>%
  mutate(threshold_ratio = threshold / true_threshold) %>%
  lm_robust(formula = a_G - a_B ~ threshold_ratio, data = .) %>%
  summary() %>%
  print() %>%
  sink_analysis("weights_diff_threshold_ratio_lm_robust",
    analysis_group = "parameter_analysis"
  )

(df_MLE_result_binary %>%
  select(-log_likelihood, -name) %>%
  pivot_wider(names_from = params, values_from = value) %>%
  inner_join(df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(true_threshold = unique(true_threshold)), by = "PlayerID") %>%
  select(-PlayerID, -number_of_params, -AIC) %>%
  mutate(threshold_ratio = threshold / true_threshold) %>%
  ggplot(aes(x = threshold_ratio, y = a_G - a_B)) +
  geom_point() +
  stat_smooth(method = lm_robust) +
  xlab("threshold ratio") +
  ylab("weights difference") +
  coord_cartesian(ylim = c(-15, 20)) +
  theme_fig_base) %>%
  save_svg_figure(
    "weights_diff_threshold_ratio_fig",
    analysis_group = "parameter_analysis",
    width = fig_anova_width,
    height = fig_anova_height,
    scaling = fig_anova_scale,
    unit = "mm"
  )
