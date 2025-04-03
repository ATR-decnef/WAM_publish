## load packages ------------------------------------------------
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggforce)
library(ggExtra)
library(magrittr)
library(patchwork)
library(slider)
library(gtsummary)
library(flextable)
library(lme4)
library(lmerTest)
library(rio)
library(broom)
library(broom.mixed)
library(estimatr)
library(rjags)


## theme setup --------------------------------------------------

cbPalette <- c(
  edge = "#E69F00", center = "#56B4E9",
  `random to skill` = "#009E73", `skill to random` = "#CC79A7",
  negative = "#0072B2", positive = "#D55E00",
  negative = "#0072B2", positive = "#D55E00",
  skill = "#009E73", random = "#CC79A7",
  Skill = "#009E73", Random = "#CC79A7",
  positive.skill = "#E69F00", positive.random = "#D55E00",
  negative.skill = "#56B4E9", negative.random = "#0072B2",
  positive.skill = "#E69F00", positive.random = "#D55E00",
  negative.skill = "#56B4E9", negative.random = "#0072B2",
  "#CC79A7", "#999999"
)

standardize_factor <- function(df) {
  df %>% str_replace_all(DisplayScore, "[G|g]ood")
}

fig_anova_scale <- 3.1
fig_anova_width <- 383
fig_anova_height <- 306
fig_2box_width <- 250
fig_2box_height <- 301
fig_1box_width <- 230
fig_1box_height <- 301
fig_timeseries_width <- 383 + 92
fig_timeseries_height <- 239
fig_example_width <- 383
fig_example_height <- 100

box_width_2box <- 0.4

theme_fig <- list(
  labs_pubr(),
  theme_pubr(),
  theme(
    text = element_text(size = 52 / fig_anova_scale),
    legend.position = "right"
  ),
  scale_color_manual(values = cbPalette),
  scale_fill_manual(values = cbPalette),
  scale_alpha_manual(values = c(
    Good = 1, Bad = 0, good = 1, bad = 0,
    positive = 1, negative = 0, `1` = 0.2,
    `2` = 0.5, `3` = 0.8, `4` = 1
  ))
)
theme_fig1 <- theme_fig
theme_fig_boxplot <- list(theme_fig, theme(axis.line.x = element_blank()))
theme_fig_example <-
  list(
    theme_fig,
    theme(
      text = element_text(size = 42 / fig_anova_scale),
      legend.position = "right"
    )
  )


## state_name_definition-------------------------
true_rule_name <- "task state"
est_rule_name <- "choice"



## ----load_data----------------------------------------------------------------
df_rule_hit_origin <- read_csv(here::here("data/df_rule_hit_untilMar_switch.csv")) %>%
  mutate(PlayerID = as.factor(PlayerID)) %>%
  mutate(
    EstRule = str_replace_all(EstRule, pattern = c("luck" = "random")),
    TrueRule = str_replace_all(TrueRule, pattern = c("luck" = "random"))
  ) %>%
  mutate(
    TrueScore = if_else(TrueScore < 0, 0, TrueScore),
    DisplayScore = if_else(DisplayScore < 0, 0, DisplayScore)
  )
df_score_hit <- read_csv(here::here("data/df_score_hit_untilMar.csv")) %>% mutate(PlayerID = as.factor(PlayerID))
df_median <- df_score_hit %>% group_by(PlayerID)


## ----preprocess---------------------------------------------------------------
source(here::here("behaviour", "analysis_scripts", "CheckCriteria.R"))
source(here::here("behaviour", "analysis_scripts", "preprocess.R"))
source(here::here("behaviour", "analysis_scripts", "summarize_utility.R"))
source(here::here("behaviour", "analysis_scripts", "anova_kun.R"))

p_criteria <- CheckCriteria(df_rule_hit_origin)

list_of_included_list <- listIncludedParticipants(df_rule_hit_origin)
pass_choice_acc_list <- list_of_included_list[[1]]
pass_choice_ratio_list <- list_of_included_list[[2]]
pass_choice_list <- intersect(pass_choice_acc_list, pass_choice_ratio_list)
pass_conf_list <- list_of_included_list[[3]]

included_list <- intersect(pass_choice_list, pass_conf_list)


df_rule_hit <- excludeDfRuleHit(df_rule_hit_origin) %>% # preprocessDfRuleHit() %>%
  # select(-V1, -`Unnamed: 0`) %>%
  mutate(EstRuleResponseTime = dmy_hms(EstRuleResponseTime, tz = "Asia/Tokyo"), EstRuleMessagePopupTime = dmy_hms(EstRuleMessagePopupTime, tz = "Asia/Tokyo")) %>%
  mutate(ruleEstimationRT = EstRuleMessagePopupTime %--% EstRuleResponseTime %>% time_length()) %>%
  mutate(
    PlayerID = as.factor(PlayerID),
    ruleEstimationRT = ruleEstimationRT
  ) %>%
  add_true_threshold() %>%
  mutate(threshold = true_threshold) %>%
  mutate(normalizedDistance = Distance / true_threshold)
playerid_list <- df_rule_hit %>%
  pull(PlayerID) %>%
  as.factor() %>%
  unique()

df_rule_hit_performance <-
  df_rule_hit %>%
  group_by(PlayerID) %>%
  summarise(
    performance = mean(Correct),
    dis_var = var(Distance),
    true_threshold = unique(true_threshold)
  )


## ----demographic--------------------------------------------------------------
# age of included participants
rio::import(here::here("data/players_rewards.csv")) %>%
  filter(ID %in% (df_rule_hit %>% pull(PlayerID) %>% unique())) %>%
  drop_na(Rewards) %>%
  summarise(
    mean_age = mean(Age),
    max_age = max(Age),
    min_age = min(Age),
    sd = sd(Age),
    sem = sd(Age) / sqrt(n()),
    n = n()
  )

# age of all participants
rio::import(here::here("data/players_rewards.csv")) %>%
  drop_na(Rewards) %>%
  summarise(
    mean_age = mean(Age),
    max_age = max(Age),
    min_age = min(Age),
    sd = sd(Age),
    sem = sd(Age) / sqrt(n()),
    n = n()
  )

# reward of included participants
rio::import(here::here("data/players_rewards.csv")) %>%
  filter(ID %in% (df_rule_hit %>% pull(PlayerID))) %>%
  summarise(
    mean = mean(Rewards),
    max = max(Rewards),
    min = min(Rewards),
    sd = sd(Rewards),
    sem = sd / sqrt(n()),
    n = n()
  )

# reward of all participants
rio::import(here::here("data/players_rewards.csv")) %>%
  drop_na(Rewards) %>%
  summarise(
    mean = mean(Rewards),
    max = max(Rewards),
    min = min(Rewards),
    sd = sd(Rewards),
    sem = sd / sqrt(n()),
    n = n()
  )



## ----histogram----------------------------------------------------------------
# histogram of the number of trials between switches
df_rle <-
  df_rule_hit %>%
  group_by(PlayerID) %>%
  nest() %>%
  mutate(
    rle = map(
      data,
      ~ .x %>%
        pull(TrueRule) %>%
        rle()
    ),
    lengths = map(rle, ~ `$`(.x, lengths)),
    states = map(rle, ~ `$`(.x, values)),
    participant = map_id(PlayerID, playerid_list)
  ) %>%
  unnest(cols = c(lengths, states))


df_rle %>%
  select(-data, -rle) %>%
  mutate(
    length_of_block = lengths,
    state_of_block = states
  ) %>%
  select(PlayerID, participant, state_of_block, length_of_block) %>%
  output_csv(
    "block_length",
    analysis_group = "block_length"
  )

df_rle %>%
  select(-data, -rle) %>%
  group_by(PlayerID, lengths) %>%
  summarise(number_of_miniblocks = n()) %>%
  select(PlayerID, lengths, number_of_miniblocks) %>%
  output_csv(
    "block_length_hist",
    analysis_group = "block_length"
  )

df_rle %>%
  group_by(PlayerID) %>%
  summarise(
    mean_within = mean(lengths),
    sd_within = sd(lengths)
  ) %>%
  ungroup() %>%
  summarise(
    mean_of_mean = mean(mean_within),
    sd_of_mean = sd(mean_within),
    mean_of_sd = mean(sd_within),
    sd_of_sd = sd(sd_within)
  ) %>%
  output_csv(
    "mean_sd_of_length_of_block",
    analysis_group = "block_length"
  )

p_rle <- df_rle %>%
  ggplot(aes(x = lengths, group = participant, color = participant)) +
  geom_histogram(fill = NA) +
  theme_fig +
  scale_color_discrete() +
  theme(
    legend.position = "right",
    legend.key.size = unit(1, "cm"), # change legend key size
    legend.key.height = unit(1 / 20, "in"), # change legend key height
    legend.key.width = unit(1 / 10, "in"), # change legend key width
    legend.title = element_text(size = 14), # change legend title font size
    legend.text = element_text(size = 10)
  )

# supplementary figure 1 ----
p_rle %>% save_svg_figure("p_rle",
  analysis_group = "block_length",
  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
)




## general_accuracy---------------------------------------------
general_acc_included <- df_rule_hit_origin %>%
  group_by(PlayerID) %>%
  summarise(accuracy = mean(Correct)) %>%
  mutate(is_included = PlayerID %in% included_list) %>%
  mutate(is_included = if_else(is_included, "included", "excluded")) %>%
  pivot_wider(names_from = is_included, values_from = accuracy) %>%
  summarise(accuracy = mean(included, na.rm = TRUE)) %>%
  pull(accuracy)
num_of_all_participants <- df_rule_hit_origin %>%
  pull(PlayerID) %>%
  unique() %>%
  length()

df_general_count <-
  df_rule_hit_origin %>%
  group_by(PlayerID) %>%
  summarise(accuracy = mean(Correct)) %>%
  mutate(is_included = PlayerID %in% included_list) %>%
  mutate(is_included = if_else(is_included, "included", "excluded")) %>%
  count(is_included) %>%
  pivot_wider(names_from = is_included, values_from = n)
number_of_included <- df_general_count %>% pull(included)
number_of_excluded <- df_general_count %>% pull(excluded)
number_of_all <- number_of_included + number_of_excluded

p_general_acc <- df_rule_hit_origin %>%
  group_by(PlayerID) %>%
  summarise(accuracy = mean(Correct)) %>%
  mutate(is_included = PlayerID %in% included_list) %>%
  mutate(is_included = if_else(is_included, "included", "excluded")) %>%
  filter(is_included == "included") %>%
  ggplot(aes(x = "", y = accuracy)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, width = 0.4) +
  geom_sina_fitted(alpha = 1, size = 2.5, maxwidth = 0.4 / 0.75 * 0.5, shape = 21, color = "white", fill = "black") +
  theme_fig_boxplot +
  xlab("") +
  labs(shape = "") +
  ylab("p(correct state inference)") +
  coord_cartesian(ylim = c(0.48, 0.92))

# Figure 1C
p_general_acc %>%
  save_svg_figure("general accuracy plot", scaling = fig_anova_scale, width = fig_1box_width, height = fig_1box_height, unit = "mm")


## ----confusion_matrix (Table 1)---------------------------------------------------------
df_rule_hit %>%
  mutate(`task state` = TrueRule, `choice` = EstRule) %>%
  xtabs(~ `choice` + `task state`, data = .) / (560 * 51)

df_proportion_state_choice <- df_rule_hit %>%
  group_by(PlayerID) %>%
  mutate(num_of_trials = n()) %>%
  group_by(PlayerID, TrueRule, EstRule) %>%
  summarize(proportion = n() / unique(num_of_trials), .groups = "drop") %>%
  ungroup() %T>%
  output_csv(
    "proportion_state_choice_each_participant",
    analysis_group = "description"
  )

df_proportion_state_choice_conditional <- df_rule_hit %>%
  group_by(PlayerID, TrueRule) %>%
  mutate(num_of_trials = n()) %>%
  group_by(PlayerID, TrueRule, EstRule) %>%
  summarize(proportion = n() / unique(num_of_trials), .groups = "drop") %>%
  ungroup() %T>%
  output_csv(
    "proportion_state_choice_each_participant",
    analysis_group = "description"
  )

df_proportion_state_choice_conditional %>%
  group_by(TrueRule, EstRule) %>%
  summarise(mean_proportion = mean(proportion), sd = sd(proportion)) %>%
  output_csv(
    "proportion_state_choice_conditional",
    analysis_group = "description"
  )
# rio::export(
#   here::here(fs::path("Figure",
#                       "results",
#                       "description",
#                       "proportion_state_choice_each_participant", ext = "csv")))



df_proportion_state_choice %>%
  group_by(TrueRule, EstRule) %>%
  summarise(mean_proportion = mean(proportion), sd = sd(proportion)) %>%
  output_csv(
    "proportion_state_choice",
    analysis_group = "description"
  )

df_proportion_state_choice %>%
  mutate(task_choice = interaction(TrueRule, EstRule)) %>%
  select(-TrueRule, -EstRule) %>%
  pivot_wider(names_from = task_choice, values_from = proportion) %>%
  coin::wilcoxsign_test(
    skill.skill ~ random.random,
    data = .,
    distribution = "exact",
    zero.method = "Wilcoxon"
  ) %>%
  print() %>%
  sink_analysis(
    "wilcox_test_proportion_state_choice",
    analysis_group = "description"
  )

df_proportion_state_choice_conditional %>%
  mutate(task_choice = interaction(TrueRule, EstRule)) %>%
  select(-TrueRule, -EstRule) %>%
  pivot_wider(names_from = task_choice, values_from = proportion) %>%
  coin::wilcoxsign_test(
    skill.skill ~ random.random,
    data = .,
    distribution = "exact",
    zero.method = "Wilcoxon"
  ) %>%
  print() %T>%
  sink_analysis(
    "wilcox_test_proportion_state_choice_conditional",
    analysis_group = "description"
  )

## reaction_time------------------------------------------------
p_hit_location_scatter <- (df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  ggplot(aes(x = LocX, LocY, color = TrueRule)) +
  geom_point() +
  theme_fig +
  theme(aspect.ratio = 1) +
  xlab("X") +
  ylab("Y") +
  labs(color = true_rule_name)) %T>%
  save_svg_figure("hit location scatter plot", scaling = fig_anova_scale, unit = "mm")

p_estimation_RT <- (df_rule_hit %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect"),
    DisplayScore =
      case_when(
        DisplayScore > 0 ~ "positive",
        DisplayScore <= 0 ~ "negative",
        TRUE ~ as.character(DisplayScore)
      )
  ) %>%
  ggplot(aes(x = ruleEstimationRT, group = DisplayScore, color = DisplayScore), fill = NA) +
  geom_density() +
  theme_fig +
  xlab("rule estimation RT [s] (logscale)") +
  scale_x_log10() +
  scale_x_log10(breaks = c(0.3, 0.5, 1.0, 2, 5, 10.0)) +
  coord_cartesian(xlim = c(0.3, 10)) +
  labs(legend = "", linetype = "")) %T>%
  save_svg_figure("estimation RT plot",
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    scaling = fig_anova_scale,
    unit = "mm"
  ) # Supplementary Figure 3A

p_distance_true <- (df_rule_hit %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect"),
    DisplayScore = as.factor(numeric_score_to_strings(DisplayScore))
  ) %>%
  ggplot(aes(x = Distance, group = TrueRule, color = TrueRule), fill = NA) +
  geom_density() +
  theme_fig +
  scale_x_log10() +
  xlab("distance from the centre of the moles") +
  labs(color = true_rule_name)) %T>%
  save_svg_figure("distancescore true ruleplot",
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    scaling = fig_anova_scale, unit = "mm"
  )


p_hit_location_scatter_score <- (df_rule_hit %>% mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>% ggplot(aes(x = LocX, LocY, color = DisplayScore)) +
  geom_point() +
  theme_fig +
  theme_fig +
  theme(aspect.ratio = 1) +
  xlab("X") +
  ylab("Y") +
  labs(color = "score")) %T>% save_svg_figure("hit location scatter plot score", scaling = fig_anova_scale, unit = "mm")


p_distance_score <- (df_rule_hit %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect"),
    DisplayScore = as.factor(numeric_score_to_strings(DisplayScore))
  ) %>%
  ggplot(aes(x = Distance, group = DisplayScore, color = DisplayScore), fill = NA) +
  geom_density() +
  theme_fig +
  scale_x_log10() +
  xlab("distance from the centre of the moles") +
  labs(color = "score")) %T>%
  save_svg_figure("distancescore plot",
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    scaling = fig_anova_scale, unit = "mm"
  ) # Supplementary Figure 3B


## score_ratio--------------------------------------------------
p_score_ratio_truerule <- (df_rule_hit %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  ggplot(aes(
    x = TrueRule,
    y = positive / (positive + negative),
    group = TrueRule,
    color = TrueRule
  )) +
  geom_boxplot(show.legend = FALSE, width = box_width_2box) +
  geom_sina_fitted(
    alpha = 0.2,
    show.legend = FALSE,
    maxwidth = (box_width_2box / 0.75) * 0.5
  ) +
  geom_signif(
    comparisons = list(c("random", "skill")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    test.args = list(paired = TRUE, exact = TRUE),
    color = "black",
    textsize = 6,
    margin_top = 0.15
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    minor_breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0.05, 0.2))
  ) +
  coord_cartesian(ylim = c(0.17, 1)) +
  theme_fig_boxplot +
  guides(color = guide_legend(color = FALSE)) +
  xlab(true_rule_name) +
  ylab("ratio of positive scores")) %T>%
  save_svg_figure(paste0("ratio of scores for ", true_rule_name),
    analysis_group = "ratio_score",
    scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm"
  ) # Supplementary Figure 2

df_rule_hit %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  ungroup() %>%
  mutate(ratio = positive / (positive + negative)) %>%
  select(PlayerID, TrueRule, ratio) %>%
  posthoc_wilcox_test(ratio ~ TrueRule) %>%
  output_posthoc_result("posthoc_wilcox_test_ratio_score_true_rule", analysis_group = "ratio_score")

# test against chance level
df_rule_hit %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  mutate(ratio = positive / (positive + negative)) %>%
  select(PlayerID, TrueRule, ratio) %>%
  posthoc_wilcox_against_chance(ratio ~ TrueRule, alternative = "two.sided") %>%
  output_posthoc_result(
    "posthoc_wilcox_test_ratio_score_true_rule_against_chance",
    analysis_group = "ratio_score"
  )


p_score_ratio_estimation <- (df_rule_hit %>%
  group_by(PlayerID, EstRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  ggplot(aes(x = EstRule, y = positive / (positive + negative), group = EstRule, color = EstRule)) +
  geom_boxplot(show.legend = FALSE, width = box_width_2box) +
  geom_sina_fitted(alpha = 0.2, show.legend = FALSE, maxwidth = (box_width_2box / 0.75) * 0.5) +
  geom_signif(comparisons = list(c("random", "skill")), test = "wilcox.test", map_signif_level = TRUE, test.args = list(paired = TRUE, exact = TRUE), color = "black", textsize = 6, margin_top = 0.15, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), minor_breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0.05, 0.2))) +
  coord_cartesian(ylim = c(0.17, 1)) +
  theme_fig_boxplot +
  guides(color = guide_legend(color = FALSE)) +
  xlab(est_rule_name) +
  ylab("ratio of positive scores")) %T>%
  save_svg_figure(paste0("ratio of scores for ", est_rule_name),
    analysis_group = "ratio_score",
    scaling = fig_anova_scale, width =
      fig_2box_width, height =
      fig_2box_height, unit = "mm"
  )

df_rule_hit %>%
  group_by(PlayerID, EstRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  ungroup() %>%
  mutate(ratio = positive / (positive + negative)) %>%
  select(PlayerID, EstRule, ratio) %>%
  posthoc_wilcox_test(ratio ~ EstRule) %>%
  output_posthoc_result("posthoc_wilcox_test_ratio_score_est_rule", analysis_group = "ratio_score")

df_rule_hit %>%
  group_by(PlayerID, EstRule, DisplayScore) %>%
  summarise(count = n()) %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  pivot_wider(names_from = DisplayScore, values_from = count) %>%
  mutate(ratio = positive / (positive + negative)) %>%
  select(PlayerID, EstRule, ratio) %>%
  posthoc_wilcox_against_chance(ratio ~ EstRule, alternative = "two.sided") %>%
  output_posthoc_result(
    "posthoc_wilcox_test_ratio_score_est_rule_against_chance",
    analysis_group = "ratio_score"
  )


p_choice_ratio <- df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID, DisplayScore, EstRule) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = EstRule, values_from = count) %>%
  mutate(ratio = skill / (skill + random), total = skill + random) %>%
  ggplot(aes(x = DisplayScore, y = ratio, alpha = DisplayScore, fill = "black")) +
  gg_filled_box_sina(box_width = box_width_2box) +
  geom_signif(comparisons = list(c("negative", "positive")), test = "wilcox.test", map_signif_level = TRUE, test.args = list(paired = TRUE, exact = TRUE), alpha = 1, color = "black", textsize = 6, margin_top = 0.15) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), minor_breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0.05, 0.2))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_fig +
  guides(color = guide_legend(color = FALSE), alpha = FALSE) +
  xlab("score") +
  ylab("P(skill)")

wilcoxsign_choice_ratio <-
  df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID, DisplayScore, EstRule) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = EstRule, values_from = count) %>%
  mutate(ratio = skill / (skill + random), total = skill + random) %>%
  ungroup() %>%
  select(PlayerID, DisplayScore, ratio) %>%
  pivot_wider(names_from = DisplayScore, values_from = ratio) %>%
  coin::wilcoxsign_test(positive ~ negative, distribution = "exact", data = ., zero.method = "Wilcoxon")

p_choice_ratio %T>% save_svg_figure("ratio of choice for score",
  scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm"
)


## ----example------------------------------------------------------------------
p_example <-
  df_rule_hit %>%
  filter(PlayerID == 12) %>%
  mutate(Correct = if_else(Correct, "correct", "error")) %>%
  mutate(EstRule = as.numeric(EstRule == "skill"), TrueRule = as.numeric(TrueRule == "skill")) %>%
  mutate(EstRule = slide_dbl(EstRule, mean, .before = 14, .after = 0)) %>%
  ggplot(aes(x = TrialID)) +
  geom_line(aes(y = TrueRule, group = 1, linetype = true_rule_name)) +
  geom_line(aes(y = EstRule, linetype = est_rule_name)) +
  guides(shape = "none") +
  ylab("P(skill)") +
  xlab("trial") +
  labs(linetype = "") +
  theme_fig_example

p_example %T>% save_svg_figure("p_example", scaling = fig_anova_scale, width = fig_example_width * 1.2, height = fig_example_height * 1.2, unit = "mm")

p_example2 <-
  df_rule_hit %>%
  filter(PlayerID == 32) %>%
  mutate(Correct = if_else(Correct, "correct", "error")) %>%
  mutate(EstRule = as.numeric(EstRule == "skill"), TrueRule = as.numeric(TrueRule == "skill")) %>%
  mutate(EstRule = slide_dbl(EstRule, mean, .before = 14, .after = 0)) %>%
  ggplot(aes(x = TrialID)) +
  geom_line(aes(y = TrueRule, group = 1, linetype = true_rule_name)) +
  geom_line(aes(y = EstRule, linetype = est_rule_name)) +
  guides(shape = "none") +
  ylab("P(skill)") +
  xlab("trial") +
  labs(linetype = "") +
  theme_fig

p_example2 %T>% save_svg_figure("p_example2",
  scaling = fig_anova_scale,
  width = fig_example_width,
  height = fig_example_height,
  unit = "mm"
)



## p_val_timeseries---------------------------------------------
df_acc_timeseries_p <-
  df_rule_hit %>%
  process_for_switch(Correct, accuracy) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>%
  group_by(`num of trials`) %>%
  wilcox_test(accuracy ~ switch, paired = TRUE, data = ., exact = TRUE) %>%
  adjust_pvalue(method = "fdr") %>%
  mutate(star = if_else(p.adj < 0.001, "***",
    if_else(p.adj < 0.01, "**",
      if_else(p.adj < 0.05, "*", "")
    )
  ))

# calculate p value and Z value for  of performance with coin package
df_acc_timeseries_p_coin <-
  df_rule_hit %>%
  process_for_switch(Correct, accuracy) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>%
  select(-TrueRule) %>%
  pivot_wider(names_from = switch, values_from = accuracy) %>%
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
  ))

df_acc_timeseries_p_coin %>%
  select(-data, -wilcoxsign) %>%
  output_csv(
    "wilcox_test_switch_acc",
    analysis_group = "time_series"
  )

df_conf_timeseries_p <-
  df_rule_hit %>%
  process_for_switch(zConfidence, mean_conf) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>%
  group_by(`num of trials`) %>%
  wilcox_test(mean_conf ~ switch, paired = TRUE, data = .) %>%
  adjust_pvalue(method = "fdr") %>%
  mutate(star = if_else(p.adj < 0.001, "***",
    if_else(p.adj < 0.01, "**",
      if_else(p.adj < 0.05, "*", "")
    )
  ))

df_conf_timeseries_p_coin <-
  df_rule_hit %>%
  process_for_switch(zConfidence, mean_conf) %>%
  ungroup() %>%
  filter(`num of trials` > -1 & `num of trials` < 8) %>%
  select(-TrueRule) %>%
  pivot_wider(names_from = switch, values_from = mean_conf) %>%
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
  ))

df_conf_timeseries_p_coin %>%
  select(-data, -wilcoxsign) %>%
  output_csv(
    "wilcox_test_switch_conf",
    analysis_group = "time_series"
  )


## -------------------------------------------------------------------
generate_tick_for_time_series <- function(limits, label_step = 1) {
  list(
    scale_x_continuous(
      limits = c(limits[1] - 0.4, limits[2] + 0.4),
      breaks = seq(limits[1] - 0.4, limits[2] + 0.4, label_step),
      minor_breaks = seq(limits[1], limits[2], label_step),
      labels = c(seq(limits[1], 0, label_step) - 1, seq(label_step, limits[2], label_step))
    )
  )
}
tick_for_time_series <- generate_tick_for_time_series(c(-3, 8))
# list(
#   scale_x_continuous(
#     limits = c(-3.4, 8.4), breaks = seq(-3, 8, 1), minor_breaks = seq(-3, 8, 1),
#     labels = c(seq(-3, 0, 1) -1, seq(1, 8, 1))
#     )
#   )
tick_for_time_series_15 <- generate_tick_for_time_series(c(-15, 15), 5)
# list(
#   scale_x_continuous(
#     limits = c(-15.4, 15.4), breaks = seq(-15, 15, 1), minor_breaks = seq(-15, 15, 1),
#     labels = c(seq(-15, 0, 1) -1, seq(1, 8, 1))
#     )
#   )

p_switch_acc <- df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  group_by(PlayerID, switch, `num of trials`) %>%
  summarise(prob = mean(EstRule == "skill"), accuracy = mean(EstRule == TrueRule)) %>%
  dplyr::select(PlayerID, switch, `num of trials`, accuracy) %>%
  mutate(switch = if_else(switch == "TrialsAfterSwitchToRandom", "skill to random", "random to skill")) %>%
  ggplot(aes(x = `num of trials` + 1, y = accuracy, group = switch)) +
  stat_summary(aes(linetype = switch), fun = mean, geom = "line") +
  stat_summary(aes(linetype = switch),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  geom_text(data = df_acc_timeseries_p_coin, mapping = aes(x = `num of trials` + 1, y = Inf, label = star), group = 1, color = 1, size = 8) +
  coord_cartesian(xlim = c(-3.4, 8.4), ylim = c(0.2, 0.8)) +
  xlab("trials after the switch") +
  ylab("p(correct state inference)") + # + c(theme_fig, tick_for_time_series)
  theme_fig +
  tick_for_time_series

p_switch_acc %T>% save_svg_figure("p_switch_acc",
  analysis_group = "time_series",
  scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height, unit = "mm"
)

p_switch_conf <- df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  group_by(PlayerID, switch, `num of trials`) %>%
  summarise(confidence = mean(zConfidence)) %>%
  dplyr::select(PlayerID, switch, `num of trials`, confidence) %>%
  mutate(switch = if_else(switch == "TrialsAfterSwitchToRandom", "skill to random", "random to skill")) %>%
  ggplot(aes(x = `num of trials` + 1, y = confidence, group = switch, fill = "black"), color = "black") +
  stat_summary(fun = mean, geom = "line", mapping = aes(linetype = switch)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2),
    mapping = aes(linetype = switch)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  geom_text(data = df_conf_timeseries_p, mapping = aes(x = `num of trials` + 1, y = 0.2, label = star), group = 1, size = 8, show.legend = FALSE) +
  coord_cartesian(xlim = c(-3.4, 8.4), ylim = c(-0.35, 0.25)) +
  xlab("trials after the switch") +
  ylab("confidence") +
  theme_fig +
  tick_for_time_series

p_switch_conf %T>% save_svg_figure("p_switch_conf", analysis_group = "time_series", scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height, unit = "mm")

p_switch_conf_15 <- df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  group_by(PlayerID, switch, `num of trials`) %>%
  summarise(confidence = mean(zConfidence)) %>%
  dplyr::select(PlayerID, switch, `num of trials`, confidence) %>%
  mutate(switch = if_else(switch == "TrialsAfterSwitchToRandom", "skill to random", "random to skill")) %>%
  ggplot(aes(x = `num of trials` + 1, y = confidence, group = switch, fill = "black"), color = "black") +
  stat_summary(fun = mean, geom = "line", mapping = aes(linetype = switch)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2),
    mapping = aes(linetype = switch)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  # geom_text(data = df_conf_timeseries_p, mapping = aes(x = `num of trials` + 1, y = 0.2, label = star), group = 1, size = 8, show.legend = FALSE) +
  coord_cartesian(xlim = c(-15, 15), ylim = c(-0.35, 0.25)) +
  xlab("trials after the switch") +
  ylab("confidence") +
  theme_fig +
  tick_for_time_series_15 +
  # change legend position to top right inside the plot and make it transparent
  theme(legend.position = c(0.82, 0.22), legend.background = element_rect(fill = "transparent")) +
  # delete legend title
  guides(linetype = guide_legend(title = NULL))

p_switch_conf_15 %T>% save_svg_figure("p_switch_conf_15", analysis_group = "time_series", scaling = fig_anova_scale, width = fig_timeseries_width, height = fig_timeseries_height * 1.2, unit = "mm")



## -----------------------------------------------------------------------------
df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  group_by(PlayerID, switch, `num of trials`) %>%
  summarise(prob = mean(EstRule == "skill"), accuracy = mean(EstRule == TrueRule)) %>%
  dplyr::select(PlayerID, switch, `num of trials`, accuracy) %>%
  mutate(switch = if_else(switch == "TrialsAfterSwitchToRandom", "skill to random", "random to skill")) %>%
  filter(`num of trials` >= 0 & `num of trials` < 8) %T>%
  output_csv(
    "switch_acc",
    analysis_group = "time_series"
  ) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  print() %>%
  sink_analysis(filename = "anova_switch_acc.txt", analysis_group = "time_series")

df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  group_by(PlayerID, switch, `num of trials`) %>%
  summarise(confidence = mean(zConfidence)) %>%
  dplyr::select(PlayerID, switch, `num of trials`, confidence) %>%
  mutate(switch = if_else(switch == "TrialsAfterSwitchToRandom", "skill to random", "random to skill")) %>%
  filter(`num of trials` >= 0 & `num of trials` < 8) %T>%
  output_csv(
    "switch_conf",
    analysis_group = "time_series"
  ) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  print() %>%
  sink_analysis(filename = "anova_switch_conf.txt", analysis_group = "time_series")


## -------------------------------------------------------------------
p_switch_distance <-
  df_rule_hit %>%
  process_for_switch(Distance / threshold, Distance) %>%
  ggplot(aes(x = `num of trials` + 1, y = Distance, group = switch, color = switch), linetype = "solid") +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-3.4, 8.4), ylim = c(0, 2)) +
  geom_hline(yintercept = 1) +
  xlab("trials after the switch") +
  ylab("mean of normalised distance") +
  theme_fig +
  tick_for_time_series
p_switch_distance %>% save_svg_figure("p_switch_distance",
  analysis_group = "switch_distance",
  width = fig_timeseries_width, height = fig_timeseries_height, scaling = fig_anova_scale, unit = "mm"
)


## score difference around the switch ------
p_switch_score <-
  df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  ggplot(aes(x = `num of trials` + 1, y = DisplayScore, group = switch, color = switch)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-3.4, 8.4), ylim = c(0.4, 0.7)) +
  xlab("trials after the switch") +
  ylab("mean of score\n(0: negative, 1: positive)") +
  theme_fig +
  tick_for_time_series

p_switch_score %>% save_svg_figure("p_switch_score",
  analysis_group = "switch_score",
  width = fig_timeseries_width, height = fig_timeseries_height, scaling = fig_anova_scale, unit = "mm"
)

p_switch_score_with_individual <-
  df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  ggplot(aes(x = `num of trials` + 1, y = DisplayScore, group = switch, color = switch)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(
    aes(
      group = interaction(switch, PlayerID),
      color = switch
    ),
    fun = mean,
    geom = "line",
    alpha = 0.2,
    size = 0.5,
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-3.4, 8.4), ylim = c(0.4, 0.7)) +
  xlab("trials after the switch") +
  ylab("mean of score\n(0: negative, 1: positive)") +
  theme_fig +
  tick_for_time_series

p_switch_score_with_individual %>% save_svg_figure("p_switch_score_with_individual",
  analysis_group = "switch_score",
  width = fig_timeseries_width, height = fig_timeseries_height, scaling = fig_anova_scale, unit = "mm"
)

p_switch_score_15 <-
  df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  ggplot(aes(x = `num of trials` + 1, y = DisplayScore, group = switch, color = switch)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-15, 15), ylim = c(0.4, 0.7)) +
  xlab("trials after the switch") +
  ylab("mean of score\n(0: negative, 1: positive)") +
  theme_fig +
  tick_for_time_series_15

p_switch_score_15 %>% save_svg_figure("p_switch_score_15",
  analysis_group = "switch_score",
  width = fig_timeseries_width, height = fig_timeseries_height * 1.2, scaling = fig_anova_scale, unit = "mm"
)

p_switch_score_15_player <-
  df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  filter(PlayerID == 12) %>%
  ggplot(aes(x = `num of trials` + 1, y = DisplayScore, group = switch, color = switch)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.7,
    size = 0.8,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
  coord_cartesian(xlim = c(-30, 30)) +
  xlab("trials after the switch") +
  ylab("mean of score\n(0: negative, 1: positive)") +
  theme_fig
p_switch_score_15_player %>% save_svg_figure("p_switch_score_15_player_6",
  analysis_group = "switch_score",
  width = fig_timeseries_width, height = fig_timeseries_height * 1.2, scaling = fig_anova_scale, unit = "mm"
)

## comparison between before and after switch ----
df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  filter(`num of trials` == 0 |
    `num of trials` == -1) %>%
  group_by(PlayerID, switch, DisplayScore) %>%
  select(-TrueRule) %>%
  pivot_wider(names_from = `num of trials`, values_from = DisplayScore) %>%
  # filter(switch == "skill to random") %>%
  filter(switch == "random to skill") %>%
  coin::wilcoxsign_test(`0` ~ `-1`, distribution = "exact", data = ., zero.method = "Wilcoxon")

df_rule_hit %>%
  process_for_switch(DisplayScore, DisplayScore) %>%
  filter(`num of trials` == 0 |
    `num of trials` == -1) %>%
  group_by(PlayerID, switch, DisplayScore) %>%
  select(-TrueRule) %>%
  pivot_wider(names_from = `num of trials`, values_from = DisplayScore) %>%
  filter(switch == "skill to random") %>%
  coin::wilcoxsign_test(`0` ~ `-1`, distribution = "exact", data = ., zero.method = "Wilcoxon")

df_rule_hit %>%
  pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
  dplyr::select(PlayerID, switch, TrueRule, `num of trials`, DisplayScore) %>%
  mutate(
    switch =
      if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
  ) %>%
  filter(`num of trials` == -1 |
    `num of trials` == 0) %>%
  lme4::glmer(
    DisplayScore ~ switch * `num of trials` + (1 | PlayerID),
    family = binomial,
    data = .
  ) %>%
  summary()
## -------------------------------------------------------------------
p_switch_distance


## accuracy_score-----------------------------------------------
df_rule_hit_for_test <-
  df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  mutate(PlayerID = as.factor(PlayerID)) %>%
  # mutate(Correct = as.numeric(Correct)) %>%
  mutate(Correct = as.logical(Correct)) %>%
  group_by(PlayerID) %>%
  mutate(scale_conf = scale(EstRuleConfidence, center = FALSE, scale = TRUE)) %>%
  ungroup()

p_per_score_T <- df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(accuracy = mean(Correct)) %>%
  ggplot(aes(x = `DisplayScore`, y = `accuracy`, alpha = DisplayScore, fill = "black")) +
  gg_filled_box_sina(box_width = box_width_2box) +
  coord_cartesian(ylim = c(0.4, 1.0)) +
  xlab("score") +
  ylab("p(correct state inference)") +
  theme(aspect.ratio = 1.6) +
  geom_signif(comparisons = list(c("negative", "positive")), map_signif_level = T, test = "wilcox.test", test.args = list(paired = TRUE, exact = TRUE), color = "black", alpha = 1, textsize = 6, margin_top = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_fig +
  theme(legend.position = "none")
p_per_score_T %>% save_svg_figure("p_per_score_T", analysis_group = "per_score", scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm")

df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(accuracy = mean(Correct)) %>%
  posthoc_wilcox_test(accuracy ~ DisplayScore, fixed_factor = NULL) %>%
  output_posthoc_result("posthoc_wilcox_test_per_score", analysis_group = "per_score")

df_per_score_test_against_chance <- df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(accuracy = mean(Correct)) %>%
  mutate(chance_level = 0.5) %>%
  group_by(DisplayScore) %>% # DisplayScoreでグループ化
  wilcox_test(accuracy ~ 1, mu = 0.5, alternative = "greater", exact = TRUE) %>% # Wilcoxon符号順位検定
  adjust_pvalue(method = "fdr") %>%
  add_significance(p.col = "p.adj")

df_per_score_test_against_chance_coin <-
  df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(accuracy = mean(Correct)) %>%
  group_by(DisplayScore) %>%
  mutate(chance_level = 0.5) %>%
  nest() %>%
  mutate(
    wilcoxsign =
      map(
        .x = data, .f =
          ~ .x %>%
            select(accuracy, chance_level) %>%
            coin::wilcoxsign_test(accuracy ~ chance_level, data = ., distribution = "exact", zero.method = "Wilcoxon", alternative = "greater")
      ),
    Z = sapply(wilcoxsign, coin::statistic),
    p_coin = sapply(wilcoxsign, coin::pvalue)
  ) %>%
  select(-data, -wilcoxsign)

df_per_score_test_against_chance %>%
  inner_join(df_per_score_test_against_chance_coin, by = "DisplayScore") %>%
  select(.y., DisplayScore, group1, group2, n, statistic, Z, p, p_coin, p.adj, p.adj.signif) %>%
  output_csv(
    "per_score_T_test_against_chance",
    analysis_group = "per_score"
  )

p_conf_score <-
  df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(zConfidence = mean(zConfidence)) %>%
  ggplot(aes(x = DisplayScore, y = zConfidence, alpha = DisplayScore, fill = "black")) +
  # geom_line(aes(group = PlayerID, color=PlayerID), alpha = 0.2, show.legend = F) +
  gg_filled_box_sina(box_width = box_width_2box) +
  geom_signif(comparisons = list(c("negative", "positive")), test = "wilcox.test", test.args = list(paired = TRUE, exact = TRUE), map_signif_level = TRUE, color = "black", alpha = 1, textsize = 6, margin_top = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  xlab("score") +
  ylab("confidence") +
  theme_fig +
  theme(legend.position = "none")

df_test_conf_score <-
  df_rule_hit_for_test %>%
  group_by(PlayerID, DisplayScore) %>%
  summarise(zConfidence = mean(zConfidence)) %>%
  posthoc_wilcox_test(zConfidence ~ DisplayScore, fixed_factor = NULL)

df_test_conf_score %>%
  output_posthoc_result("posthoc_wilcox_test_conf_score", analysis_group = "conf_score")

p_conf_score %>% save_svg_figure("p_conf_score", analysis_group = "conf_score", scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm")


## -----------------------------------------------------------------------------
p_conf_correct <-
  df_rule_hit_for_test %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect")
  ) %>%
  group_by(PlayerID, Correct) %>%
  summarise(zConfidence = mean(zConfidence)) %>%
  ggplot(aes(x = Correct, y = zConfidence, alpha = "good")) +
  gg_filled_box_sina(box_width = box_width_2box) +
  geom_signif(
    comparisons = list(c("correct", "incorrect")),
    test = "wilcox.test",
    test.args = list(paired = TRUE, exact = TRUE),
    map_signif_level = TRUE, color = "black",
    alpha = 1, textsize = 6,
    margin_top = 0.15
  ) +
  scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_linetype_manual(values = c(correct = 1, incorrect = 2)) +
  xlab("") +
  ylab("confidence") +
  theme_fig +
  theme(legend.position = "none")

p_conf_correct %>% save_svg_figure("p_conf_correct", analysis_group = "conf_correct", scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm")

wilcoxsign_conf_correct <-
  df_rule_hit_for_test %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect")
  ) %>%
  group_by(PlayerID, Correct) %>%
  summarise(zConfidence = mean(zConfidence)) %>%
  ungroup() %>%
  select(PlayerID, Correct, zConfidence) %>%
  pivot_wider(names_from = Correct, values_from = zConfidence) %>%
  coin::wilcoxsign_test(correct ~ incorrect, distribution = "exact", data = ., zero.method = "Wilcoxon")

df_test_conf_correct <-
  df_rule_hit_for_test %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect")
  ) %>%
  group_by(PlayerID, Correct) %>%
  summarise(zConfidence = mean(zConfidence)) %>%
  ungroup() %>%
  posthoc_wilcox_test(zConfidence ~ Correct, fixed_factor = NULL)

df_test_conf_score %>%
  output_posthoc_result("posthoc_wilcox_test_conf_correct", analysis_group = "conf_correct")



## -----------------------------------------------------------------------------
(df_rule_hit_for_test %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect")
  ) %>%
  summarize_performance(Correct, zConfidence, DisplayScore) %>%
  ggplot(aes(x = DisplayScore, y = mean_zConfidence, group = interaction(DisplayScore, Correct), fill = "black", alpha = DisplayScore, linetype = Correct)) +
  gg_filled_box_sina() +
  theme_fig_boxplot +
  guides(alpha = FALSE, linetype = guide_legend(override.aes = list(color = "black"))) +
  labs(linetype = "") +
  xlab("score") +
  ylab("confidence")) %T>%
  save_svg_figure(
    "conf_correct_score_fig",
    analysis_group = "conf_correct",
    scaling = fig_anova_scale,
    width = fig_anova_width,
    height = fig_anova_height,
    unit = "mm"
  )

df_rule_hit_for_test %>%
  mutate(
    Correct = if_else(Correct, "correct", "incorrect")
  ) %>%
  do_anova(Correct, DisplayScore, zConfidence, tech = FALSE) %>%
  sink_analysis(
    "anova_conf_correct_score.txt",
    analysis_group = "conf_correct"
  )


## -------------------------------------------------------------------
p_accuracy_score_truerule <-
  df_rule_hit %>%
  plot_score_rule(TrueRule, Correct, DisplayScore, .alpha = DisplayScore, .fill = TrueRule) +
  theme_fig + ylab("p(correct state inference)") + xlab("task state") + labs(alpha = "score") + guides(fill = FALSE) +
  theme(legend.position = "right")
p_accuracy_score_truerule %>% save_svg_figure("p_accuracy_score_truerule", analysis_group = "accuracy_score_truerule", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")

df_rule_hit %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(`p(correct state inference)` = mean(Correct)) %>%
  group_by(TrueRule, DisplayScore) %>%
  wilcox_test(`p(correct state inference)` ~ 1, mu = 0.5) %>%
  adjust_pvalue(method = "fdr") %>%
  add_significance() %>%
  output_csv(
    "wilcox_test_accuracy_score_truerule",
    analysis_group = "accuracy_score_truerule"
  )


## -------------------------------------------------------------------
df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, Correct, TrueRule) %>%
  select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_DisplayScore_TrueRule_mean_Correct.txt", analysis_group = "accuracy_score_truerule")

df_rule_hit_for_test %>%
  lme4::glmer(Correct ~ TrueRule * DisplayScore + (1 | PlayerID), data = ., family = binomial) %>%
  output_lme_results("lme_accuracy_score_truerule",
    analysis_group = "accuracy_score_truerule"
  )

df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, Correct, TrueRule) %>%
  select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
  posthoc_wilcox_test2(mean_Correct, TrueRule, DisplayScore) %>%
  output_posthoc_result("posthoc_wilcox_test_accuracy_score_truerule", analysis_group = "accuracy_score_truerule")


## -------------------------------------------------------------------
df_posthoc_acc_score_truerule <- df_rule_hit %>% summary_posthoc_test2(Correct, DisplayScore, TrueRule)



## -------------------------------------------------------------------
p_accuracy_score_estrule <-
  df_rule_hit %>%
  plot_score_rule(EstRule, Correct, DisplayScore, .alpha = DisplayScore, .fill = EstRule) +
  theme_fig + ylab("p(correct state inference)") + xlab(est_rule_name) + labs(alpha = "score") + guides(fill = FALSE) +
  theme(legend.position = "right")
p_accuracy_score_estrule %>% save_svg_figure("p_accuracy_score_estrule", analysis_group = "accuracy_score_estrule", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")


## -------------------------------------------------------------------

df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, Correct, EstRule) %>%
  select(PlayerID, DisplayScore, EstRule, mean_Correct) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_DisplayScore_EstRule_mean_Correct.txt", analysis_group = "accuracy_score_estrule")

df_rule_hit_for_test %>%
  lme4::glmer(Correct ~ EstRule * DisplayScore + (1 | PlayerID),
    data = .,
    family = binomial
  ) %>%
  output_lme_results("lme_accuracy_score_estrule", analysis_group = "accuracy_score_estrule")

df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, Correct, EstRule) %>%
  select(PlayerID, DisplayScore, EstRule, mean_Correct) %>%
  posthoc_wilcox_test2(mean_Correct, EstRule, DisplayScore) %>%
  output_posthoc_result("posthoc_wilcox_test_accuracy_score_estrule", analysis_group = "accuracy_score_estrule")


## -------------------------------------------------------------------
df_posthoc_acc_score_estrule <- df_rule_hit %>% summary_posthoc_test2(Correct, DisplayScore, EstRule)


## -------------------------------------------------------------------
p_conf_score_truerule <- df_rule_hit %>%
  plot_score_rule(TrueRule, zConfidence, DisplayScore, .alpha = DisplayScore, .fill = TrueRule) +
  theme_fig + ylab("confidence") + xlab("task state") + labs(alpha = "score") +
  theme(legend.position = "right") + guides(fill = FALSE)
p_conf_score_truerule %>% save_svg_figure("p_conf_score_truerule", analysis_group = "conf_score_truerule", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")


## -------------------------------------------------------------------
df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, zConfidence, TrueRule) %>%
  select(PlayerID, DisplayScore, TrueRule, mean_zConfidence) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_DisplayScore_TrueRule_mean_zConfidence.txt", analysis_group = "conf_score_truerule")

df_rule_hit_for_test %>%
  group_by(PlayerID) %>%
  mutate(scale_conf = scale(EstRuleConfidence, center = FALSE, scale = TRUE)) %>%
  ungroup() %>%
  lmerTest::lmer(scale_conf ~ TrueRule * DisplayScore + (1 | PlayerID), data = .) %>%
  output_lme_results("lme_conf_score_truerule", analysis_group = "conf_score_truerule")


## -------------------------------------------------------------------
df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, zConfidence, TrueRule) %>%
  select(PlayerID, DisplayScore, TrueRule, mean_zConfidence) %>%
  posthoc_wilcox_test2(mean_zConfidence, TrueRule, DisplayScore) %T>% print() %>%
  output_posthoc_result("posthoc_wilcox_test_conf_score_truerule", analysis_group = "conf_score_truerule")


## -------------------------------------------------------------------
df_posthoc_conf_score_truerule <- df_rule_hit %>% summary_posthoc_test2(zConfidence, DisplayScore, TrueRule)


## -------------------------------------------------------------------
p_conf_score_estrule <-
  df_rule_hit %>%
  plot_score_rule(EstRule, zConfidence, DisplayScore, .alpha = DisplayScore, .fill = EstRule) +
  theme_fig + ylab("confidence") + xlab(est_rule_name) +
  labs(fill = est_rule_name, alpha = "score") + guides(fill = FALSE) +
  theme(legend.position = "right")
# theme(legend.position = "none")
p_conf_score_estrule %>% save_svg_figure("p_conf_score_estrule", analysis_group = "conf_score_estrule", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm")


## -------------------------------------------------------------------
df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, zConfidence, EstRule) %>%
  select(PlayerID, DisplayScore, EstRule, mean_zConfidence) %>%
  anovakun("sAB", long = TRUE, gg = TRUE) %>%
  sink_analysis(filename = "anova_DisplayScore_EstRule_mean_zConfidence.txt", analysis_group = "conf_score_estrule")

df_rule_hit_for_test %>%
  group_by(PlayerID) %>%
  mutate(scale_conf = scale(EstRuleConfidence, center = FALSE, scale = TRUE)) %>%
  ungroup() %>%
  lmerTest::lmer(scale_conf ~ EstRule * DisplayScore + (1 | PlayerID), data = .) %>%
  output_lme_results("lme_conf_score_estrule", analysis_group = "conf_score_estrule")

df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, zConfidence, EstRule) %>%
  select(PlayerID, DisplayScore, EstRule, mean_zConfidence) %>%
  posthoc_wilcox_test2(mean_zConfidence, EstRule, DisplayScore) %>%
  output_posthoc_result("posthoc_wilcox_test_conf_score_estrule", analysis_group = "conf_score_estrule")


## -------------------------------------------------------------------
df_posthoc_conf_score_estrule <- df_rule_hit %>% summary_posthoc_test2(zConfidence, DisplayScore, EstRule)


## -------------------------------------------------------------------
df_rule_hit_for_test %>%
  summarize_performance(DisplayScore, zConfidence, EstRule) %>%
  select(PlayerID, DisplayScore, EstRule, mean_zConfidence) %>%
  aov(mean_zConfidence ~ DisplayScore * EstRule + Error(PlayerID / (DisplayScore * EstRule)), data = .) %>%
  summary() # almost same result as anova-kun


## -------------------------------------------------------------------
p_switch_prev_choice_score <-
  df_rule_hit %>%
  process_for_summary_anova() %>%
  group_by(PlayerID, prev_choice, DisplayScore) %>%
  summarise(switch_prob = mean(switch)) %>%
  mutate(condition = interaction(DisplayScore, prev_choice)) %>%
  ggplot(aes(x = prev_choice, y = switch_prob, group = interaction(prev_choice, DisplayScore), fill = prev_choice, alpha = DisplayScore)) +
  gg_filled_box_sina() +
  theme_fig +
  ylab("switch probability") +
  xlab("previouce choice") +
  labs(alpha = "score", fill = "") +
  theme(legend.position = "right") +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = FALSE)
p_switch_prev_choice_score %>% save_svg_figure("p_switch_prev_choice_score", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "switch_prev_choice_score")

(p_switch_prev_choice_score + coord_cartesian(ylim = c(0, 1))) %>% save_svg_figure("p_switch_prev_choice_score_axis_scaled", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "switch_prev_choice_score")


## -------------------------------------------------------------------
p_conf_switch <- df_rule_hit %>%
  process_for_summary_anova() %>%
  group_by(PlayerID, conf_binary, DisplayScore) %>%
  summarise(switch_prob = mean(switch)) %>%
  mutate(conf_binary = factor(conf_binary, levels = c("low", "high"))) %>%
  ggplot(aes(x = conf_binary, y = switch_prob, group = interaction(DisplayScore, conf_binary), alpha = DisplayScore, fill = "black")) +
  gg_filled_box_sina() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_fig +
  ylab("switch probability") +
  xlab("confidence") +
  labs(alpha = "score") +
  theme(legend.position = "right") +
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))), fill = FALSE)
p_conf_switch %>% save_svg_figure("p_conf_switch",
  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm",
  analysis_group = "switch_conf_score"
)

(p_conf_switch + coord_cartesian(ylim = c(0, 1))) %>% save_svg_figure("p_conf_switch_axis_scaled",
  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm",
  analysis_group = "switch_conf_score"
)


## -------------------------------------------------------------------
# df_rule_hit %>%
#   process_for_summary_anova() %>%
#   do_anova(prev_choice, DisplayScore, switch, tech=TRUE) %>%
#   print_anova_sentences()
# df_rule_hit %>%
#   process_for_summary_anova() %>% glmer(switch ~ prev_choice*DisplayScore + (1|PlayerID) + (prev_choice|PlayerID) + (DisplayScore|PlayerID), data=., family=binomial(link="logit")) %>% summary()
(df_rule_hit_for_test %>%
  process_for_summary_anova() %>% group_by(PlayerID, conf_binary, TrueRule) %>% summarise(switch = mean(switch)) %>%
  ggplot(aes(x = conf_binary, y = switch, group = interaction(conf_binary, TrueRule), color = TrueRule)) +
  geom_boxplot(outlier.shape = NA) +
  geom_sina_fitted() +
  theme_fig +
  ylab("switch probability") +
  xlab("confidence") +
  labs(color = "true state")) %>%
  save_svg_figure("p_switch_conf_truerule", analysis_group = "switch_conf_truerule")

df_rule_hit_for_test %>%
  process_for_summary_anova() %>%
  glmer(switch ~ conf_binary * TrueRule +
    (conf_binary * TrueRule | PlayerID), data = ., family = binomial) %>%
  output_lme_results("lme_switch_conf_truerule", analysis_group = "switch_conf_truerule")

df_rule_hit %>%
  process_for_summary_anova() %>%
  do_anova(prev_choice, DisplayScore, switch, tech = F) %>%
  print() %>%
  sink_analysis(filename = "anova_DisplayScore_switch_prev_choice_score.txt", analysis_group = "switch_prev_choice_score")

df_rule_hit_for_test %>%
  process_for_summary_anova() %>%
  lme4::glmer(switch ~ prev_choice * DisplayScore + (1 | PlayerID), data = ., family = binomial) %>%
  print() %>%
  output_lme_results("lme_switch_prev_choice_score", analysis_group = "switch_prev_choice_score")


## -------------------------------------------------------------------
df_rule_hit %>%
  process_for_summary_anova() %>%
  do_anova(prev_choice, DisplayScore, switch) %>%
  print_anova_sentences()


## -------------------------------------------------------------------
df_test_score_prev_switch <-
  df_rule_hit %>%
  process_for_summary_anova() %>%
  summary_posthoc_test2(switch, DisplayScore, prev_choice)

df_test_score_prev_switch %>%
  output_posthoc_result("posthoc_wilcox_test_switch_prev_choice_score", analysis_group = "switch_prev_choice_score")


## -------------------------------------------------------------------
df_rule_hit %>%
  process_for_summary_anova() %>%
  do_anova(conf_binary, DisplayScore, switch, tech = F) %>%
  print() %>%
  sink_analysis(filename = "anova_DisplayScore_conf_switch.txt", analysis_group = "switch_conf_score")

df_rule_hit_for_test %>%
  process_for_summary_anova() %>%
  lme4::glmer(switch ~ conf_binary * DisplayScore + (1 | PlayerID), data = ., family = binomial) %>%
  output_lme_results("lme_switch_conf_score", analysis_group = "switch_conf_score")


## -------------------------------------------------------------------
df_rule_hit %>%
  process_for_summary_anova() %>%
  do_anova(conf_binary, DisplayScore, switch, tech = TRUE) %>%
  print_anova_sentences()


## -------------------------------------------------------------------
df_test_score_conf_switch <-
  df_rule_hit %>%
  process_for_summary_anova() %>%
  summary_posthoc_test2(switch, DisplayScore, conf_binary)
p_val_adj_score_switch_on_high <-
  df_test_score_conf_switch %>% pick_post_tests(conf_binary == "high")
p_val_adj_score_switch_on_low <-
  df_test_score_conf_switch %>% pick_post_tests(conf_binary == "low")
p_val_adj_conf_switch_on_positive <-
  df_test_score_conf_switch %>% pick_post_tests(DisplayScore == "positive")
p_val_adj_conf_switch_on_negative <-
  df_test_score_conf_switch %>% pick_post_tests(DisplayScore == "negative")

df_test_score_conf_switch %>%
  output_posthoc_result("posthoc_wilcox_test_switch_conf_score", analysis_group = "switch_conf_score")


## -------------------------------------------------------------------
p_distance_and_choice_conf <- df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore), `P(Skill)` = as.numeric(EstRule == "skill"), PlayerID = as.factor(PlayerID), EstRuleConfidence = as.factor(EstRuleConfidence), scaledDistance = Distance / threshold) %>%
  ggplot(aes(x = scaledDistance, y = `P(Skill)`, group = EstRuleConfidence)) +
  geom_smooth(aes(color = EstRuleConfidence), method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  geom_line(stat = "smooth", aes(group = PlayerID, alpha = 0.2), alpha = 0.2, method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  xlab("normalised distance") +
  labs(color = "confidence level") +
  facet_grid(. ~ DisplayScore) +
  theme_fig +
  scale_color_hue()

p_distance_and_choice_conf %>%
  save_svg_figure("p_distance_and_choice_conf",
    analysis_group = "distance_and_choice_conf",
    scaling = fig_anova_scale, width = 8 * 25.4 * fig_anova_scale, height = 5 * 25.4 * fig_anova_scale, unit = "mm"
  )

p_choice_distance_score <- (df_rule_hit %>%
  mutate(
    DisplayScore = numeric_score_to_strings(DisplayScore),
    `P(Skill)` = as.numeric(EstRule == "skill"),
    PlayerID = as.factor(PlayerID),
    EstRuleConfidence = as.factor(EstRuleConfidence), scaledDistance = Distance / threshold
  ) %>%
  ggplot(aes(x = scaledDistance, y = `P(Skill)`, group = DisplayScore, color = DisplayScore)) +
  geom_smooth(aes(color = DisplayScore), method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  geom_line(stat = "smooth", aes(group = interaction(PlayerID, DisplayScore), alpha = 0.2), alpha = 0.2, method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  xlab("normalised distance") +
  theme_fig +
  labs(color = "score")) %T>%
  save_svg_figure("choice_distance_score",
    analysis_group = "distance_and_choice_conf",
    scaling = fig_anova_scale, unit = "mm",
    width = fig_anova_width,
    height = fig_anova_height
  )


## -----------------------------------------------------------------------------
# x: distance, y: accuracy
# p_distance_and_accuracy =
df_rule_hit %>%
  mutate(
    DisplayScore = if_else(DisplayScore > 0, "positive", "negative")
  ) %>%
  ggplot(aes(
    x = Distance / true_threshold,
    y = as.numeric(Correct),
    group = DisplayScore,
    color = DisplayScore
  )) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  geom_line(
    stat = "smooth",
    aes(
      group = interaction(PlayerID, DisplayScore),
      alpha = 0.2
    ),
    alpha = 0.2,
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  xlab("normalised distance") +
  ylab("p(correct state inference)") +
  theme_fig +
  labs(color = "score")

# x: distance, y: accuracy, color: true state
# p_distance_and_accuracy_true =
df_rule_hit %>%
  mutate(
    DisplayScore = if_else(DisplayScore > 0, "positive", "negative")
  ) %>%
  ggplot(aes(
    x = Distance / true_threshold,
    y = as.numeric(Correct),
    group = TrueRule,
    color = TrueRule
  )) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  geom_line(
    stat = "smooth",
    aes(
      group = interaction(PlayerID, TrueRule),
      alpha = 0.2
    ),
    alpha = 0.2,
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  xlab("normalised distance") +
  ylab("p(correct state inference)") +
  theme_fig +
  labs(color = "task state")

# x: distance, y: accuracy, color: score, facet: true state
# p_distance_and_accuracy_score =
df_rule_hit %>%
  mutate(
    DisplayScore = if_else(DisplayScore > 0, "positive", "negative")
  ) %>%
  ggplot(aes(
    x = Distance / true_threshold,
    y = as.numeric(Correct),
    group = DisplayScore,
    color = DisplayScore
  )) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  geom_line(
    stat = "smooth",
    aes(
      group = interaction(PlayerID, DisplayScore),
      alpha = 0.2
    ),
    alpha = 0.2,
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE
  ) +
  xlab("normalised distance") +
  ylab("p(correct state inference)") +
  theme_fig +
  labs(color = "score") +
  facet_wrap(~TrueRule)


## -------------------------------------------------------------------
p_prev_estimation_distance <-
  df_rule_hit %>%
  group_by(PlayerID) %>%
  mutate(
    prev_choice = lag(EstRule),
    DisplayScore = numeric_score_to_strings(DisplayScore),
    conf_binary = if_else(zConfidence > 0, "high", "low"),
    switch = prev_choice != EstRule,
    scaledDistance = Distance / threshold
  ) %>%
  ungroup() %>%
  group_by(PlayerID, prev_choice) %>%
  summarize(scaledDistance = mean(scaledDistance)) %>%
  drop_na() %>%
  ggplot(aes(x = prev_choice, y = scaledDistance, group = prev_choice, color = prev_choice)) +
  geom_boxplot(aes(group = prev_choice), width = box_width_2box, show.legend = FALSE, outlier.shape = NA) +
  # geom_line(show.legend = FALSE, alpha = 0.8, color = "grey") +
  geom_sina_fitted(alpha = 0.2, show.legend = FALSE, maxwidth = (box_width_2box / 0.75) * 0.5) +
  geom_signif(comparisons = list(c("random", "skill")), test = "wilcox.test", map_signif_level = TRUE, test.args = list(paired = TRUE, exact = TRUE), color = "black", textsize = 6, margin_top = 0.15) +
  theme_fig +
  coord_cartesian(ylim = c(0, 5)) +
  xlab("previous estimation") +
  ylab("normalised distance")

p_prev_estimation_distance %>%
  save_svg_figure("p_prev_estimation_distance",
    analysis_group = "distance_prev_choice",
    scaling = fig_anova_scale, width = fig_2box_width, height = fig_2box_height, unit = "mm"
  )


df_rule_hit %>%
  group_by(PlayerID) %>%
  mutate(
    prev_choice = lag(EstRule),
    DisplayScore = numeric_score_to_strings(DisplayScore),
    conf_binary = if_else(zConfidence > 0, "high", "low"),
    switch = prev_choice != EstRule,
    scaledDistance = Distance / threshold
  ) %>%
  ungroup() %>%
  group_by(PlayerID, prev_choice) %>%
  summarize(scaledDistance = mean(scaledDistance)) %>%
  drop_na() %>%
  posthoc_wilcox_test(scaledDistance ~ prev_choice) %>%
  output_posthoc_result("posthoc_wilcox_test_prev_estimation_distance",
    analysis_group = "distance_prev_choice"
  )





## -----------------------------------------------------------------------------
p_switch_distance / (p_choice_distance_score | p_prev_estimation_distance)



## -------------------------------------------------------------------
df_distance <-
  df_rule_hit %>%
  group_by(PlayerID) %>%
  mutate(
    DisplayScore =
      numeric_score_to_strings(DisplayScore) %>% as.factor(),
    prev_LocX = lag(LocX), prev_LocY = lag(LocY),
    prev_distance = lag(Distance),
    next_distance = lead(Distance),
    next_LocX = lead(LocX), next_LocY = lead(LocY),
    distance_from_prev = Distance - prev_distance,
    distance_from_next = Distance - next_distance,
    normalized_distance_from_prev = distance_from_prev / threshold,
    normalized_distance_from_next = distance_from_next / threshold
  )

p_distance_diff_prev <-
  df_distance %>%
  summarize_performance(EstRule, normalized_distance_from_prev, DisplayScore) %>%
  mutate(condition = interaction(DisplayScore, EstRule)) %>%
  ggplot(aes(x = EstRule, y = mean_normalized_distance_from_prev, group = interaction(EstRule, DisplayScore), fill = EstRule, alpha = DisplayScore)) +
  gg_filled_box_sina() +
  geom_hline(yintercept = 0) +
  xlab(bquote("[" * .(est_rule_name) * "]"[t])) +
  ylab(expression(paste(
    {
      `[distance]`[t]
    },
    " - ",
    {
      `[distance]`[t - 1]
    }
  ))) +
  theme_fig +
  theme(legend.position = "right") +
  labs(alpha = expression(paste({
    `[score]`[t]
  })), fill = "") +
  guides(fill = FALSE) +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  theme(
    axis.title.y = element_text(size = 12)
  )

p_distance_diff_prev %>% save_svg_figure("p_distance_diff_prev",
  analysis_group = "distance_diff_prev",
  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
)


## -------------------------------------------------------------------
df_distance %>%
  drop_na(prev_distance) %>%
  select(-next_distance) %>%
  do_anova(EstRule, DisplayScore, normalized_distance_from_prev) %>%
  print_anova_sentences()
df_distance %>%
  drop_na(prev_distance) %>%
  select(-next_distance) %>%
  do_anova(EstRule, DisplayScore, normalized_distance_from_prev, tech = FALSE) %>%
  print() %>%
  sink_analysis(filename = "anova_DisplayScore_EstRule_distance_from_prev.txt", analysis_group = "distance_diff_prev")


## -------------------------------------------------------------------
df_distance %>%
  drop_na(prev_distance) %>%
  select(-next_distance) %>%
  summarize_performance(
    DisplayScore,
    normalized_distance_from_prev,
    EstRule
  ) %>%
  posthoc_wilcox_test2(
    mean_normalized_distance_from_prev,
    DisplayScore, EstRule
  ) %>%
  output_posthoc_result(
    "posthoc_wilcox_test_DisplayScore_EstRule_distance_from_prev",
    analysis_group = "distance_diff_prev"
  )


## -------------------------------------------------------------------
p_distance_diff_next <-
  df_distance %>%
  summarize_performance(EstRule, normalized_distance_from_next, DisplayScore) %>%
  mutate(condition = interaction(DisplayScore, EstRule)) %>%
  ggplot(aes(x = EstRule, y = mean_normalized_distance_from_next, group = interaction(EstRule, DisplayScore), fill = EstRule, alpha = DisplayScore)) +
  gg_filled_box_sina() +
  geom_hline(yintercept = 0) +
  xlab(bquote("[" * .(est_rule_name) * "]"[t])) +
  ylab(expression(paste(
    {
      `[distance]`[t]
    },
    " - ",
    {
      `[distance]`[t + 1]
    }
  ))) +
  theme_fig +
  theme(legend.position = "right") +
  labs(alpha = expression(paste({
    `[score]`[t]
  })), fill = "") +
  guides(fill = FALSE) +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  theme(
    axis.title.y = element_text(size = 12)
  )

p_distance_diff_next %>% save_svg_figure("p_distance_diff_next",
  analysis_group = "distance_diff_next",
  scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm"
)


## -------------------------------------------------------------------
df_distance %>%
  drop_na(next_distance) %>%
  select(-prev_distance) %>%
  do_anova(EstRule, DisplayScore, normalized_distance_from_next) %>%
  print_anova_sentences()

df_distance %>%
  drop_na(next_distance) %>%
  select(-prev_distance) %>%
  do_anova(EstRule, DisplayScore, normalized_distance_from_next, tech = FALSE) %>%
  print() %>%
  sink_analysis(filename = "anova_DisplayScore_EstRule_distance_from_next.txt", analysis_group = "distance_diff_next")


## -------------------------------------------------------------------
df_distance %>%
  drop_na(next_distance) %>%
  select(-prev_distance) %>%
  summarize_performance(
    DisplayScore,
    normalized_distance_from_next,
    EstRule
  ) %>%
  posthoc_wilcox_test2(
    mean_normalized_distance_from_next,
    DisplayScore, EstRule
  ) %>%
  output_posthoc_result(
    "posthoc_wilcox_test_DisplayScore_EstRule_distance_from_next",
    analysis_group = "distance_diff_next"
  )

## -----------------------------------------------------------------------------
calc_conf_dis_difference <- function(df) {
  df %>%
    mutate(scaledDistance = Distance / threshold) %>%
    group_by(PlayerID) %>%
    mutate(
      prev_distance = lag(scaledDistance),
      prev_conf = lag(zConfidence)
    ) %>%
    mutate(
      diff_conf = zConfidence - prev_conf,
      diff_dis = scaledDistance - prev_distance
    ) %>%
    ungroup() %>%
    select(PlayerID, TrialID, TrialsAfterSwitchToRandom, TrialsAfterSwitchToSkill, diff_conf, diff_dis) %>%
    pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "Trials")
}
lm_diff_conf_dis <- function(df, trials) {
  df2 <- df %>%
    calc_conf_dis_difference() %>%
    dplyr::filter(Trials == trials)

  res_all <- df2 %>%
    estimatr::lm_robust(diff_dis ~ diff_conf * switch, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf * switch")
  res_post_skill <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToSkill") %>%
    estimatr::lm_robust(diff_dis ~ diff_conf, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf (random to skill)")
  res_post_random <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToRandom") %>%
    estimatr::lm_robust(diff_dis ~ diff_conf, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf (skill to random)")
  rbind(res_all, res_post_skill, res_post_random) %>%
    select(formula, term, estimate, std.error, statistic, p.value, df)
}

lm_diff_conf_dis_abs <- function(df, trials) {
  df2 <- df %>%
    calc_conf_dis_difference() %>%
    dplyr::filter(Trials == trials)

  res_all <- df2 %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf) * switch, data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) * switch")
  res_post_skill <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToSkill") %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf), data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) (random to skill)")
  res_post_random <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToRandom") %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf), data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) (skill to random)")
  rbind(res_all, res_post_skill, res_post_random) %>%
    select(formula, term, estimate, std.error, statistic, p.value, df)
}

lm_diff_conf_dis_mean_abs <- function(df, trials) {
  df2 <- df %>%
    calc_conf_dis_difference() %>%
    dplyr::filter(Trials == trials) %>%
    group_by(PlayerID, switch) %>%
    summarise(diff_dis = mean(diff_dis), diff_conf = mean(diff_conf)) %>%
    ungroup()

  res_all <- df2 %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf) * switch, data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) * switch")
  res_post_skill <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToSkill") %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf), data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) (random to skill)")
  res_post_random <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToRandom") %>%
    estimatr::lm_robust(abs(diff_dis) ~ abs(diff_conf), data = .) %>%
    tidy() %>%
    mutate(formula = "abs(diff_dis) ~ abs(diff_conf) (skill to random)")
  rbind(res_all, res_post_skill, res_post_random) %>%
    select(formula, term, estimate, std.error, statistic, p.value, df)
}

lm_diff_conf_dis_mean <- function(df, trials) {
  df2 <- df %>%
    calc_conf_dis_difference() %>%
    dplyr::filter(Trials == trials) %>%
    group_by(PlayerID, switch) %>%
    summarise(diff_dis = mean(diff_dis), diff_conf = mean(diff_conf)) %>%
    ungroup()
  res_all <- df2 %>%
    estimatr::lm_robust(diff_dis ~ diff_conf * switch, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf * switch")
  res_post_skill <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToSkill") %>%
    estimatr::lm_robust(diff_dis ~ diff_conf, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf (random to skill)")
  res_post_random <- df2 %>%
    dplyr::filter(switch == "TrialsAfterSwitchToRandom") %>%
    estimatr::lm_robust(diff_dis ~ diff_conf, data = .) %>%
    tidy() %>%
    mutate(formula = "diff_dis ~ diff_conf (skill to random)")
  rbind(res_all, res_post_skill, res_post_random) %>%
    select(formula, term, estimate, std.error, statistic, p.value, df)
}

## -----------------------------------------------------------------------------
label_conf_dis_diff <- c(
  "2nd - 1st",
  "3rd - 2nd"
)

(df_rule_hit %>%
  calc_conf_dis_difference() %>%
  filter(Trials < 3 & Trials >= 1) %>% group_by(PlayerID, Trials, switch) %>%
  summarise(mean_diff_dis = mean(diff_dis), mean_diff_conf = mean(diff_conf)) %>%
  mutate(
    switch =
      str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
        str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
  ) %>%
  mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>%
  ggplot(aes(x = mean_diff_conf %>% abs(), y = mean_diff_dis %>% abs(), color = switch)) +
  geom_point() +
  stat_smooth(method = estimatr::lm_robust) +
  stat_smooth(aes(group = 1), color = "black", method = estimatr::lm_robust) +
  facet_wrap(. ~ Trials) +
  theme_fig +
  xlab("|mean difference of confidence|") +
  ylab("|mean difference of distance|")) %>% save_svg_figure("conf_dis_diff_mean_abs", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "conf_dis_diff")

(df_rule_hit %>%
  calc_conf_dis_difference() %>%
  filter(Trials < 3 & Trials >= 1) %>% group_by(PlayerID, Trials, switch) %>%
  summarise(mean_diff_dis = mean(diff_dis), mean_diff_conf = mean(diff_conf)) %>%
  mutate(
    switch =
      str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
        str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
  ) %>%
  mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>%
  ggplot(aes(x = mean_diff_conf, y = mean_diff_dis, color = switch)) +
  geom_point() +
  stat_smooth(method = estimatr::lm_robust) +
  stat_smooth(aes(group = 1), color = "black", method = estimatr::lm_robust) +
  facet_wrap(. ~ Trials) +
  theme_fig +
  xlab("mean difference of confidence") +
  ylab("mean difference of distance")) %>% save_svg_figure("conf_dis_diff_mean", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "conf_dis_diff")

(df_rule_hit %>%
  calc_conf_dis_difference() %>%
  filter(Trials < 3 & Trials >= 1) %>%
  mutate(
    switch =
      str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
        str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
  ) %>%
  mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>%
  ggplot(aes(x = diff_conf %>% abs(), y = diff_dis %>% abs(), color = switch)) +
  geom_point() +
  stat_smooth(method = estimatr::lm_robust) +
  stat_smooth(aes(group = 1), color = "black", method = estimatr::lm_robust) +
  facet_wrap(. ~ Trials) +
  theme_fig1 +
  xlab("|difference of confidence|") +
  ylab("|difference of distance|")) %>% save_svg_figure("conf_dis_diff_abs", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "conf_dis_diff")

(df_rule_hit %>%
  calc_conf_dis_difference() %>%
  filter(Trials < 3 & Trials >= 1) %>%
  mutate(
    switch =
      str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
        str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
  ) %>%
  mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>%
  ggplot(aes(x = diff_conf, y = diff_dis, color = switch)) +
  geom_point() +
  stat_smooth(method = estimatr::lm_robust) +
  stat_smooth(aes(group = 1), color = "black", method = estimatr::lm_robust) +
  facet_wrap(. ~ Trials) +
  theme_fig1 +
  xlab("difference of confidence") +
  ylab("difference of distance")) %>% save_svg_figure("conf_dis_diff", scaling = fig_anova_scale, width = fig_anova_width, height = fig_anova_height, unit = "mm", analysis_group = "conf_dis_diff")

## -----------------------------------------------------------------------------
df_rule_hit %>%
  calc_conf_dis_difference() %>%
  filter(Trials < 3 & Trials >= 1) %>%
  mutate(
    switch =
      str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
        str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
  ) %>%
  mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>%
  group_by(Trials) %>%
  nest() %>%
  tibble_lme(diff_dis ~ diff_conf * switch + (1 | PlayerID)) %>%
  unnest(cols = c(res)) %>%
  select(-data) %>%
  rbind(
    df_rule_hit %>%
      calc_conf_dis_difference() %>%
      filter(Trials < 3 & Trials >= 1) %>%
      mutate(
        switch =
          str_replace_all(switch, "TrialsAfterSwitchToRandom", "skill to random") %>%
            str_replace_all("TrialsAfterSwitchToSkill", "random to skill")
      ) %>%
      mutate(Trials = label_conf_dis_diff[Trials] %>% unlist()) %>% group_by(Trials) %>% nest() %>%
      tibble_lme(abs(diff_dis) ~ abs(diff_conf) * switch + (1 | PlayerID)) %>% unnest(cols = c(res)) %>% select(-data)
  ) %>%
  select(formula, everything()) %>%
  output_csv(
    "lmer_diff_conf_dis.csv",
    analysis_group = "diff_conf_dis"
  )


## -----------------------------------------------------------------------------
df_rule_hit %>%
  lm_diff_conf_dis_mean(1) %>%
  output_csv("lm_diff_conf_dis_mean(1).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis_mean(2) %>%
  output_csv("lm_diff_conf_dis_mean(2).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis_mean_abs(1) %>%
  output_csv("lm_diff_conf_dis_mean_abs(1).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis_mean_abs(2) %>%
  output_csv("lm_diff_conf_dis_mean_abs(2).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis(1) %>%
  output_csv("lm_diff_conf_dis(1).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis(2) %>%
  output_csv("lm_diff_conf_dis(2).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis_abs(1) %>%
  output_csv("lm_diff_conf_dis_abs(1).csv", analysis_group = "diff_conf_dis")
df_rule_hit %>%
  lm_diff_conf_dis_abs(2) %>%
  output_csv("lm_diff_conf_dis_abs(2).csv", analysis_group = "diff_conf_dis")



## ----prev_correct-------------------------------------------------------------
df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID) %>%
  mutate(prev_correct = if_else(lag(Correct, 1), "correct", "incorrect")) %>%
  drop_na(prev_correct) %>%
  ungroup() %>%
  group_by(PlayerID, prev_correct, DisplayScore) %>%
  summarise(performance = mean(Correct)) %>%
  ggplot(aes(x = prev_correct, y = performance, group = interaction(prev_correct, DisplayScore), alpha = DisplayScore, fill = "black")) +
  gg_filled_box_sina() +
  theme_fig +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  ylab("p(correct state inference)") +
  xlab("previous trial") +
  labs(alpha = "score")

df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID) %>%
  mutate(
    prev_correct =
      if_else(lag(Correct, 1), "correct", "incorrect"),
    prev_score = lag(DisplayScore)
  ) %>%
  drop_na(prev_correct) %>%
  ungroup() %>%
  group_by(PlayerID, prev_score) %>%
  summarise(mean_score = mean(DisplayScore == "positive")) %>%
  ggplot(aes(x = prev_score, y = mean_score, group = prev_score, alpha = prev_score, fill = "black")) +
  gg_filled_box_sina() +
  theme_fig +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  ylab("p(positive)") +
  xlab("previous trial") +
  labs(alpha = "score")

df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID) %>%
  mutate(
    prev_correct =
      if_else(lag(Correct, 1), "correct", "incorrect"),
    prev_score = lag(DisplayScore)
  ) %>%
  drop_na(prev_correct) %>%
  ungroup() %>%
  group_by(PlayerID, prev_score, prev_correct) %>%
  summarise(mean_score = mean(DisplayScore == "positive")) %>%
  ggplot(aes(x = prev_correct, y = mean_score, group = interaction(prev_score, prev_correct), alpha = prev_score, fill = "black")) +
  gg_filled_box_sina() +
  theme_fig +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  ylab("p(positive)") +
  xlab("previous trial") +
  labs(alpha = "score")

df_rule_hit %>%
  mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
  group_by(PlayerID) %>%
  mutate(prev_correct = if_else(lag(Correct, 1), "correct", "incorrect")) %>%
  drop_na(prev_correct) %>%
  ungroup() %>%
  group_by(PlayerID, prev_correct) %>%
  summarise(performance = mean(Correct)) %>%
  ggplot(aes(x = prev_correct, y = performance, group = prev_correct, fill = "black", color = "black", alpha = "1")) +
  gg_filled_box_sina() +
  theme_fig +
  guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
  ylab("p(correct state inference)") +
  xlab("previous trial")



## ----score metacognition------------------------------------------------------
df_score_hit %>%
  ungroup() %>%
  mutate(Error = abs(EstScore - TrueScore)) %>%
  group_by(PlayerID) %>%
  mutate(zConfidence = scale(EstScoreConfidence), scaledError = scale(Error)) %>%
  ungroup() %>%
  lmer(Error ~ zConfidence + (1 + zConfidence | PlayerID), data = .) %>%
  summary()

df_score_conf_lme <-
  tibble(
    PlayerID = df_score_hit %>% pull(PlayerID) %>% unique(),
    estimate =
      df_score_hit %>%
        ungroup() %>%
        mutate(Error = abs(EstScore - TrueScore)) %>%
        group_by(PlayerID) %>%
        mutate(
          zConfidence = scale(EstScoreConfidence),
          scaledError = scale(Error)
        ) %>%
        ungroup() %>%
        lmer(scaledError ~ zConfidence + (1 + zConfidence | PlayerID), data = .) %>% coef() %>%
        `$`(PlayerID) %>%
        `$`(zConfidence)
    # true_threshold = df_score_hit %>% group_by(PlayerID) %>% summarise(median = median(Distance)) %>% pull(median)
  ) %>%
  inner_join(
    df_rule_hit %>%
      mutate(PlayerID = as.character(PlayerID)) %>%
      select(PlayerID, true_threshold) %>%
      group_by(PlayerID) %>%
      summarise(true_threshold = unique(true_threshold)),
    by = "PlayerID"
  )

(df_score_hit %>%
  ungroup() %>%
  mutate(PlayerID = as.character(PlayerID), Error = abs(EstScore - TrueScore)) %>%
  group_by(PlayerID) %>%
  mutate(
    zConfidence = scale(EstScoreConfidence),
    scaledError = scale(Error, center = FALSE)
  ) %>%
  ungroup() %>%
  group_by(PlayerID) %>%
  summarise(error = mean(scaledError)) %>%
  inner_join(
    df_rule_hit %>%
      mutate(PlayerID = as.character(PlayerID)) %>%
      select(PlayerID, true_threshold) %>%
      group_by(PlayerID) %>%
      summarise(true_threshold = unique(true_threshold)),
    by = "PlayerID"
  ) %>%
  ggplot(aes(x = true_threshold, y = error)) +
  geom_point() +
  stat_smooth(method = lm_robust, color = "black") +
  theme_fig +
  xlab("threshold") +
  ylab("score estimation error") +
  theme(plot.margin = margin(10, 10, 10, 10))) %>%
  save_svg_figure("p_scaled_score_estimate_error_true_threshold",
    analysis_group = "scaled_score_estimate_error_true_threshold",
    scaling = fig_anova_scale,
    width = fig_anova_width,
    height = fig_anova_height,
    unit = "mm"
  )

# output the result of the robust regression of the fig above to a file
df_score_hit %>%
  ungroup() %>%
  mutate(PlayerID = as.character(PlayerID), Error = abs(EstScore - TrueScore)) %>%
  group_by(PlayerID) %>%
  mutate(
    zConfidence = scale(EstScoreConfidence),
    scaledError = scale(Error, center = FALSE)
  ) %>%
  ungroup() %>%
  group_by(PlayerID) %>%
  summarise(error = mean(scaledError)) %>%
  inner_join(
    df_rule_hit %>%
      mutate(PlayerID = as.character(PlayerID)) %>%
      select(PlayerID, true_threshold) %>%
      group_by(PlayerID) %>%
      summarise(true_threshold = unique(true_threshold)),
    by = "PlayerID"
  ) %>%
  lm_robust(error ~ true_threshold, data = .) %>%
  summary() %>%
  print() %>%
  sink_analysis(
    filename = "lm_robust_score_estimate_error_true_threshold.txt",
    analysis_group = "scaled_score_estimate_error_true_threshold"
  )




(df_score_conf_lme %>%
  ggplot(aes(x = true_threshold, y = estimate)) +
  geom_point() +
  stat_smooth(method = lm_robust, color = "black") +
  theme_fig +
  xlab("threshold") +
  ylab("coefficient") +
  theme(plot.margin = margin(10, 10, 10, 10))) %>%
  save_svg_figure("p_coeff_true_threshold",
    analysis_group = "coeff_true_threshold",
    scaling = fig_anova_scale,
    width = fig_anova_width,
    height = fig_anova_height,
    unit = "mm"
  )

# output the result of the robust regression of the fig above to a file
df_score_conf_lme %>%
  lm_robust(estimate ~ true_threshold, data = .) %>%
  summary() %>%
  print() %>%
  sink_analysis(
    filename = "lm_robust_coeff_true_threshold.txt",
    analysis_group = "coeff_true_threshold"
  )

# now factors are columns and observations are rows
# transform the data so that the colum is the factor of score and the row is the factor of rule
# the value is mean of correct
# df_rule_hit_for_test %>%
#     summarize_performance(DisplayScore, Correct, TrueRule) %>%
#     select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
#     # pivot_wider(names_from = DisplayScore, values_from = mean_Correct) %>%
#     rio::export(fs::path("anova", "df_rule_hit_for_test_summary",ext = "csv"))

# df_rule_hit_for_test %>%
#     summarize_performance(DisplayScore, Correct, TrueRule) %>%
#     select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
#     pivot_wider(names_from = DisplayScore, values_from = mean_Correct) %>%
#     rio::export(fs::path("anova", "df_rule_hit_for_test_summary_wide",ext = "csv"))

# df_rule_hit_for_test %>%
#     summarize_performance(DisplayScore, Correct, TrueRule) %>%
#     select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>% mutate(PlayerID = as.numeric(as.factor(PlayerID))) %>% pivot_wider(names_from = PlayerID, values_from = mean_Correct, names_prefix = "p") %>%
#     rio::export(fs::path("anova", "df_rule_hit_for_test_summary_player_wide",ext = "csv"))

# df_rule_hit_for_test %>%
#     summarize_performance(DisplayScore, Correct, TrueRule) %>%
#     select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>% mutate(PlayerID = as.numeric(as.factor(PlayerID))) %>%  pivot_wider(names_from = c(TrueRule, DisplayScore), values_from = mean_Correct) %>%
#   rio::export(fs::path("anova", "df_rule_hit_for_test_summary_flatten",ext = "csv"))


## -----------------------------------------------------------------------------
(df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, EstRuleConfidence, DisplayScore) %>%
  summarise(skill_rate = mean(EstRule == "skill"), performance = mean(Correct)) %>%
  ggplot(aes(x = EstRuleConfidence, y = performance, group = PlayerID, color = PlayerID)) +
  geom_point() +
  geom_line() +
  geom_boxplot(aes(group = EstRuleConfidence)) +
  theme_fig +
  facet_wrap(. ~ DisplayScore)) %>%
  save_svg_figure("p_performance_score_conf_rating", analysis_group = "metaconfidence")

(df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, EstRuleConfidence, DisplayScore) %>%
  summarise(skill_rate = mean(EstRule == "skill"), performance = mean(Correct)) %>%
  ggplot(aes(x = EstRuleConfidence, y = skill_rate, group = PlayerID, color = PlayerID)) +
  geom_point() +
  geom_line() +
  geom_boxplot(aes(group = EstRuleConfidence)) +
  theme_fig +
  facet_wrap(. ~ DisplayScore)) %>%
  save_svg_figure("p_choice_score_conf_rating", analysis_group = "metaconfidence")

(df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, EstRuleConfidence, TrueRule) %>%
  summarise(skill_rate = mean(EstRule == "skill"), performance = mean(Correct)) %>%
  ggplot(aes(x = EstRuleConfidence, y = performance, group = PlayerID, color = PlayerID)) +
  geom_point() +
  geom_line() +
  geom_boxplot(aes(group = EstRuleConfidence)) +
  theme_fig +
  labs(subtitle = "true state") +
  facet_wrap(. ~ TrueRule)) %>%
  save_svg_figure("p_performance_truerule_conf_rating", analysis_group = "metaconfidence")

(df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, EstRuleConfidence, EstRule) %>%
  summarise(skill_rate = mean(EstRule == "skill"), performance = mean(Correct)) %>%
  ggplot(aes(x = EstRuleConfidence, y = performance, group = PlayerID, color = PlayerID)) +
  geom_point() +
  geom_line() +
  geom_boxplot(aes(group = EstRuleConfidence)) +
  theme_fig +
  labs(subtitle = "choice") +
  facet_wrap(. ~ EstRule)) %T>%
  save_svg_figure("p_performance_choice_conf_rating", analysis_group = "metaconfidence")

## -----------------------------------------------------------------------------
# x: EstRuleConfidence, y: performance, group: true state
(df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, EstRuleConfidence, TrueRule) %>%
  summarise(skill_rate = mean(EstRule == "skill"), performance = mean(Correct)) %>%
  ggplot(aes(x = EstRuleConfidence, y = performance, group = PlayerID, color = PlayerID)) +
  geom_point() +
  geom_line() +
  geom_boxplot(aes(group = EstRuleConfidence)) +
  theme_fig +
  labs(subtitle = "true state") +
  xlab("confidence rating") +
  ylab("p(correct state inference)") +
  facet_wrap(. ~ TrueRule)) %>%
  save_svg_figure("p_performance_truerule_conf_rating",
    analysis_group = "metaconfidence",
    scaling = fig_anova_scale,
    width = fig_anova_width * 2,
    height = fig_anova_height,
    unit = "mm"
  )


## -----------------------------------------------------------------------------

source(here::here("behaviour", "analysis_scripts", "meta_d_rule.R"))


## -----------------------------------------------------------------------------
source(here::here("behaviour", "analysis_scripts", "confidence_and_performances.R"))


## -----------------------------------------------------------------------------
# plot timeseries of prediction error (mean across players)
(df_score_hit %>%
  group_by(PlayerID) %>%
  mutate(error = abs(EstScore - TrueScore)) %>%
  ungroup() %>%
  group_by(TrialID) %>%
  summarise(
    mean_error = mean(error),
    sd_error = sd(error)
  ) %>%
  ggplot(aes(x = TrialID, y = mean_error, group = 1)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error), alpha = 0.5) +
  theme_fig +
  xlab("trial") +
  ylab("mean prediction error") +
  theme(plot.margin = margin(10, 10, 10, 10))) %>%
  save_svg_figure("score_prediction_error_timeseries",
    analysis_group = "score_prediction",
    scaling = fig_anova_scale,
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    unit = "mm"
  )


# facet_wrap(.~PlayerID) %>%
# save_svg_figure("p_score_prediction_error", analysis_group = "score_prediction")
