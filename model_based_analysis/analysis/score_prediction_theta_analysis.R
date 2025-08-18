library(tidyverse)
library(here)
library(ggthemes)

# 1. Load parameter estimates
param_file <- here("model_based_analysis", "R_result", "binary_sign_model_obj_Distance_random_estimation.rds")
param_df <- rio::import(param_file) %>%
    as_tibble() %>%
    mutate(PlayerID = as.factor(PlayerID))

# 2. Load df_score_hit
score_file <- here("data", "df_score_hit.csv")
df_score_hit <- read_csv(score_file) %>%
    mutate(PlayerID = as.factor(PlayerID))

# 3. Calculate prediction error
df_score_hit_summary <- df_score_hit %>%
    mutate(pred_error = EstScore - TrueScore) %>%
    group_by(PlayerID) %>%
    summarise(
        pred_error = mean(pred_error, na.rm = TRUE),
        true_score = mean(TrueScore, na.rm = TRUE), # true_scoreの平均を計算
        estimated_score = mean(EstScore, na.rm = TRUE), # 推定スコアの平均を計算
        median_true_score = median(TrueScore, na.rm = TRUE) # true_scoreの中央値を計算
    ) %>%
    ungroup() %>%
    inner_join(
        df_rule_hit_performance %>% select(PlayerID, true_threshold),
        by = "PlayerID"
    )

# 4. Merge theta parameters
df_merged <- df_score_hit_summary %>%
    left_join(param_df %>% select(PlayerID, theta = threshold), by = "PlayerID") %>%
    mutate(
        threshold_ratio = theta / true_threshold
    )

# 5. Correlation plot
p_theta_pred_error <- df_merged %>%
    ggplot(aes(x = threshold_ratio, y = pred_error)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "subj. thr. / true thr.", y = "mean score prediction error",
    )

p_theta_pred_error %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "theta_vs_prediction_error.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )

# Show the result of robust regression
summary(lm_robust(pred_error ~ threshold_ratio, data = df_merged))

# correlation between prediction error and true theta (Supplementary Figure 18)
p_theta_true_pred_error <- df_merged %>%
    ggplot(aes(x = true_threshold, y = pred_error)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "true thr.", y = "mean score prediction error"
    )
p_theta_true_pred_error %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "true_theta_vs_prediction_error.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )

# robust regression
summary(lm_robust(pred_error ~ true_threshold, data = df_merged))

# correlation between prediction error and subjective theta
p_theta_pred_error <- df_merged %>%
    ggplot(aes(x = theta, y = pred_error)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "subj. thr.", y = "mean score prediction error"
    )
p_theta_pred_error %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "theta_vs_prediction_error.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )

# robust regression
summary(lm_robust(pred_error ~ theta, data = df_merged))


# correlation between theta and true score itself
p_theta_true_score <- df_merged %>%
    ggplot(aes(x = threshold_ratio, y = true_score)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "subj. thr. / true thr.", y = "true score"
    )
p_theta_true_score %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "theta_vs_true_score.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )
# correlation between theta and true score itself
summary(lm_robust(true_score ~ threshold_ratio, data = df_merged))

# correlation between theta and estimation of  score
p_theta_est_score <- df_merged %>%
    ggplot(aes(x = threshold_ratio, y = estimated_score)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "subj. thr. / true thr.", y = "EstScore"
    )
p_theta_est_score %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "theta_vs_est_score.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )
# correlation between theta and estimation of  score
summary(lm_robust(estimated_score ~ threshold_ratio, data = df_merged))

# correlation between true theta and true score
p_true_theta_true_score <- df_merged %>%
    ggplot(aes(x = true_threshold, y = true_score)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "true theta", y = "true score"
    )

p_true_theta_true_score %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "true_theta_vs_true_score.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )
# correlation between true theta and true score
summary(lm_robust(true_score ~ true_threshold, data = df_merged))

# correlation between true theta and median true score
p_true_theta_median_true_score <- df_merged %>%
    ggplot(aes(x = true_threshold, y = median_true_score)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "true theta", y = "median true score"
    )
p_true_theta_median_true_score %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "true_theta_vs_median_true_score.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )
# correlation between true theta and median true score
summary(lm_robust(median_true_score ~ true_threshold, data = df_merged))


# get median distance of hit location during the score prediction task
df_hit_location_score <-
    read_csv(score_file) %>%
    mutate(PlayerID = as.factor(PlayerID)) %>%
    group_by(PlayerID) %>%
    summarise(
        median_hit_location = median(Distance)
    ) %>%
    ungroup()

# df_score_hit <- read_csv(here::here("data/df_score_hit.csv"))

# df_median <- df_score_hit %>%
#     group_by(PlayerID) %>%
#     summarise(threshold = median(Distance))


# merge with df_merged
df_merged_location <- df_merged %>%
    inner_join(df_hit_location_score, by = "PlayerID")

# correlation between true theta and median hit location
p_true_theta_median_hit_location <- df_merged_location %>%
    ggplot(aes(x = true_threshold, y = median_hit_location)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "true theta", y = "median hit location"
    )
p_true_theta_median_hit_location %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "true_theta_vs_median_hit_location.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )


df_rule_hit %>%
    inner_join(df_median %>% mutate(PlayerID = as.factor(PlayerID), median_threshold = threshold) %>% select(PlayerID, median_threshold), by = "PlayerID") %>%
    filter(Distance < median_threshold) %>%
    filter(TrueRule == "skill") %>%
    select(PlayerID, Distance, TrueRule, DisplayScore, EstRule) %>%
    summary()

(df_rule_hit %>% inner_join(df_median %>% mutate(PlayerID = as.factor(PlayerID), median_threshold = threshold) %>% select(PlayerID, median_threshold), by = "PlayerID") %>%
    select(PlayerID, Distance, TrueRule, DisplayScore, EstRule, median_threshold, true_threshold) %>%
    ggplot(aes(x = median_threshold, y = true_threshold)) +
    geom_point() +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "median threshold", y = "true threshold"
    ) +
    ggtitle("Median threshold vs True threshold") +
    theme(plot.title = element_text(hjust = 0.5))
) %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "median_threshold_vs_true_threshold.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )


# plot relationship between Distance and True Score in df_score_hit
p_distance_true_score <- df_score_hit %>%
    ggplot(aes(x = Distance, y = TrueScore)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "Distance", y = "True Score"
    )
p_distance_true_score %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "distance_vs_true_score.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )


# correlation between true threshold and subjectiv threshold
p_true_threshold_subjective_threshold <- df_merged %>%
    ggplot(aes(x = true_threshold, y = theta)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = lm_robust, se = TRUE, color = "blue") +
    theme_fig +
    labs(
        x = "true threshold", y = "subjective threshold (theta)"
    )
p_true_threshold_subjective_threshold %>%
    save_svg_figure(
        here("model_based_analysis", "figures", "true_threshold_vs_subjective_threshold.svg"),
        width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm", analysis_group = "score_prediction_and_theta"
    )
