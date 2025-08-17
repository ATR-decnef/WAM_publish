filter_IQR <- function(vec_data) {
    Q1 <- quantile(vec_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(vec_data, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value

    # return true if vec_data is within the IQR bounds
    return(vec_data >= lower_bound & vec_data <= upper_bound)
}

scale_transformation <- function(vec_data) {
    sigmoided_data <- 1 / (1 + exp(-(vec_data - 1)))
    scaled_sigmoided_data <- sigmoided_data * 2 - 1
    return(scaled_sigmoided_data)
}

# 正規化distanceとzスコア化confidenceの関係プロット
p_conf_distance <- df_rule_hit %>%
    ggplot(aes(x = normalizedDistance, y = zConfidence)) +
    # geom_point(alpha = 0.3) +
    geom_smooth(color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
    theme_fig +
    xlab("distance") +
    ylab("z-scored confidence") +
    scale_x_log10()

p_conf_distance %>% save_svg_figure("normalized_distance_vs_zscored_confidence",
    analysis_group = "confidence_distance_relation",
    width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm"
)

p_conf_subjective_distance <- df_rule_hit %>%
    select(-threshold) %>%
    select(PlayerID, TrialID, Distance, normalizedDistance, zConfidence) %>%
    inner_join(
        import(
            here::here(
                "model_based_analysis",
                "R_result",
                "binary_sign_model_obj_Distance_random_estimation.rds"
            )
        ) %>%
            mutate(PlayerID = as.factor(PlayerID)) %>%
            select(PlayerID, threshold),
        by = "PlayerID"
    ) %>%
    # filter(threshold > 10) %>%
    # filter(filter_IQR(threshold)) %>%
    mutate(
        subjectiveDistance = Distance / threshold
    ) %>%
    ggplot(aes(x = subjectiveDistance, y = zConfidence)) +
    # geom_point(alpha = 0.3) +
    geom_smooth(color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
    annotate("text",
        x = 1.2, y = 0.35,
        label = "subjective\nthreshold", hjust = 0, vjust = 1, size = 4
    ) +
    theme_fig +
    xlab("distance normalised to the subjective threshold\n(log scale)") +
    ylab("confidence\n(z-scored)") +
    scale_x_log10() +
    theme(
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
    ) +
    theme(plot.margin = margin(0, 20, 0, 5))

p_conf_subjective_distance %>% save_svg_figure("subjective_distance_vs_zscored_confidence",
    analysis_group = "confidence_distance_relation",
    width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm"
)

p_conf_subjective_distance_scaled <- df_rule_hit %>%
    select(-threshold) %>%
    select(PlayerID, TrialID, Distance, normalizedDistance, zConfidence) %>%
    inner_join(
        import(
            here::here(
                "model_based_analysis",
                "R_result",
                "binary_sign_model_obj_Distance_random_estimation.rds"
            )
        ) %>%
            mutate(PlayerID = as.factor(PlayerID)) %>%
            select(PlayerID, threshold),
        by = "PlayerID"
    ) %>%
    mutate(
        subjectiveDistance = Distance / threshold,
        subjectiveDistanceScaled = scale_transformation(Distance / threshold)
    ) %>%
    ggplot(aes(x = subjectiveDistanceScaled, y = zConfidence)) +
    # geom_point(alpha = 0.3) +
    geom_smooth(color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_fig +
    xlab("scaled subjective distance") +
    ylab("z-scored confidence")

p_conf_subjective_distance_scaled %>% save_svg_figure("scaled_subjective_distance_vs_zscored_confidence",
    analysis_group = "confidence_distance_relation",
    width = fig_anova_width, height = fig_anova_height, scaling = fig_anova_scale, unit = "mm"
)


# PlayerIDごとに主観的thresholdとzスコア化confidenceの平均を紐付けてプロット
df_subjective_threshold <- df_MLE_result_binary %>%
    select(PlayerID, threshold)

df_confidence <- df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(zConfidence = mean(, na.rm = TRUE))

df_thr_conf <- inner_join(df_subjective_threshold, df_confidence, by = "PlayerID")

p_thr_conf <- ggplot(df_thr_conf, aes(x = subjective_threshold, y = zConfidence)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "black") +
    theme_fig_base +
    xlab("Subjective threshold") +
    ylab("Mean z-scored confidence") +
    ggtitle("Relationship between subjective threshold and z-scored confidence")

p_thr_conf %>% save_svg_figure("subjective_threshold_vs_zscored_confidence",
    analysis_group = "parameter_analysis",
    width = fig_2box_width, height = fig_2box_height, scaling = fig_anova_scale, unit = "mm"
)


threshold_df <- import(
    here::here(
        "model_based_analysis",
        "R_result",
        "binary_sign_model_obj_Distance_random_estimation.rds"
    )
) %>%
    mutate(PlayerID = as.factor(PlayerID)) %>%
    select(PlayerID, threshold)

# IQRと外れ値範囲の計算
Q1 <- quantile(threshold_df$threshold, 0.25, na.rm = TRUE)
Q3 <- quantile(threshold_df$threshold, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# 外れ値の抽出
outliers <- threshold_df %>%
    filter(threshold < lower_bound | threshold > upper_bound)

# 外れ値を出力
print("外れ値 (PlayerID, threshold):")
print(outliers)
