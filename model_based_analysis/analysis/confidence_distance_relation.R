# relation between subjective distance and z-scored confidence
# Figure 4C
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

# Plot mean z-scored confidence for each PlayerID against subjective threshold
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
