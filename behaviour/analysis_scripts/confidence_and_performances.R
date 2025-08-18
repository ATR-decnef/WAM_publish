p_number_of_trials_score_rule_correct <-
    df_rule_hit %>%
    group_by(PlayerID, TrueRule, DisplayScore, Correct) %>%
    mutate(
        DisplayScore =
            if_else(DisplayScore > 0, "positive", "negative"),
        Correct = if_else(Correct, "correct", "incorrect")
    ) %>%
    summarise(mean_conf = mean(zConfidence), number_of_trials = n()) %>%
    ggplot(aes(
        x = DisplayScore,
        y = number_of_trials,
        group = interaction(DisplayScore, Correct),
        alpha = DisplayScore,
        fill = TrueRule,
        linetype = Correct
    )) +
    gg_filled_box_sina() +
    facet_wrap(. ~ TrueRule) +
    xlab("score") +
    ylab("number of trials") +
    labs(linetype = "") +
    theme_fig_boxplot +
    guides(
        fill = FALSE,
        alpha = FALSE,
        linetype = guide_legend(override.aes = list(color = "black"))
    )

p_number_of_trials_score_rule_correct %>%
    save_svg_figure(
        "number_of_trials_score_rule_correct_fig",
        analysis_group = "confidence_score_rule_correct",
        width = fig_anova_width * 2,
        height = fig_anova_height,
        scaling = fig_anova_scale,
        unit = "mm"
    )

p_confidence_score_rule_correct <- # Supplementary figure 14D
    df_rule_hit %>%
    group_by(PlayerID, TrueRule, DisplayScore, Correct) %>%
    mutate(
        DisplayScore =
            if_else(DisplayScore > 0, "positive", "negative"),
        Correct = if_else(Correct, "correct", "incorrect")
    ) %>%
    summarise(mean_conf = mean(zConfidence), number_of_trials = n()) %>%
    ggplot(aes(
        x = DisplayScore,
        y = mean_conf,
        group = interaction(DisplayScore, Correct),
        alpha = DisplayScore,
        fill = TrueRule,
        linetype = Correct
    )) +
    gg_filled_box_sina() +
    facet_wrap(. ~ TrueRule) +
    xlab("score") +
    ylab("confidence") +
    labs(linetype = "") +
    theme_fig_boxplot +
    guides(
        fill = FALSE, alpha = FALSE,
        linetype = guide_legend(override.aes = list(color = "black"))
    )

p_confidence_score_rule_correct %>%
    save_svg_figure(
        "confidence_score_rule_correct_fig",
        analysis_group = "confidence_score_rule_correct",
        width = fig_anova_width * 2,
        height = fig_anova_height,
        scaling = fig_anova_scale,
        unit = "mm"
    )
