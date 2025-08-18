# calculate meta-d' for each participant
# nR_skill: number of responses in the skill condition
# nR_random: number of responses in the random condition
# confiddence ratings are from 1 to 4
# data are from df_rule_hit
# if nR_skill is [100, 30, 20, 10, 5, 3, 4, 2], it means
# 100 responded skill with confidence rating 4
# 30 responded skillwith confidence rating 3
# 20 responded skillwith confidence rating 2
# 10 responded skillwith confidence rating 1
# 5 responded random with confidence rating 1
# 3 responded random with confidence rating 2
# 4 responded random with confidence rating 3
# 2 responded random with confidence rating 4
source(here::here("HMeta-d-master", "R", "fit_metad_indiv.R"))

df_rule_hit_meta_d_response_list <- df_rule_hit %>% # extract the list of responses for each participant
    group_by(PlayerID, TrueRule, EstRule, EstRuleConfidence) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    tidyr::complete(PlayerID, TrueRule, EstRule, EstRuleConfidence, fill = list(count = 0)) %>%
    group_by(PlayerID, TrueRule) %>%
    nest() %>%
    mutate(t = map(data, ~ .x %>%
        group_by(EstRule, EstRuleConfidence) %>%
        summarise(value = list(count), .groups = "drop") %>%
        group_by(EstRule) %>%
        nest(.key = "sub_list") %>%
        mutate(sub_list = map(sub_list, ~ setNames(.x$value, .x$EstRuleConfidence))) %>%
        deframe())) %>%
    select(PlayerID, TrueRule, t) %>%
    pivot_wider(names_from = TrueRule, values_from = t) %>%
    mutate(
        nR_skill = # extract the response list when the true rule is skill
            map(
                skill,
                function(skill) {
                    c(
                        skill[["skill"]][["4"]],
                        skill[["skill"]][["3"]],
                        skill[["skill"]][["2"]],
                        skill[["skill"]][["1"]],
                        skill[["random"]][["1"]],
                        skill[["random"]][["2"]],
                        skill[["random"]][["3"]],
                        skill[["random"]][["4"]]
                    )
                }
            ),
        nR_random = # extract the response list when the true rule is random
            map(random, function(random) {
                c(
                    random[["skill"]][["4"]],
                    random[["skill"]][["3"]],
                    random[["skill"]][["2"]],
                    random[["skill"]][["1"]],
                    random[["random"]][["1"]],
                    random[["random"]][["2"]],
                    random[["random"]][["3"]],
                    random[["random"]][["4"]]
                )
            })
    )

df_rule_hit_meta_d <-
    df_rule_hit_meta_d_response_list %>%
    mutate(meta_res = map2(nR_skill, nR_random, ~ fit_metad_indiv(.x, .y)))

df_rule_hit_meta_d_performance <- df_rule_hit_meta_d %>% # calculate meta d' for each participant
    select(PlayerID, meta_res) %>%
    mutate(output = map(meta_res, ~ .x[[1]] %>%
        ggmcmc::ggs() %>%
        filter(Parameter == "meta_d") %>%
        pull(value) %>%
        mean() %>%
        unlist())) %>%
    unnest(cols = c(output)) %>%
    inner_join(df_rule_hit_performance)

df_rule_hit_meta_d_true_rule <-
    df_rule_hit_meta_d_response_list %>%
    mutate(
        zero = map(PlayerID, ~ c(0, 0, 0, 0, 0, 0, 0, 0) %>% as.integer()),
        meta_res_skill = map2(nR_skill, zero, ~ fit_metad_indiv(.x, .y)),
        meta_res_random = map2(zero, nR_random, ~ fit_metad_indiv(.x, .y))
    )

df_rule_hit_meta_d_true_rule_performance <-
    df_rule_hit_meta_d_true_rule %>%
    select(PlayerID, meta_res_skill, meta_res_random) %>%
    mutate(
        skill = map(meta_res_skill, ~ .x[[1]] %>%
            ggmcmc::ggs() %>%
            filter(Parameter == "meta_d") %>%
            pull(value) %>%
            mean() %>%
            unlist()),
        random = map(meta_res_random, ~ .x[[1]] %>%
            ggmcmc::ggs() %>%
            filter(Parameter == "meta_d") %>%
            pull(value) %>%
            mean() %>%
            unlist())
    ) %>%
    unnest(cols = c(skill, random)) %>%
    inner_join(df_rule_hit_performance)

# Suppelementary Figure 14A
(df_rule_hit_meta_d_true_rule_performance %>%
    pivot_longer(names_to = "TrueRule", values_to = "output", cols = c(skill, random)) %>%
    ggplot(aes(x = TrueRule, y = output, group = TrueRule, color = TrueRule, alpha = TrueRule)) +
    geom_boxplot(outlier.shape = NA) +
    geom_sina_fitted() +
    xlab("task state") +
    ylab("meta-d'") +
    guides(colour = FALSE) +
    theme_fig_boxplot
) %>%
    save_svg_figure("metad_state",
        analysis_group = "metacognition",
        scaling = fig_anova_scale,
        width = fig_2box_width,
        height = fig_2box_height,
        unit = "mm"
    )

# wilcoxon signed rank test
df_rule_hit_meta_d_true_rule_performance %>%
    pivot_longer(names_to = "TrueRule", values_to = "output", cols = c(skill, random)) %>%
    mutate(meta_d = output) %>%
    posthoc_wilcox_test(meta_d ~ TrueRule, fixed_factor = NULL) %>%
    output_posthoc_result("posthoc_wilcox_test_metad_truerule",
        analysis_group = "metacognition"
    )


df_rule_hit_meta_d_response_list_score <- df_rule_hit %>% # extract the list of responses for each score and participant
    group_by(PlayerID, TrueRule, EstRule, EstRuleConfidence, DisplayScore) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    tidyr::complete(PlayerID, TrueRule, EstRule, EstRuleConfidence, DisplayScore, fill = list(count = 0)) %>%
    group_by(PlayerID, TrueRule, DisplayScore) %>%
    nest() %>%
    mutate(t = map(data, ~ .x %>%
        group_by(EstRule, EstRuleConfidence) %>%
        summarise(value = list(count), .groups = "drop") %>%
        group_by(EstRule) %>%
        nest(.key = "sub_list") %>%
        mutate(sub_list = map(sub_list, ~ setNames(.x$value, .x$EstRuleConfidence))) %>%
        deframe())) %>%
    select(PlayerID, TrueRule, DisplayScore, t) %>%
    pivot_wider(names_from = TrueRule, values_from = t) %>%
    mutate(
        nR_skill = # extract the response list when the true rule is skill
            map(
                skill,
                function(skill) {
                    c(
                        skill[["skill"]][["4"]],
                        skill[["skill"]][["3"]],
                        skill[["skill"]][["2"]],
                        skill[["skill"]][["1"]],
                        skill[["random"]][["1"]],
                        skill[["random"]][["2"]],
                        skill[["random"]][["3"]],
                        skill[["random"]][["4"]]
                    )
                }
            ),
        nR_random = # extract the response list when the true rule is random
            map(random, function(random) {
                c(
                    random[["skill"]][["4"]],
                    random[["skill"]][["3"]],
                    random[["skill"]][["2"]],
                    random[["skill"]][["1"]],
                    random[["random"]][["1"]],
                    random[["random"]][["2"]],
                    random[["random"]][["3"]],
                    random[["random"]][["4"]]
                )
            })
    )

df_rule_hit_meta_d_score <-
    df_rule_hit_meta_d_response_list_score %>%
    mutate(meta_res = map2(
        nR_skill, nR_random,
        ~ fit_metad_indiv(.x, .y)
    ))

df_rule_hit_meta_d_performance_score <-
    df_rule_hit_meta_d_score %>% # calculate meta d' for each score and participant
    select(PlayerID, meta_res, DisplayScore) %>%
    mutate(output = map(meta_res, ~ .x[[1]] %>%
        ggmcmc::ggs() %>%
        filter(Parameter == "meta_d") %>%
        pull(value) %>%
        mean() %>%
        unlist())) %>%
    unnest(cols = c(output)) %>%
    inner_join(df_rule_hit_performance)
df_rule_hit_meta_d_performance_score <-
    df_rule_hit_meta_d_performance_score %>%
    mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative"))

df_auc <-
    df_rule_hit %>%
    mutate(DisplayScore = if_else(DisplayScore > 0,
        "positive", "negative"
    )) %>%
    select(
        PlayerID,
        TrialID,
        TrueRule,
        EstRule,
        EstRuleConfidence,
        Correct
    ) %>%
    group_by(PlayerID) %>%
    summarise(
        auc = pROC::roc(Correct ~ EstRuleConfidence)$auc %>%
            as.numeric()
    )

df_auc_score <-
    df_rule_hit %>%
    mutate(DisplayScore = if_else(DisplayScore > 0,
        "positive", "negative"
    )) %>%
    select(
        PlayerID,
        TrialID,
        TrueRule,
        EstRule,
        DisplayScore,
        EstRuleConfidence,
        Correct
    ) %>%
    group_by(PlayerID, DisplayScore) %>%
    summarise(
        auc = pROC::roc(Correct ~ EstRuleConfidence)$auc %>%
            as.numeric()
    )

df_auc_true_rule <-
    df_rule_hit %>%
    mutate(DisplayScore = if_else(DisplayScore > 0,
        "positive", "negative"
    )) %>%
    select(
        PlayerID,
        TrialID,
        TrueRule,
        EstRule,
        EstRuleConfidence,
        Correct
    ) %>%
    group_by(PlayerID, TrueRule) %>%
    summarise(
        auc = pROC::roc(Correct ~ EstRuleConfidence)$auc %>%
            as.numeric()
    )

# Supplementary Figure 14B
(df_rule_hit_meta_d_performance_score %>%
    ggplot(aes(
        x = DisplayScore,
        y = output,
        group = DisplayScore,
        alpha = DisplayScore,
        color = "black",
        fill = "black"
    )) +
    gg_filled_box_sina() +
    theme_fig_boxplot +
    theme(legend.position = "none") +
    xlab("score") +
    ylab("meta-d'")
) %>%
    save_svg_figure("metad_score",
        analysis_group = "metacognition",
        scaling = fig_anova_scale,
        width = fig_2box_width,
        height = fig_2box_height,
        unit = "mm"
    )

df_rule_hit_meta_d_performance_score %>%
    mutate(meta_d = output) %>%
    posthoc_wilcox_test(meta_d ~ DisplayScore, fixed_factor = NULL) %>%
    output_posthoc_result("posthoc_wilcox_test_metad_score",
        analysis_group = "metacognition"
    )

(df_auc %>%
    inner_join(df_rule_hit_meta_d_performance) %>%
    ggplot(aes(x = output, y = auc)) +
    geom_point() +
    xlab("meta-d'") +
    theme_fig) %>%
    save_svg_figure(
        "auc_meta-d_fig",
        analysis_group = "metacognition",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )

(df_auc_score %>%
    inner_join(df_rule_hit_meta_d_performance_score) %>%
    ggplot(aes(x = output, y = auc, group = DisplayScore, color = DisplayScore)) +
    geom_point() +
    xlab("meta-d'") +
    labs(colour = "score") +
    theme_fig) %>%
    save_svg_figure(
        "auc_meta-d_score_fig",
        analysis_group = "metacognition",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )

(df_auc_true_rule %>%
    inner_join(df_rule_hit_meta_d_true_rule_performance %>%
        pivot_longer(
            names_to = "TrueRule",
            values_to = "output",
            cols = c(skill, random)
        )) %>%
    ggplot(aes(x = output, y = auc, group = TrueRule, color = TrueRule)) +
    geom_point() +
    theme_fig +
    xlab("meta-d'") +
    labs(colour = "task state")
) %>%
    save_svg_figure(
        "auc_meta-d_truerule_fig",
        analysis_group = "metacognition",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )

# Supplementary figure 14C
df_auc_true_rule_score <-
    df_rule_hit %>%
    mutate(DisplayScore = if_else(DisplayScore > 0,
        "positive", "negative"
    )) %>%
    select(
        PlayerID,
        TrialID,
        TrueRule,
        DisplayScore,
        EstRule,
        EstRuleConfidence,
        Correct
    ) %>%
    group_by(PlayerID, TrueRule, DisplayScore) %>%
    summarise(
        auc = pROC::roc(Correct ~ EstRuleConfidence)$auc %>%
            as.numeric()
    )

# plot the AUC for each task state and score
df_auc_true_rule_score %>%
    anovakun("sAB", long = TRUE, gg = TRUE) %>%
    sink_analysis(
        "auc_truerule_score_anova",
        analysis_group = "AUC",
    )

# df_auc_true_rule_score %>%
#     posthoc_wilcox_test2(auc, TrueRule, DisplayScore) %>%

(df_auc_true_rule_score %>% ####
    ggplot(aes(
        x = TrueRule,
        y = auc, group = interaction(TrueRule, DisplayScore),
        alpha = DisplayScore,
        fill = TrueRule
    )) +
    gg_filled_box_sina() +
    xlab("task state") +
    ylab("AUC") +
    labs(alpha = "score") +
    guides(fill = FALSE) +
    guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
    theme_fig_boxplot) %>%
    save_svg_figure(
        "auc_truerule_score_fig",
        analysis_group = "AUC",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )

# plot AUC for only score
(df_auc_true_rule_score %>%
    ggplot(aes(
        x = DisplayScore,
        y = auc,
        group = DisplayScore,
        alpha = DisplayScore,
        fill = "black"
    )) +
    gg_filled_box_sina() +
    xlab("score") +
    ylab("AUC") +
    labs(alpha = "task state") +
    guides(fill = FALSE) +
    guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
    theme_fig_boxplot) %>%
    save_svg_figure(
        "auc_score_fig",
        analysis_group = "AUC",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )

# plot AUC for only task state
(df_auc_true_rule_score %>%
    ggplot(aes(
        x = TrueRule,
        y = auc,
        group = TrueRule,
        colour = TrueRule
    )) +
    geom_boxplot(outlier.shape = NA) +
    geom_sina_fitted() +
    xlab("task state") +
    ylab("AUC") +
    guides(colour = FALSE) +
    guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black")))) +
    theme_fig_boxplot) %>%
    save_svg_figure(
        "auc_truerule_fig",
        analysis_group = "AUC",
        scaling = fig_anova_scale,
        width = fig_anova_width,
        height = fig_anova_height,
        unit = "mm"
    )
