library(tidyverse)
library(rio)
library(ggplot2)
library(ggthemes)
library(ggforce)
library(ggpubr)
library(patchwork)
library(ggsignif)

source(here::here("model_based_analysis", "model", "model_prediction.R"))

# function definitions ----
simulate_model <- function(
    # simulate the model with given parameters
    df_behaviour,
    df_est_params, model,
    input = "Distance",
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`) {
  # simulate the model with the given parameters
  df_sim <- predict_from_model(df_est_params, model, df_behaviour, input) %>%
    mutate( # todo: standardize the sanitization below across all scripts
      input = "Distance",
      score = if_else({{ DisplayScore }} > 0, "positive", "negative"),
      DisplayScore = score,
      PlayerID = as.factor(PlayerID),
      Correct = if_else(TrueRule == "random", pred, 1 - pred),
      output = pred > 0.5,
      pred_adj = if_else(abs(pred) < 1, pred, pred - 10^(-5)),
      entropy = (pred_adj * log(pred_adj) + (1 - pred_adj) * log(1 - pred_adj))
    ) %>%
    group_by(input, PlayerID) %>%
    mutate(z_entropy = scale(entropy)) %>%
    ungroup()
  return(df_sim)
}

extract_hidden_variables_from_sim <- function(df_sim) {
  df_sim %>%
    unnest(cols = c(data)) %>%
    mutate(
      biases = calc_bias(
        b_G = b_G,
        b_B = b_B,
        score = DisplayScore
      ),
      Z_bias = calc_Z_bias_from_p(pred),
      Z = calc_Z_from_Z_bias(Z_bias, biases)
    )
}

format_df_seq <- function(
    df,
    target,
    summary_function = mean,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    input = input) {
  df %>%
    # select(-(Distance)) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise({{ target }} := summary_function({{ target }})) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, {{ target }}) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    )
}

list_combination_of_parameters <- function(df_est_params) {
  cond_is_true_theta <- c(TRUE, FALSE)
  cond_is_alpha_symmetric <- c(TRUE, FALSE)
  cond_is_beta_symmetric <- c(TRUE, FALSE)

  # iterate all the combinations of the parameters
  # if theta is true, then threshold = true_threshold
  # if alpha is symmetric, then a_G = a_B = mean(a_G, a_B)
  # if beta is symmetric, then b_G = b_B = mean(b_G, b_B)
  df_est_params %>%
    inner_join(
      crossing(
        is_true_theta = cond_is_true_theta,
        is_alpha_symmetric = cond_is_alpha_symmetric,
        is_beta_symmetric = cond_is_beta_symmetric,
        PlayerID = df_est_params %>%
          pull(PlayerID) %>%
          unique()
      )
    ) %>%
    mutate(
      threshold = if_else(
        is_true_theta,
        true_threshold,
        threshold
      ),
      a_G = if_else(
        is_alpha_symmetric,
        map2_dbl(a_G, a_B, ~ mean(c(.x, .y))),
        a_G
      ),
      a_B = if_else(
        is_alpha_symmetric,
        a_G,
        a_B
      ),
      b_G = if_else(
        is_beta_symmetric,
        map2_dbl(b_G, b_B, ~ mean(c(.x, .y))),
        b_G
      ),
      b_B = if_else(
        is_beta_symmetric,
        b_G,
        b_B
      )
    )
}


# function for .combine option in foreach
func_combine <- function(a, b) {
  # extract list of keys and list of values from a
  keys_a <- names(a)
  values_a <- a
  # extract list of keys and list of values from b
  keys_b <- names(b)
  values_b <- b
  # combine the keys
  keys <- c(keys_a, keys_b)
  # combine the values
  values <- c(values_a, values_b)
  # return the combined list
  setNames(values, keys)
}

# plot functions ----
plot_acc_seq <- function(
    # plot the accuracy sequence
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    Correct = Correct,
    input = input) {
  df %>%
    # select(-Distance) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise(
      performance = mean({{ Correct }})
    ) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, performance) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    ) %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = performance, linetype = switch)) +
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
    coord_cartesian(xlim = c(-3, 8), ylim = c(0, 1)) +
    xlab("trials after the switch") +
    ylab("p(correct state inference)") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_conf_seq <- function(
    # plot the confidence sequence
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = z_entropy,
    input = input) {
  df %>%
    # select(-(Distance)) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise(z_entropy = mean({{ confidence }})) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, z_entropy) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    ) %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = z_entropy, linetype = switch)) +
    stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
    # stat_summary(
    #   aes(
    #     group = interaction(PlayerID, model, switch),
    #     alpha = 0.2
    #   ),
    #   alpha = 0.2,
    #   fun = mean, geom = "line", position = position_dodge(width = 0.2)
    # ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 1.0,
      size = 0.8,
      alpha = 0.8,
      position = position_dodge(width = 0.2)
    ) +
    geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray") +
    coord_cartesian(xlim = c(-3, 8), ylim = c(-1, 1)) +
    xlab("trials after the switch") +
    ylab("z-scored negative entropy") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_raw_negative_entropy_seq <- function(
    # plot the confidence sequence
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  df %>%
    # select(-(Distance)) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise(entropy = mean({{ confidence }})) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, entropy) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    ) %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = entropy, color = model, linetype = switch)) +
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
    coord_cartesian(xlim = c(-3, 8), ylim = c(-0.5, 0)) +
    xlab("trials after the switch") +
    ylab("raw negative entropy") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_random_seq <- function(
    # plot the confidence sequence
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  df %>%
    # select(-(Distance)) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise(random_choice = mean(prob)) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, random_choice) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    ) %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = random_choice, color = model, linetype = switch)) +
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
    coord_cartesian(xlim = c(-3, 8), ylim = c(0, 1)) +
    xlab("trials after the switch") +
    ylab("random choice probability") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_acc_variance_seq <- function(
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  # calculate variance of pred within input and PlayerID
  df %>%
    # select(-(Distance)) %>%
    pivot_longer(
      cols =
        c({{ TrialsAfterSwitchToSkill }}, {{ TrialsAfterSwitchToRandom }}),
      names_to = "switch",
      values_to = "num of trials"
    ) %>%
    mutate(
      {{ input }} := if_else({{ input }} == "Distance", "binary model", {{ input }})
    ) %>%
    pivot_wider(names_from = {{ input }}, values_from = pred) %>%
    pivot_longer(
      cols = c(`binary model`),
      names_to = "model",
      values_to = "prob"
    ) %>%
    group_by(PlayerID, model, switch, {{ num_of_trials }}) %>%
    summarise(variance = var(prob)) %>%
    dplyr::select(PlayerID, model, switch, {{ num_of_trials }}, variance) %>%
    mutate(
      switch = if_else(switch == "TrialsAfterSwitchToRandom",
        "skill to random",
        "random to skill"
      )
    ) %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = variance, color = model, linetype = switch)) +
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
    coord_cartesian(xlim = c(-3, 8), ylim = c(0, 0.25)) +
    xlab("trials after the switch") +
    ylab("variance of prediction") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_acc_anova <- function(
    df,
    TrueRule = TrueRule,
    Correct = Correct,
    DisplayScore = DisplayScore) {
  # extract the name of the variable bound to Correct
  Correct <- enquo(Correct)
  name_mean_Correct <- paste0("mean_", quo_name(Correct))

  # name_mean_Correct as a symbol
  sym_mean_Correct <- sym(name_mean_Correct)

  df %>%
    summarize_performance({{ TrueRule }}, {{ Correct }}, {{ DisplayScore }}) %>%
    mutate(condition = interaction(
      {{ TrueRule }},
      {{ DisplayScore }}
    ) %>% as.character()) %>%
    ggplot(
      aes(
        x = {{ TrueRule }},
        y = !!sym_mean_Correct,
        group = interaction({{ TrueRule }}, {{ DisplayScore }}),
        fill = {{ TrueRule }},
        alpha = {{ DisplayScore }}
      )
    ) +
    gg_filled_box_sina() +
    guides(
      alpha =
        guide_legend(
          override.aes =
            list(color = "black", fill = c("black"))
        ), fill = "none"
    ) +
    # facet_wrap(.~input) +
    labs(alpha = "score") +
    xlab("task state") +
    ylab("p(correcte state inference)") +
    theme_fig_anova +
    theme(legend.position = "right") +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_conf_anova <- function(
    df,
    TrueRule = TrueRule,
    conf = z_entropy,
    DisplayScore = DisplayScore) {
  # extract the name of the variable bound to conf
  conf <- enquo(conf)
  name_mean_conf <- paste0("mean_", quo_name(conf))

  # name_mean_conf as a symbol
  sym_mean_conf <- sym(name_mean_conf)

  df %>%
    summarize_performance(
      {{ TrueRule }}, {{ conf }}, {{ DisplayScore }}
    ) %>%
    mutate(
      condition = interaction(
        {{ TrueRule }},
        {{ DisplayScore }}
      ) %>% as.character()
    ) %>%
    ggplot(aes(
      x = {{ TrueRule }},
      y = !!sym_mean_conf,
      group = interaction({{ TrueRule }}, {{ DisplayScore }}),
      fill = {{ TrueRule }},
      alpha = {{ DisplayScore }}
    )) +
    gg_filled_box_sina() +
    guides(
      alpha =
        guide_legend(
          override.aes = list(color = "black", fill = c("black"))
        ), fill = "none"
    ) +
    coord_cartesian(ylim = c(-1, 1)) +
    # facet_wrap(.~input) +
    labs(alpha = "score") +
    xlab("task state") +
    ylab("z-scored negative entropy") +
    theme_fig_anova +
    theme(legend.position = "right") +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_acc_anova_no_sina <- function(
    df,
    TrueRule = TrueRule,
    Correct = Correct,
    DisplayScore = DisplayScore) {
  # extract the name of the variable bound to Correct
  Correct <- enquo(Correct)
  name_mean_Correct <- paste0("mean_", quo_name(Correct))

  # name_mean_Correct as a symbol
  sym_mean_Correct <- sym(name_mean_Correct)

  df %>%
    summarize_performance({{ TrueRule }}, {{ Correct }}, {{ DisplayScore }}) %>%
    mutate(condition = interaction(
      {{ TrueRule }},
      {{ DisplayScore }}
    ) %>% as.character()) %>%
    ggplot(
      aes(
        x = {{ TrueRule }},
        y = !!sym_mean_Correct,
        group = interaction({{ TrueRule }}, {{ DisplayScore }}),
        fill = {{ TrueRule }},
        alpha = {{ DisplayScore }}
      )
    ) +
    gg_filled_box() +
    guides(
      alpha =
        guide_legend(
          override.aes =
            list(color = "black", fill = c("black"))
        ), fill = "none"
    ) +
    # facet_wrap(.~input) +
    labs(alpha = "score") +
    xlab("task state") +
    ylab("p(correcte state inference)") +
    theme_fig_anova +
    theme(legend.position = "right") +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_conf_anova_no_sina <- function(
    df,
    TrueRule = TrueRule,
    conf = z_entropy,
    DisplayScore = DisplayScore) {
  # extract the name of the variable bound to conf
  conf <- enquo(conf)
  name_mean_conf <- paste0("mean_", quo_name(conf))

  # name_mean_conf as a symbol
  sym_mean_conf <- sym(name_mean_conf)

  df %>%
    summarize_performance(
      {{ TrueRule }}, {{ conf }}, {{ DisplayScore }}
    ) %>%
    mutate(
      condition = interaction(
        {{ TrueRule }},
        {{ DisplayScore }}
      ) %>% as.character()
    ) %>%
    ggplot(aes(
      x = {{ TrueRule }},
      y = !!sym_mean_conf,
      group = interaction({{ TrueRule }}, {{ DisplayScore }}),
      fill = {{ TrueRule }},
      alpha = {{ DisplayScore }}
    )) +
    gg_filled_box() +
    guides(
      alpha =
        guide_legend(
          override.aes = list(color = "black", fill = c("black"))
        ), fill = "none"
    ) +
    coord_cartesian(ylim = c(-1, 1)) +
    # facet_wrap(.~input) +
    labs(alpha = "score") +
    xlab("task state") +
    ylab("z-scored negative entropy") +
    theme_fig_anova +
    theme(legend.position = "right") +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}


plot_Z_time_series_one_sub <- function(df_sim, id) {
  df_sim %>%
    extract_hidden_variables_from_sim() %>%
    filter(PlayerID == id) %>%
    ggplot(aes(x = TrialID, y = Z_bias)) +
    geom_line() +
    xlab("trials") +
    ylab("Z + bias")
}

plot_Z_bias_seq <- function(
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  df %>%
    format_df_seq(Z_bias) %>%
    ggplot(aes(
      x = {{ num_of_trials }} + 1,
      y = Z_bias, linetype = switch
    )) +
    stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 1.0,
      size = 0.8,
      alpha = 0.8,
      position = position_dodge(width = 0.2)
    ) +
    coord_cartesian(xlim = c(-3, 8)) +
    xlab("trials after the switch") +
    ylab("z + bias") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

## plot delta Z = {z + bias}(t) - {z + bias}(t-1) sequence ====
plot_diff_z_bias_time_series <- function(
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  df %>%
    format_df_seq(Z_bias) %>%
    group_by(PlayerID, model, switch) %>%
    mutate(diff_Z_bias = Z_bias - lag(Z_bias)) %>%
    ungroup() %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = diff_Z_bias, linetype = switch)) +
    stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 1.0,
      size = 0.8,
      alpha = 0.8,
      position = position_dodge(width = 0.2)
    ) +
    coord_cartesian(xlim = c(-3, 8)) +
    xlab("trials after the switch") +
    ylab("delta z + bias") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_abs_diff_z_bias_time_series <- function(
    df,
    TrialsAfterSwitchToSkill = TrialsAfterSwitchToSkill,
    TrialsAfterSwitchToRandom = TrialsAfterSwitchToRandom,
    DisplayScore = DisplayScore,
    num_of_trials = `num of trials`,
    TrueRule = TrueRule,
    confidence = entropy,
    input = input) {
  df %>%
    format_df_seq(Z_bias) %>%
    group_by(PlayerID, model, switch) %>%
    mutate(diff_Z_bias = Z_bias - lag(Z_bias)) %>%
    ungroup() %>%
    ggplot(aes(x = {{ num_of_trials }} + 1, y = abs(diff_Z_bias), linetype = switch)) +
    stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 1.0,
      size = 0.8,
      alpha = 0.8,
      position = position_dodge(width = 0.2)
    ) +
    coord_cartesian(xlim = c(-3, 8)) +
    xlab("trials after the switch") +
    ylab("abs(delta z + bias)") +
    theme_fig_timeseries +
    theme(axis.title.y = element_text(size = 32 / fig_anova_scale))
}

plot_diff_z_bias_time_series_one_sub <- function(df_sim, id) {
  df_sim %>%
    extract_hidden_variables_from_sim() %>%
    filter(PlayerID == id) %>%
    mutate(diff_Z_bias = Z_bias - lag(Z_bias)) %>%
    ggplot(aes(x = TrialID, y = diff_Z_bias)) +
    geom_line() +
    xlab("trials") +
    ylab("delta Z + bias")
}

# load the original parameters ====
filename <-
  here::here(
    "model_based_analysis",
    "R_result",
    "binary_sign_model_obj_Distance_random_estimation.rds")

df_est_params <- import(filename) %>%
  as_tibble() %>%
  mutate(PlayerID = PlayerID %>% as.factor()) %>%
  inner_join(df_rule_hit_performance %>%
    select(PlayerID, true_threshold), by = "PlayerID") %>%
  mutate(threshold_ratio = threshold / true_threshold)

# df_est_params_symmetric_model <-
#   import(here::here("model_based_analysis", "R_result/binary_sign_common_alpha_common_beta_model_obj_Distance_random_estimation.rds")) %>%
#   as_tibble() %>%
#   mutate(PlayerID = PlayerID %>% as.factor()) %>%
#   inner_join(df_rule_hit_performance %>%
#     select(PlayerID, true_threshold), by = "PlayerID") %>%
#   mutate(threshold_ratio = threshold / true_threshold)

# input_list <- filename %>%
#   `[`(str_detect(., pattern = ".*obj.*estimation.rds")) %>%
#   str_extract(pattern = "(?<=_obj_).*(?=_estimation)") %>%
#   str_match(pattern = "^.*(?=_)")

# df_sim_with_symmetric_model_actual_parameters <- simulate_model(
#   df_rule_hit, df_est_params_symmetric_model %>%
#     select(-c(true_threshold, threshold_ratio)),
#   binary_sign_common_alpha_common_beta_model_obj, "Distance"
# )
# df_sim_with_actual_parameters <- simulate_model(
#   df_rule_hit, df_est_params %>%
#     select(-c(true_threshold, threshold_ratio)),
#   binary_sign_model_obj, "Distance"
# )

## modify the paramter and the threshold is now same as the true threshold ====
df_est_params_true_threshold <-
  df_est_params %>%
  mutate(threshold = true_threshold)

## simulate the model with the true threshold ====
df_sim_with_true_threshold <- simulate_model(
  df_rule_hit, df_est_params_true_threshold %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

## simulate the model with the parameters which a_G and a_B, and b_G and b_B are flipped ====
df_est_params_flip <- df_est_params %>%
  mutate(
    a_G_tmp = a_G, a_G = a_B, a_B = a_G_tmp,
    b_G_tmp = b_G, b_G = b_B, b_B = b_G_tmp
  ) %>%
  select(-c(a_G_tmp, b_G_tmp))

df_sim_with_flipped_parameters <- simulate_model(
  df_rule_hit, df_est_params_flip %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

## simulate the model with the parameters which a_G and a_B, and b_G and b_B are flipped, and the threshold is now same as the true threshold ====
df_est_params_flip_true_threshold <-
  df_est_params_flip %>% mutate(threshold = true_threshold)

df_sim_with_flipped_parameters_and_true_threshold <- simulate_model(
  df_rule_hit, df_est_params_flip_true_threshold %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

## simulate the model with the paraeters which a_G = a_B, and b_G = b_B ====
df_est_params_mean <-
  df_est_params %>%
  mutate(
    a_G = map2_dbl(a_G, a_B, ~ mean(c(.x, .y))),
    a_B = a_G,
    b_G = map2_dbl(b_G, b_B, ~ mean(c(.x, .y))),
    b_B = b_G
  )

df_sim_with_mean_parameters <- simulate_model(
  df_rule_hit, df_est_params_mean %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

## simulate the model with the paraeters which a_G = a_B, and b_G = b_B, and the threshold is now same as the true threshold ====
df_est_params_mean_true_threshold <- df_est_params_mean %>% mutate(threshold = true_threshold)

df_sim_with_mean_parameters_and_true_threshold <- simulate_model(
  df_rule_hit, df_est_params_mean_true_threshold %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

## simulate the model with the paraeters which a_G = a_B, and b_G = b_B, and the threshold is now same as the true threshold ====
df_est_params_max <-
  df_est_params %>%
  mutate(
    a_G = map2_dbl(a_G, a_B, ~ max(c(.x, .y))),
    a_B = a_G,
    b_G = map2_dbl(b_G, b_B, ~ min(c(.x, .y))),
    b_B = b_G
  )

df_sim_with_max_parameters <- simulate_model(
  df_rule_hit, df_est_params_max %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

df_est_params_max_true_threshold <- df_est_params_max %>% mutate(threshold = true_threshold)

df_sim_with_max_parameters_and_true_threshold <- simulate_model(
  df_rule_hit, df_est_params_max_true_threshold %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)


df_est_params_minmax <-
  df_est_params %>%
  mutate(
    a_G = map2_dbl(a_G, a_B, ~ min(c(.x, .y))),
    a_B = a_G,
    b_G = map2_dbl(b_G, b_B, ~ max(c(.x, .y))),
    b_B = b_G
  )

df_sim_with_minmax_parameters <- simulate_model(
  df_rule_hit, df_est_params_minmax %>%
    select(-c(true_threshold, threshold_ratio)),
  binary_sign_model_obj, "Distance"
)

simulation_condition_list <- c(
  "with_true_threshold",
  "with_flipped_parameters",
  "with_flipped_parameters_and_true_threshold",
  "with_mean_parameters",
  "with_mean_parameters_and_true_threshold"
)

df_sim_list <- lapply(
  simulation_condition_list,
  function(condition) {
    df_sim <- get(paste0("df_sim_", condition))
  }
) %>% setNames(simulation_condition_list)

simulation_condition_list_with_actual <- c(
  "with_actual_parameters",
  "with_true_threshold",
  "with_flipped_parameters",
  "with_flipped_parameters_and_true_threshold",
  "with_mean_parameters",
  "with_mean_parameters_and_true_threshold"
)

## calculate the anova of accuracy for all conditions corrected with Greenhouse-Geisser ====
# calculate anova with anovakun and save the result into files for each condition
# file name format is model_sim_{condition}_anova_acc_score_true_rule
foreach(condition = names(df_sim_list), df_sim = df_sim_list) %do% {
  df_sim %>%
    summarize_performance(TrueRule, Correct, DisplayScore) %>%
    select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
    anovakun("sAB", long = TRUE, gg = TRUE) %>%
    print() %>%
    sink_analysis(
      paste0(
        "model_sim_",
        condition,
        "_anova_model_acc_score_truerule",
        ".txt"
      ),
      analysis_group = "simulation_with_parameters"
    )
}

# simulation of all set of parameters ----
df_sim_with_all_parameters <- df_est_params %>% # list up the all the combinations of the parameters
  list_combination_of_parameters() %>%
  group_by(is_true_theta, is_alpha_symmetric, is_beta_symmetric) %>%
  nest() %>%
  mutate(
    model_name =
      paste0(
        if_else(is_true_theta, "true_theta", "est_theta"),
        "_",
        if_else(is_alpha_symmetric, "sym_alpha", "asym_alpha"),
        "_",
        if_else(is_beta_symmetric, "sym_beta", "asym_beta")
      ),
    model_title = paste0(
      if_else(is_true_theta, "true θ", "subjective θ"),
      ", ",
      if_else(is_alpha_symmetric, "fixed α", "asymmetric α"),
      ", ",
      if_else(is_beta_symmetric, "fixed β", "asymmetric β")
    )
  ) %>%
  mutate( # simulate the model with the conditions
    df_sim = map(data, ~ simulate_model(
      df_rule_hit, .x %>%
        select(-c(true_threshold, threshold_ratio)),
      binary_sign_model_obj, "Distance"
    ))
  )

# plot simulation results ====
df_hebaviour_performance <- df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(`p(correct state inference)` = mean(Correct)) %>%
  group_by(TrueRule, DisplayScore)

df_hebaviour_performance_summary <-
  df_hebaviour_performance %>%
  group_by(TrueRule, DisplayScore) %>%
  summarise(
    median_p_correct = median(`p(correct state inference)`),
    iqr_p_correct = IQR(`p(correct state inference)`),
    q1 = quantile(`p(correct state inference)`, 0.25),
    q3 = quantile(`p(correct state inference)`, 0.75)
  )


df_hebaviour_confidence <- df_rule_hit %>%
  mutate(DisplayScore = if_else(DisplayScore > 0, "positive", "negative")) %>%
  group_by(PlayerID, TrueRule, DisplayScore) %>%
  summarise(confidence = mean(zConfidence)) %>%
  group_by(TrueRule, DisplayScore)

df_hebaviour_confidence_summary <-
  df_hebaviour_confidence %>%
  group_by(TrueRule, DisplayScore) %>%
  summarise(
    median_confidence = median(confidence),
    iqr_confidence = IQR(confidence),
    q1 = quantile(confidence, 0.25),
    q3 = quantile(confidence, 0.75)
  )

# plot accuracy (Supplementary Figure 8) ----
df_fig_switch_acc_sim <-
  df_sim_with_all_parameters %>%
  mutate( # plot the accuracy sequence
    p_model_switch_acc = map(df_sim, ~ (plot_acc_seq(.x) +
      labs(title = model_title) +
      theme(plot.title = element_text(size = 32 / fig_anova_scale))
    ) %T>%
      save_svg_figure(paste0("model_sim_", model_name, "_switch_acc_fig"),
        analysis_group =
          fs::path(
            "simulation_with_all_comb_parameters",
            "switch_acc_fig"
          ),
        width = fig_timeseries_width, height = fig_timeseries_height,
        scaling = fig_anova_scale, unit = "mm"
      ))
  )

df_fig_switch_acc_sim %>% # layout the figures, columns are based on is_true_theta, rows are based on is_alpha_symmetric and is_beta_symmetric
  pull(p_model_switch_acc) %>%
  wrap_plots(
    nrow = ceiling(length(.) / 2),
    ncol = 2,
    byrow = FALSE,
    guides = "collect"
  ) %>%
  save_svg_figure(
    "model_sim_all_parameters_switch_acc_fig",
    analysis_group = "simulation_with_all_comb_parameters",
    width = fig_timeseries_width / fig_anova_scale * 2,
    height = fig_timeseries_height / fig_anova_scale * ceiling(length(.) / 2),
    scaling = 1,
    unit = "mm"
  )

# plot entropy (Supplementary Figure 11) ----
df_fig_switch_conf_sim <-
  df_sim_with_all_parameters %>%
  mutate( # plot the entropy sequence
    p_model_switch_conf = map(df_sim, ~ (plot_conf_seq(.x) +
      labs(title = model_title) +
      theme(plot.title = element_text(size = 32 / fig_anova_scale))) %T>%
      save_svg_figure(paste0("model_sim_", model_name, "_switch_conf_fig"),
        analysis_group =
          fs::path(
            "simulation_with_all_comb_parameters",
            "switch_conf_fig"
          ),
        width = fig_timeseries_width, height = fig_timeseries_height,
        scaling = fig_anova_scale, unit = "mm"
      ))
  )

df_fig_switch_conf_sim %>% # layout the figures, columns are based on is_true_theta, rows are based on is_alpha_symmetric and is_beta_symmetric
  pull(p_model_switch_conf) %>%
  wrap_plots(
    nrow = ceiling(length(.) / 2),
    ncol = 2,
    byrow = FALSE,
    guides = "collect"
  ) %>%
  save_svg_figure(
    "model_sim_all_parameters_switch_conf_fig",
    analysis_group = "simulation_with_all_comb_parameters",
    width = fig_timeseries_width / fig_anova_scale * 2,
    height = fig_timeseries_height / fig_anova_scale * ceiling(length(.) / 2),
    scaling = 1,
    unit = "mm"
  )

# plot Z + bias
df_fig_Z_bias_seq_sim <-
  df_sim_with_all_parameters %>%
  mutate( # plot the Z + bias sequence
    p_model_Z_bias_seq = map(df_sim, ~ (plot_Z_bias_seq(
      .x %>%
        unnest(cols = data) %>%
        mutate(
          biases = calc_bias(b_G, b_B, DisplayScore),
          Z_bias = calc_Z_bias_from_p(pred_adj)
        )
    ) +
      labs(title = model_title) +
      theme(plot.title = element_text(size = 32 / fig_anova_scale))
    ) %T>%
      save_svg_figure(paste0("model_sim_", model_name, "_Z_bias_seq_fig"),
        analysis_group =
          fs::path(
            "simulation_with_all_comb_parameters",
            "Z_bias_seq_fig"
          ),
        width = fig_timeseries_width, height = fig_timeseries_height,
        scaling = fig_anova_scale, unit = "mm"
      ))
  )

# plot accuracy anova (Supplementary Figure 6) ----
df_fig_anova_score_true_rule_sim <- df_sim_with_all_parameters %>%
  mutate( # plot the accuracy anova
    p_model_anova_acc_score_truerule = map(df_sim, ~ (
      plot_acc_anova_no_sina(.x) +
        geom_errorbar(
          data = df_hebaviour_performance_summary,
          aes(
            x = TrueRule,
            y = median_p_correct,
            ymin = q1,
            ymax = q3,
            alpha = "black",
            group = interaction(TrueRule, DisplayScore)
          ),
          width = 0.15,
          color = "black",
          size = 0.6,
          position = position_dodge(width = 0.9)
        ) +
        geom_point(
          data = df_hebaviour_performance_summary,
          aes(
            x = TrueRule,
            y = median_p_correct,
            fill = NA,
            alpha = "black",
            group = interaction(TrueRule, DisplayScore)
          ),
          size = 2,
          color = "black",
          shape = 21,
          position = position_dodge(width = 0.9)
        ) +
        labs(title = model_title) +
        theme(plot.title = element_text(size = 32 / fig_anova_scale))
    ) %T>%
      save_svg_figure(paste0("model_sim_", model_name, "_anova_acc_score_truerule_fig"),
        analysis_group =
          fs::path(
            "simulation_with_all_comb_parameters",
            "anova_acc_score_truerule_fig"
          ),
        width = fig_anova_width, height = fig_anova_height,
        scaling = fig_anova_scale, unit = "mm"
      ))
  )

df_fig_anova_score_true_rule_sim %>% # layout the figures, columns are based on is_true_theta, rows are based on is_alpha_symmetric and is_beta_symmetric
  pull(p_model_anova_acc_score_truerule) %>%
  wrap_plots(
    nrow = ceiling(length(.) / 2),
    ncol = 2,
    byrow = FALSE,
    guides = "collect"
  ) %>%
  save_svg_figure(
    "model_sim_all_parameters_anova_acc_score_truerule_fig",
    analysis_group = "simulation_with_all_comb_parameters",
    width = fig_anova_width / fig_anova_scale * 2,
    height = fig_anova_height / fig_anova_scale * ceiling(length(.) / 2),
    scaling = 1,
    unit = "mm"
  )

# calculate anova with anovakun and save the result into files for each condition
df_sim_with_all_parameters %>%
  mutate( # calculate the anova with anovakun
    anova_acc_score_truerule = map(df_sim, ~ {
      df_anova <- .x %>%
        summarize_performance(TrueRule, Correct, DisplayScore) %>%
        select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
        anovakun("sAB", long = TRUE, gg = TRUE) %>%
        print() %>%
        sink_analysis(
          paste0("model_sim_", model_name, "_anova_acc_score_truerule", ".txt"),
          analysis_group =
            fs::path(
              "simulation_with_all_comb_parameters",
              "anova_acc_score_truerule_fig"
            ),
        )
      df_anova
    })
  )

# plot entropy anova (Supplementary Figure 9) ----
df_fig_anova_entropy_score_true_rule_sim <-
  df_sim_with_all_parameters %>%
  mutate( # plot the entropy anova
    p_model_anova_entropy_score_truerule = map(df_sim, ~ (
      plot_conf_anova_no_sina(.x) +
        geom_errorbar(
          data = df_hebaviour_confidence_summary,
          aes(
            x = TrueRule,
            y = median_confidence,
            ymin = q1,
            ymax = q3,
            alpha = "black",
            group = interaction(TrueRule, DisplayScore)
          ),
          width = 0.15,
          color = "black",
          size = 0.6,
          position = position_dodge(width = 0.9)
        ) +
        geom_point(
          data = df_hebaviour_confidence_summary,
          aes(
            x = TrueRule,
            y = median_confidence,
            fill = NA,
            alpha = "black",
            group = interaction(TrueRule, DisplayScore)
          ),
          size = 2,
          color = "black",
          shape = 21,
          position = position_dodge(width = 0.9)
        ) +
        labs(title = model_title) +
        theme(plot.title = element_text(size = 32 / fig_anova_scale))
    ) %T>%
      save_svg_figure(paste0("model_sim_", model_name, "_anova_entropy_score_truerule_fig"),
        analysis_group =
          fs::path(
            "simulation_with_all_comb_parameters",
            "anova_entropy_score_truerule_fig"
          ),
        width = fig_anova_width, height = fig_anova_height,
        scaling = fig_anova_scale, unit = "mm"
      ))
  )

df_fig_anova_entropy_score_true_rule_sim %>% # layout the figures, columns are based on is_true_theta, rows are based on is_alpha_symmetric and is_beta_symmetric
  pull(p_model_anova_entropy_score_truerule) %>%
  wrap_plots(
    nrow = ceiling(length(.) / 2),
    ncol = 2,
    byrow = FALSE,
    guides = "collect"
  ) %>%
  save_svg_figure(
    "model_sim_all_parameters_anova_entropy_score_truerule_fig",
    analysis_group = "simulation_with_all_comb_parameters",
    width = fig_anova_width / fig_anova_scale * 2,
    height = fig_anova_height / fig_anova_scale * ceiling(length(.) / 2),
    scaling = 1,
    unit = "mm"
  )

# calculate anova with anovakun and save the result into files for each condition
df_sim_with_all_parameters %>%
  mutate( # calculate the anova with anovakun
    anova_entropy_score_truerule = map(df_sim, ~ {
      df_anova <- .x %>%
        summarize_performance(TrueRule, Correct, DisplayScore) %>%
        select(PlayerID, DisplayScore, TrueRule, mean_Correct) %>%
        anovakun("sAB", long = TRUE, gg = TRUE) %>%
        print() %>%
        sink_analysis(
          paste0("model_sim_", model_name, "_anova_entropy_score_truerule", ".txt"),
          analysis_group =
            fs::path(
              "simulation_with_all_comb_parameters",
              "anova_entropy_score_truerule_fig"
            ),
        )
      df_anova
    })
  )
