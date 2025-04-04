library(tidyverse)
library(ggplot2)
library(ggforce)
library(rstatix)
library(svglite)

# result_root <- "Figure/results/"
result_root_dir <- fs::path("results")

figure_around_switch <-
  list(
    stat_summary(fun = mean, geom = "line"),
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.7,
      linewidth = 0.8,
      alpha = 0.8
    ),
    geom_vline(xintercept = 1 - 0.5, linetype = "dashed", color = "gray")
  )

process_for_switch <- function(data, exp_to_be_summarized, summary_var) {
  enquo_var_to_be_summarized <- rlang::enquo(exp_to_be_summarized)
  enquo_summary_var <- rlang::enquo(summary_var)
  data %>%
    pivot_longer(cols = c(TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom), names_to = "switch", values_to = "num of trials") %>%
    group_by(PlayerID, switch, `num of trials`) %>%
    summarise(!!enquo_summary_var := mean(!!enquo_var_to_be_summarized), TrueRule = unique(TrueRule)) %>%
    dplyr::select(PlayerID, switch, TrueRule, `num of trials`, !!enquo_summary_var) %>%
    mutate(
      switch =
        if_else(switch == "TrialsAfterSwitchToRandom",
          "skill to random",
          "random to skill"
        )
    ) %>%
    mutate(
      switch = as.factor(switch)
    )
}

plot_dis_diff <- function(data, x_var, y_var, group_var) {
  # df_rule_hit %>% group_by(PlayerID) %>%
  #   mutate(
  #     DisplayScore = if_else(DisplayScore > 0, "positive", "negative") %>% as.factor(),
  #     prev_LocX = lag(LocX), prev_LocY = lag(LocY),
  #     prev_distance = lag(Distance),
  #     next_distance = lead(Distance),
  #     next_LocX = lead(LocX), next_LocY = lead(LocY),
  #     distance_from_prev = Distance - prev_distance,
  #     distance_from_next = Distance - next_distance,
  #     normalized_distance_from_prev = distance_from_prev / threshold,
  #     normalized_distance_from_next = distance_from_next / threshold) %>%
  data %>%
    drop_na({{ y_var }}) %>%
    plot_summarized_performance({{ x_var }}, {{ y_var }}, {{ group_var }})
}

numeric_score_to_strings <- function(score) {
  score <- case_when(
    is.null(score) ~ NA_character_,
    mode(score) == "numeric" & score > 0 ~ "positive",
    mode(score) == "numeric" & score <= 0 ~ "negative",
    TRUE ~ as.character(score)
  )
  score
}

process_for_summary_anova <- function(data) {
  data %>%
    group_by(PlayerID) %>%
    mutate(
      TrueRule = as.factor(TrueRule),
      EstRule = as.factor(EstRule),
      skill_choice = as.numeric(EstRule == "skill"),
      prev_choice = lag(EstRule),
      DisplayScore = numeric_score_to_strings(DisplayScore) %>% as.factor(),
      conf_binary = if_else(zConfidence > 0, "high", "low") %>% as.factor() %>% fct_relevel("low", "high"),
      prev_conf = lag(EstRuleConfidence),
      prev_zconf = lag(zConfidence),
      prev_conf_binary = lag(conf_binary) %>% as.factor(),
      delta_conf = zConfidence - prev_zconf,
      switch = EstRule != prev_choice
    ) %>%
    ungroup() %>%
    drop_na(prev_choice)
}

summarize_performance <- function(data, x_var, y_var, group) {
  enquo_x_var <- rlang::enquo(x_var)
  enquo_y_var <- rlang::enquo(y_var)
  enquo_group <- rlang::enquo(group)

  mean_name <- paste0("mean_", quo_name(enquo_y_var))

  data %>%
    group_by(PlayerID, !!enquo_x_var, !!enquo_group) %>%
    summarise(
      !!mean_name := mean(!!enquo_y_var, na.rm = TRUE),
      count = n()
    )
}

plot_grouped_summary <- function(data, x_var, y_var, group_var, ...) {
  enquo_x_var <- rlang::enquo(x_var)
  enquo_y_var <- rlang::enquo(y_var)
  enquo_group <- rlang::enquo(group_var)

  data %>%
    ggplot(
      aes(
        x = {{ x_var }}, y = {{ y_var }},
        group = interaction({{ x_var }}, {{ group_var }}), color = {{ group_var }}
      ), ...
    ) +
    geom_boxplot(outlier.shape = NA) +
    geom_sina(alpha = 0.2)
}

plot_summarized_performance <- function(data, x_factor, y_factor, group_factor, ...) {
  enquo_x_factor <- rlang::enquo(x_factor)
  enquo_y_factor <- rlang::enquo(y_factor)
  enquo_group_factor <- rlang::enquo(group_factor)
  summary_name <- paste0("mean_", quo_name(enquo_y_factor)) # should match with summarize_performance
  sym_summary_name <- sym(summary_name)
  print(summary_name)
  data %>%
    summarize_performance({{ x_factor }}, {{ y_factor }}, {{ group_factor }}) %>%
    plot_grouped_summary({{ x_factor }}, !!sym_summary_name, {{ group_factor }}, ...) +
    theme_fig1 +
    scale_color_manual(values = palette_rule)
}

save_svg_figure <- function(plot, figure_name, result_root = result_root_dir, analysis_group = "", ...) {
  # create directory if not exist
  if (!fs::dir_exists(fs::path(result_root, analysis_group))) {
    fs::dir_create(fs::path(result_root, analysis_group))
  }

  ggsave(
    filename =
      fs::path(
        result_root, analysis_group, figure_name,
        ext = "svg"
      ) %>% here::here(),
    plot = plot,
    device = "svg",
    create.dir = TRUE,
    ...
  )
  ggsave(
    filename =
      fs::path(
        result_root, analysis_group, figure_name,
        ext = "png"
      ) %>% here::here(),
    plot = plot,
    device = "png",
    create.dir = TRUE,
    ...
  )
}

do_anova <- function(processed_data, factorA, factorB, value_tobe_summarised, tech = TRUE) {
  data <- processed_data %>%
    group_by(PlayerID, {{ factorA }}, {{ factorB }}) %>%
    summarise(summarised_value = mean({{ value_tobe_summarised }})) %>%
    select(PlayerID, {{ factorA }}, {{ factorB }}, summarised_value)
  anovakun(data, "sAB", long = TRUE, tech = tech, gg = TRUE)
}

print_anova_sentences <- function(anova_df, f_digits = 3, p_digits = 3) {
  anova_table <- anova_df$`ANOVA TABLE`[[2]] %>% column_to_rownames(var = "source.col")

  var_list <- anova_df$`SPHERICITY INDICES`[[2]][["Effect"]]
  var_list <- var_list[2:length(var_list)]

  fist_anova_strings <- sapply(var_list, function(var_name) {
    strings <- paste0(
      "the effect of ", var_name,
      " is ",
      if_else(anova_table[[var_name, "p.col"]] < 0.05, "significant", "not significant"),
      "(",
      "F(",
      anova_table[[var_name, "df.col"]],
      ") = ",
      anova_table[[var_name, "f.col"]] %>% format(digits = f_digits),
      ", p ",
      if_else(anova_table[[var_name, "p.col"]] < 0.001, "< 0.001", paste0("= ", anova_table[[var_name, "p.col"]] %>% format(digits = p_digits))),
      ").\n"
    )
  })
  first_anova_strings <- as.vector(unlist(unname(fist_anova_strings)))

  analysis_list <- list("A", "B", "A:B")
  post_df <- anova_df$`POST ANALYSES`
  post_strings <- sapply(analysis_list, function(var_name) {
    if (any(is.na(post_df[var_name])) || any(is.null(post_df[[var_name]]))) {
      print(is.na(post_df[var_name]))
      print(is.null(post_df[[var_name]]))
      strings <- NULL
    } else {
      df <- post_df[[var_name]]
      variable <- df[[var_name]]
      simtab <- df[["simtab"]]
      strings <-
        simtab %>% apply(1, function(row_var) {
          paste0(
            row_var[["source.col"]],
            " is ",
            if_else(row_var[["p.col"]] %>% as.numeric() < 0.05 & !as.numeric(is.na(row_var[["p.col"]])), "", "not "),
            "significant(",
            "F(",
            row_var[["df.col"]] %>% as.numeric(),
            ") = ",
            row_var[["f.col"]] %>% format(digits = f_digits),
            "), ",
            if_else(is.na(row_var[["p.col"]] %>% as.numeric()),
              "",
              if_else(row_var[["p.col"]] %>% as.numeric() < 0.001,
                "p < 0.001",
                paste0("p = ", row_var[["p.col"]] %>% as.numeric() %>% format(digits = p_digits))
              )
            ),
            ")."
          )
        })
      strings <- as.vector(unlist(unname(strings)))
    }
    strings
  })
  c(first_anova_strings, post_strings)
}

int2ordinal <- function(x) {
  suffix <- rep("th", 10)
  suffix[1 + 1] <- "st"
  suffix[2 + 1] <- "nd"
  suffix[3 + 1] <- "rd"

  paste0(x, suffix[(x %% 10) + 1])
}

tibble_lme <- function(data, formula) {
  formula_string <- paste(c(formula))
  data %>% mutate(
    res = map(
      data,
      ~ lmerTest::lmer(formula, data = .x) %>%
        summary() %>%
        `$`(coefficients) %>%
        round(8) %>%
        as_tibble(rownames = "Predictor") %>%
        mutate(
          formula = formula_string
        ) %>%
        select(formula, everything())
    )
  )
}

show_p <- function(p, digits = 3) {
  paste0(
    "p ",
    if_else(p < 0.001, "< 0.001",
      paste0("= ", p %>% signif(digits = digits))
    )
  )
}

pick_post_tests <- function(data, formula) {
  data %>%
    filter({{ formula }}) %>%
    pull(p.adj)
}

show_post_tests <- function(data, formula, ...) {
  data %>%
    pick_post_tests({{ formula }}) %>%
    show_p(...)
}

geom_sina_fitted <- function(..., maxwidth = 0.5, scale = "width", dodge_width = 0.8) {
  geom_sina(...,
    maxwidth = maxwidth,
    scale = scale,
    position = position_dodge(width = dodge_width),
    color = "black",
    alpha = 0.2
  )
}

plot_score_rule <- function(data, x, y, group, .fill = {{ group }}, .alpha = {{ x }}, sina_maxwidth = NULL, sina_scale = NULL, sina_dodge_width = NULL) {
  summary_name <- paste0("mean_", rlang::quo_name(enquo(y))) # should match with summarize_performance
  sym_summary_name <- sym(summary_name)

  input_args <- list(
    maxwidth = sina_maxwidth,
    scale = sina_scale,
    dodge_width = sina_dodge_width
  ) %>% discard(is.null)

  wrap_sina <- function() do.call(gg_filled_box_sina, input_args)

  data %>%
    mutate(DisplayScore = numeric_score_to_strings(DisplayScore)) %>%
    mutate(PlayerID = as.factor(PlayerID)) %>%
    mutate(Correct = as.numeric(Correct)) %>%
    summarize_performance({{ x }}, {{ y }}, {{ group }}) %>%
    mutate(condition = interaction({{ x }}, {{ group }}) %>% as.character()) %>%
    ggplot(aes(x = {{ x }}, y = !!sym_summary_name, group = interaction({{ x }}, {{ group }}), fill = {{ .fill }}, alpha = {{ .alpha }})) +
    wrap_sina() +
    guides(alpha = guide_legend(override.aes = list(color = "black", fill = c("black"))))
  # ggplot(aes(x = {{x}}, y = !!sym_summary_name, group = interaction({{x}}, {{group}}), color=condition)) +
  # geom_boxplot(
  #   outlier.shape = NA) + geom_sina(alpha = 0.2, show.legend = FALSE) +
  # theme(legend.position = "right")
}

gg_filled_box_sina <- function(box_width = 0.75, maxwidth = 0.5, scale = "width", dodge_width = 0.8) {
  list(
    geom_boxplot(
      aes(
        color = after_scale(
          alpha(if_else(alpha > 0.5, "black", fill), 1)
        )
      ),
      width = box_width,
      outlier.shape = NA
    ),
    geom_sina_fitted(alpha = 0.2, color = "black", show.legend = FALSE, maxwidth = (box_width / 0.75) * (maxwidth), scale = scale, dodge_width = (box_width / 0.75) * dodge_width)
  )
}

posthoc_wilcox_test <- function(data, formula, fixed_factor = NULL) {
  factor_levels <- data %>%
    pull(formula[[3]]) %>%
    unique() %>%
    sort(decreasing = FALSE)
  tmp <- data %>%
    group_by({{ fixed_factor }}) %>%
    nest() %>%
    mutate(
      result =
        map(
          .x = data, .f =
            ~ .x %>%
              rstatix::wilcox_test(
                {{ formula }},
                paired = TRUE, data = .,
                exact = TRUE
              )
        ),
      wilcoxsign =
        map(
          .x = data, .f =
            ~ .x %>%
              select(PlayerID, formula[[2]], formula[[3]]) %>%
              pivot_wider(names_from = formula[[3]], values_from = formula[[2]]) %>%
              coin::wilcoxsign_test(as.formula(paste0(factor_levels[[2]], "~", factor_levels[[1]])), data = ., distribution = "exact", zero.method = "Wilcoxon")
        ),
      Z = sapply(wilcoxsign, coin::statistic),
      p_coin = sapply(wilcoxsign, coin::pvalue)
    ) %>%
    unnest(cols = c(result)) %>%
    adjust_pvalue(p.col = "p", method = "fdr")
}

posthoc_wilcox_test2 <- function(data, target, factor1, factor2, .method = "fdr") {
  formula1 <-
    as.formula(
      paste0(rlang::quo_name(enquo(target)), "~", rlang::quo_name(enquo(factor1)))
    )
  formula2 <-
    as.formula(
      paste0(rlang::quo_name(enquo(target)), "~", rlang::quo_name(enquo(factor2)))
    )
  factor_levels1 <- data %>%
    pull(formula1[[3]]) %>%
    unique() %>%
    sort(decreasing = FALSE)
  factor_levels2 <- data %>%
    pull(formula2[[3]]) %>%
    unique() %>%
    sort(decreasing = FALSE)
  df1 <-
    data %>%
    group_by({{ factor2 }}) %>%
    nest() %>%
    mutate(
      result =
        map(
          .x = data, .f =
            ~ .x %>%
              rstatix::wilcox_test(
                formula1,
                paired = TRUE,
                exact = TRUE,
                data = .
              )
        ),
      wilcoxsign =
        map(
          .x = data, .f =
            ~ .x %>%
              select(PlayerID, formula1[[2]], formula1[[3]]) %>%
              pivot_wider(names_from = formula1[[3]], values_from = formula1[[2]]) %>%
              coin::wilcoxsign_test(as.formula(paste0("`", factor_levels1[[2]], "`", "~", "`", factor_levels1[[1]], "`")), data = ., distribution = "exact", zero.method = "Wilcoxon")
        ),
      Z = sapply(wilcoxsign, coin::statistic),
      p_coin = sapply(wilcoxsign, coin::pvalue)
    ) %>%
    unnest(cols = c(result))

  df2 <-
    data %>%
    group_by({{ factor1 }}) %>%
    nest() %>%
    mutate(
      result =
        map(
          .x = data, .f =
            ~ .x %>%
              rstatix::wilcox_test(
                formula2,
                paired = TRUE,
                exact = TRUE,
                data = .
              )
        ),
      wilcoxsign =
        map(
          .x = data, .f =
            ~ .x %>%
              select(PlayerID, formula2[[2]], formula2[[3]]) %>%
              pivot_wider(names_from = formula2[[3]], values_from = formula2[[2]]) %>%
              coin::wilcoxsign_test(as.formula(paste0("`", factor_levels2[[2]], "`", "~", "`", factor_levels2[[1]], "`")), data = ., distribution = "exact", zero.method = "Wilcoxon")
        ),
      Z = sapply(wilcoxsign, coin::statistic),
      p_coin = sapply(wilcoxsign, coin::pvalue)
    ) %>%
    unnest(cols = c(result))
  rbind(df1, df2) %>%
    ungroup() %>%
    select(data, .y., {{ factor1 }}, {{ factor2 }}, group1, group2, wilcoxsign, n1, n2, statistic, Z, p, p_coin) %>%
    adjust_pvalue(p.col = "p", method = .method)
}

summary_posthoc_test2 <- function(df_rule_hit, target, factor1, factor2) {
  summary_target <- sym(paste0("mean_", rlang::quo_name(rlang::enquo(target))))
  if (class(df_rule_hit$DisplayScore) == "numeric") {
    df_rule_hit <- df_rule_hit %>% mutate(DisplayScore = numeric_score_to_strings(DisplayScore))
  }
  df_rule_hit %>%
    mutate(PlayerID = as.factor(PlayerID)) %>%
    mutate(Correct = as.numeric(Correct)) %>%
    summarize_performance({{ factor1 }}, {{ target }}, {{ factor2 }}) %>%
    select(PlayerID, {{ factor1 }}, {{ factor2 }}, {{ summary_target }}) %>%
    posthoc_wilcox_test2({{ summary_target }}, {{ factor1 }}, {{ factor2 }})
}

posthoc_wilcox_effsize2 <- function(data, target, factor1, factor2, .method = "fdr") {
  formula1 <-
    as.formula(
      paste0(rlang::quo_name(enquo(target)), "~", rlang::quo_name(enquo(factor1)))
    )
  formula2 <-
    as.formula(
      paste0(rlang::quo_name(enquo(target)), "~", rlang::quo_name(enquo(factor2)))
    )
  df1 <-
    data %>%
    group_by({{ factor2 }}) %>%
    nest() %>%
    mutate(
      result =
        map(
          .x = data, .f =
            ~ .x %>%
              wilcox_effsize(
                formula1,
                paired = TRUE, data = .
              )
        )
    ) %>%
    unnest(cols = c(result))

  df2 <-
    data %>%
    group_by({{ factor1 }}) %>%
    nest() %>%
    mutate(
      result =
        map(
          .x = data, .f =
            ~ .x %>%
              wilcox_effsize(
                formula2,
                paired = TRUE,
                data = .
              )
        )
    ) %>%
    unnest(cols = c(result))
  rbind(df1, df2) %>% ungroup()
}

summary_posthoc_test2 <- function(df_rule_hit, target, factor1, factor2) {
  summary_target <- sym(paste0("mean_", rlang::quo_name(rlang::enquo(target))))
  if (class(df_rule_hit$DisplayScore) == "numeric") {
    df_rule_hit <- df_rule_hit %>% mutate(DisplayScore = numeric_score_to_strings(DisplayScore))
  }
  df_rule_hit %>%
    mutate(PlayerID = as.factor(PlayerID)) %>%
    mutate(Correct = as.numeric(Correct)) %>%
    summarize_performance({{ factor1 }}, {{ target }}, {{ factor2 }}) %>%
    select(PlayerID, {{ factor1 }}, {{ factor2 }}, {{ summary_target }}) %>%
    posthoc_wilcox_test2({{ summary_target }}, {{ factor1 }}, {{ factor2 }})
}

# usage
# df_rule_hit %>%
# posthoc_wilcox_against_chance(ratio ~ TrueRule)
posthoc_wilcox_against_chance <- function(df, formula, chance_level = 0.5, alternative = "greater") {
  # extract the target variable
  target <- formula[[2]]

  # extract the factor
  factor <- formula[[3]]
  # browser()
  df_per_score_test_against_chance <- df %>%
    mutate(chance_level = chance_level) %>%
    group_by({{ factor }}) %>%
    rstatix::wilcox_test(as.formula(paste0(target, "~ 1")),
      data = .,
      mu = chance_level,
      alternative = alternative,
      exact = TRUE
    ) %>%
    adjust_pvalue(method = "fdr") %>%
    add_significance(p.col = "p.adj")

  df_per_score_test_against_chance_coin <- df %>%
    mutate(chance_level = chance_level) %>%
    group_by({{ factor }}) %>%
    nest() %>%
    mutate(
      wilcoxsign = map(.x = data, .f = ~ .x %>%
        select(PlayerID, {{ target }}, chance_level) %>%
        coin::wilcoxsign_test(as.formula(paste0(target, " ~ chance_level")),
          data = .,
          distribution = "exact",
          zero.method = "Wilcoxon",
          alternative = alternative
        )),
      Z = sapply(wilcoxsign, coin::statistic),
      p_coin = sapply(wilcoxsign, coin::pvalue)
    ) %>%
    select(-data, -wilcoxsign)

  by_name <- as.character(substitute(factor))
  result <- df_per_score_test_against_chance %>%
    inner_join(df_per_score_test_against_chance_coin, by = by_name) %>%
    select(.y., {{ factor }}, group1, group2, n, statistic, Z, p, p_coin, p.adj, p.adj.signif)

  return(result)
}

map_id <- function(x, mapping_vector) {
  sapply(x, function(.x) which(mapping_vector == .x)) %>%
    as.factor()
}

sink_output <- function(statement, filename) {
  con <- file(filename, open = "wt")
  sink(con)
  statement
  sink(file = NULL)
  close(con)
}

sink_analysis <- function(statement, filename, root_dir = "Figure/results/", analysis_group = "") {
  # add extention if not included
  if (fs::path_ext(filename) == "") {
    filename <- fs::path(filename, ext = "txt")
  }
  filename <- fs::path(root_dir, analysis_group, filename) %>% here::here()

  # extract directory from filename
  dir_to_save <- fs::path_dir(filename)

  # create directory if not exist
  if (!fs::dir_exists(dir_to_save)) {
    fs::dir_create(dir_to_save)
  }

  sink_output(statement, filename)
}

calc_wilcox_Z <- function(data) {
  data %>%
    mutate(
      n1 = sum(group == "A"), # グループAのサンプルサイズ
      n2 = sum(group == "B"), # グループBのサンプルサイズ
      mean_U = n1 * n2 / 2, # U値の平均
      sd_U = sqrt(n1 * n2 * (n1 + n2 + 1) / 12), # U値の標準偏差
      Z = (statistic - mean_U) / sd_U # Z値
    )
}

tidy_lmer_results <- function(model) {
  model_type <- class(model)[1]
  # Extract fixed effects
  fixed_effects <- tidy(model, effects = "fixed", conf.int = TRUE)

  # Extract random effects
  random_effects <- tidy(model, effects = "ran_pars", conf.int = TRUE)

  # Combine fixed and random effects
  combined_results <- bind_rows(fixed_effects, random_effects)

  # Define significance levels for fixed effects only
  get_significance <- function(p) {
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*",
          ifelse(p < 0.1, ".", "")
        )
      )
    )
  }

  # Add significance levels for fixed effects
  combined_results <- combined_results %>%
    mutate(significance = if_else(effect == "fixed", get_significance(p.value), NA))

  if (model_type == "glmerMod") {
    # For glmer models
    metadata <- list(
      formula = as.character(formula(model)),
      num_obs = nobs(model),
      random_effects = paste(names(ranef(model)), sapply(ranef(model), nrow), sep = ": ", collapse = ", "),
      AIC = AIC(model),
      convergence_status = model@optinfo$conv$opt
    )
  } else if (model_type == "lmerMod" || model_type == "lmerModLmerTest") {
    model_summary <- summary(model)
    metadata <- list(
      formula = as.character(formula(model)),
      num_obs = model_summary$nobs,
      random_effects = paste(names(model_summary$ngrps), model_summary$ngrps, sep = ": ", collapse = ", "),
      REML_criterion = model_summary$AICtab[["REML"]], # REML criterion
      convergence_status = model@optinfo$conv$opt
    )
  } else {
    print(model_type)
    stop()
  }

  # Add metadata as attributes to the combined table
  attr(combined_results, "metadata") <- metadata

  # Return the combined table
  combined_results
}

output_lme_results <- function(
    model, name, result_root = result_root_dir, analysis_group = "") {
  table <- model %>% tidy_lmer_results()
  path_to_save <- fs::path(result_root, analysis_group, name) %>% here::here()

  # create directory if not exist
  if (!fs::dir_exists(fs::path(result_root, analysis_group))) {
    fs::dir_create(fs::path(result_root, analysis_group))
  }

  # sink outputs
  model %>%
    summary() %>%
    print() %>%
    sink_output(
      fs::path(path_to_save, ext = "txt")
    )
  # save model as rds
  model %>% rio::export(
    fs::path(path_to_save, ext = "rds")
  )

  # save the table result
  table %>% rio::export(
    fs::path(path_to_save, ext = "csv")
  )
}

output_posthoc_result <- function(
    res, name, result_root = result_root_dir, analysis_group = "") {
  path_to_save <- fs::path(result_root, analysis_group, name) %>% here::here()

  # create directory if not exist
  if (!fs::dir_exists(fs::path(result_root, analysis_group))) {
    fs::dir_create(fs::path(result_root, analysis_group))
  }
  # save the table result
  res %>%
    select(-any_of(c("data", "wilcoxsign"))) %>%
    rio::export(
      fs::path(path_to_save, ext = "csv")
    )
}

output_csv <- function(data, name, result_root = result_root_dir, analysis_group = "") {
  path_to_save <- fs::path(result_root, analysis_group, name) %>% here::here()

  # create directory if not exist
  if (!fs::dir_exists(fs::path(result_root, analysis_group))) {
    fs::dir_create(fs::path(result_root, analysis_group))
  }
  # save the table result
  data %>%
    rio::export(
      fs::path(path_to_save, ext = "csv")
    )
}
