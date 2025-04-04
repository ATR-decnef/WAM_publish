library(tidyverse)
library(magrittr)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(doParallel)
library(gridExtra)
library(rlang)

# num_of_params = 5
# file_name = "R_result/alpha_GB_beta_GB_gamma_model_obj_scaledTransformedError_random_parameter_recovery_iter_100.rds"

convert_pattern <- c(
  "(?<=^|_)a(?=^|_)" = "alpha",
  "(?<=^|_)b(?=^|_)" = "beta",
  "(?<=^|_)gamma(?=^|_)" = "gamma",
  "_G$" = "\\[~pos\\]",
  "_B$" = "\\[~neg\\]",
  "^res_(.*)$" = "hat\\(\\1\\)"
)
convert_pattern <- c(
  "(?<=^|_)a(?=^|_)" = "\\\\alpha",
  "(?<=^|_)b(?=^|_)" = "\\\\beta",
  "(?<=^|_)gamma(?=^|_)" = "\\\\gamma",
  "threshold" = "\\\\theta",
  "_G$" = "_{pos}",
  "_B$" = "_{neg}",
  "^res_(.*)$" = "\\hat\\(\\1\\)",
  "^" = "$",
  "$" = "$"
)

convert_row_col_names <- function(df, pattern) {
  colnames(df) <- lapply(colnames(df), function(.x) expression(str_replace_all(.x, pattern)))
  rownames(df) <- lapply(rownames(df), function(.x) expression(str_replace_all(.x, pattern)))
  df
}

convert_label <- function(label) {
  label <- str_replace_all(label, c("(?<=^|_)a(?=^|_)" = "alpha", "(?<=^|_)b(?=^|_)" = "beta", "(?<=^|_)gamma(?=^|_)" = "gamma"))
  label <- str_replace(label, "_G$", "\\[~pos\\]")
  label <- str_replace(label, "_B$", "\\[~neg\\]")
  label <- str_replace(label, "^", "'true '*")
  label <- str_replace(label, ".*res_", "'recovered' ~")

  # expressionに変換するための文字列を作成
  parsed_label <- parse(text = label)
  print(parsed_label)
  return(parsed_label)
}


visualize_correlation <- function(file_name, fig_scale = fig_anova_scale, corr_method = "pearson") {
  # file_name <- here::here("model_based_analysis", file_name)
  data_name <- file_name %>%
    sub("\\.[^.]*", "", .) %>%
    sub("[^.]*/", "", .)
  file_name_no_dot <- file_name %>% sub("\\.[^.]*", "", .)
  file_name_no_dot_no_path <- file_name_no_dot %>% sub(".*[(\\)(//)(/)]", "", .)
  fig_title <- file_name_no_dot_no_path %>%
    sub("alpha.*gamma", "", .) %>%
    sub("(_model_obj_|_model_)", "", .) %>%
    sub("_parameter_recovery.*", "", .)
  print(fig_title)

  data_df <- import(
    file_name
  ) %>%
    as_tibble()

  num_of_params <- data_df %>%
    select(-PlayerID, -iteration) %>%
    colnames() %>%
    length() %>%
    `/`(2)
  print(num_of_params)
  stopifnot(num_of_params %% 1 == 0)

  corr_df <- data_df %>%
    select(-PlayerID, -iteration) %>%
    cor(method = corr_method) %>%
    `[`(1:(num_of_params), (2 * num_of_params):(num_of_params + 1)) # %T>% View()
  # Get p-value matrix
  p_df <- as.data.frame(ggcorrplot::cor_pmat(data_df %>% select(-PlayerID, -iteration))) %>% `[`(1:(num_of_params), (2 * num_of_params):(num_of_params + 1))

  p_df_csv <- p_df
  p_df_csv$Var1 <- as.factor(rownames(p_df_csv))
  p_df_csv <- melt(p_df_csv, id.vars = "Var1", variable.name = "Var2", value.name = "p")
  p_df_csv %>% export(
    sprintf("%s_p_df.csv", file_name_no_dot)
  )

  # Function to get asteriks
  labs.function <- function(x) {
    case_when(
      x >= 0.05 ~ "",
      x < 0.05 & x >= 0.01 ~ "*",
      x < 0.01 & x >= 0.001 ~ "**",
      x < 0.001 ~ "***"
    )
  }

  # Get asteriks matrix based on p-values
  p_labs <- p_df %>%
    mutate_all(labs.function)

  # Reshaping asteriks matrix to match ggcorrplot data output
  p_labs$Var1 <- as.factor(rownames(p_labs))
  p_labs <- melt(p_labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

  # Initial ggcorrplot
  cor_plot <- ggcorrplot(corr_df,
    type = "full",
    lab = TRUE,
    lab_size = 3,
    tl.cex = 12,
    tl.col = "red",
    legend.title = corr_method,
    ggtheme = ggplot2::theme_minimal() + theme(axis.title = element_text(), legend.key.height = unit(dev.size()[2] / 8, "inches"))
  )

  # Subsetting asteriks matrix to only those rows within ggcorrplot data
  p_labs$in.df <- ifelse(is.na(match(
    paste0(p_labs$Var1, p_labs$Var2),
    paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2)
  )),
  "No", "Yes"
  )

  p_labs <- select(filter(p_labs, in.df == "Yes"), -in.df)

  # Add asteriks to ggcorrplot
  cor_tick_x <- rownames(corr_df) %>% sapply(function(.x) str_replace_all(.x, convert_pattern) %>% TeX())
  cor_tick_y <- colnames(corr_df) %>% sapply(function(.x) str_replace_all(.x, convert_pattern) %>% TeX())
  names(cor_tick_x) <- rownames(corr_df)
  names(cor_tick_y) <- colnames(corr_df)
  print(cor_tick_x)
  print(cor_tick_y)
  # Visualize correlation between original and recovered parameters
  cor_plot_labs <- cor_plot +
    theme(
      axis.title.x = element_text(angle = 0, color = "black"),
      axis.title.y = element_text(angle = 90, color = "black")
    ) +
    scale_x_discrete(labels = cor_tick_x) +
    scale_y_discrete(labels = cor_tick_y) +
    xlab("true") +
    ylab("recovered")

  print(file_name_no_dot_no_path)
  cor_plot_labs %>%
    save_svg_figure(sprintf("%s_corr", file_name_no_dot_no_path),
      analysis_group = "parameter_recovery",
      width = fig_anova_width,
      height = fig_anova_height, units = "mm",
      scale = 0.5
    )

  cor_plot_labs
}

scatter_correlation <- function(file_name, fig_scale = fig_anova_scale) {
  # file_name <- here::here("model_based_analysis", file_name)
  data_name <- file_name %>%
    sub("\\.[^.]*", "", .) %>%
    sub("[^.]*/", "", .)
  file_name_no_dot <- file_name %>% sub("\\.[^.]*", "", .)
  file_name_no_dot_no_path <- file_name_no_dot %>% sub(".*[(\\)(//)(/)]", "", .)
  fig_title <- file_name_no_dot_no_path %>%
    sub("alpha.*gamma", "", .) %>%
    sub("(_model_obj_|_model_)", "", .) %>%
    sub("_parameter_recovery.*", "", .)
  print(fig_title)

  data_df <- import(
    file_name
  ) %>%
    as_tibble()
  scatter_param_list <-
    data_df %>%
    select(-PlayerID, -iteration) %>%
    colnames() %>%
    `[`(!data_df %>%
      select(-PlayerID, -iteration) %>%
      colnames() %>%
      str_detect("res_"))

  foreach(param = scatter_param_list) %do% {
    y_param <- paste0("res_", param)
    x_lab <- convert_label(param)
    y_lab <- convert_label(y_param)
    p_alpha_recovery_scatter <-
      data_df %>%
      ggplot(aes(x = !!sym(param), y = !!sym(y_param))) +
      geom_point() +
      # stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.3) +  # 密度のヒートマップ
      scale_fill_viridis_c() +
      theme_fig_base +
      xlab(expression("true ")) +
      xlab(x_lab) +
      ylab(y_lab) +
      scale_fill_viridis_c()
    p_alpha_recovery_scatter %>% save_svg_figure(sprintf("p_%s_recovery_scatter", param),
      analysis_group = "parameter_recovery",
      scaling = fig_scale,
      width = fig_anova_width,
      height = fig_anova_height,
      unit = "mm"
    )

    p_alpha_recovery_density <-
      data_df %>%
      ungroup() %>%
      ggplot(aes(x = !!sym(param), y = !!sym(y_param))) +
      # geom_point() +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.3) + # 密度のヒートマップ
      scale_fill_viridis_c() +
      theme_fig_base +
      xlab(expression("true ")) +
      xlab(x_lab) +
      ylab(y_lab) +
      scale_fill_viridis_c()
    p_alpha_recovery_density %>% save_svg_figure(sprintf("p_%s_recovery_density", param),
      analysis_group = "parameter_recovery",
      scaling = fig_scale,
      width = fig_anova_width,
      height = fig_anova_height,
      unit = "mm"
    )
    print(param)
    print(cor(data_df %>% pull(!!sym(y_param)), data_df %>% pull(!!sym(param))))
  }
}

file_lists <- list.files(here::here("model_based_analysis", "R_result/"), pattern = "binary_sign.*recovery_iter_100.rds", full.names = TRUE)
file_lists

# cl_i <- makeCluster(min(getOption("mc.cores", detectCores()),  12), outfile = "")
# registerDoParallel(cl_i)

res <- foreach(
  filename = file_lists,
  .combine = list,
  .packages = c(
    "gridExtra",
    "ggpubr",
    "doParallel",
    "tidyverse",
    "magrittr",
    "ggplot2",
    "ggcorrplot",
    "rio",
    "reshape2"
  )
) %do% {
  print(filename)
  visualize_correlation(filename, corr_method = "spearman")
}

# stopCluster(cl_i)
