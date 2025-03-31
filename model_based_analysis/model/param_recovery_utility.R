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

recover_paramters <- function(df_rule_hit, model_obj, iteration, input_col, output_rule, param_mean = NA, param_sd = NA) {
  foreach(
    p_ = df_rule_hit$PlayerID %>% unique(),
    .combine = rbind,
    .packages =
      c("foreach", "doParallel", "tidyverse")
  ) %:%
    foreach(
      itr = 1:iteration, .combine = rbind, .packages =
        c("foreach", "doParallel", "tidyverse")
    ) %dopar% {
      source(here::here("model_based_analysis", "model", "model_definition.R"))
      print(paste0("ID:", p_))
      each_df <- df_rule_hit %>% filter(PlayerID == p_)

      error <- each_df %>%
        select({{ input_col }}) %>%
        as.vector() %>%
        unlist() %>%
        abs()
      is_good <- each_df$DisplayScore
      behaviour <- each_df$EstRule == output_rule
      ext_params <- is_good

      if (any(is.na(param_mean))) {
        true_param <-
          as.vector(
            runif(
              model_obj$num_of_params,
              min = model_obj$lower_range,
              max = model_obj$upper_range
            )
          )
      } else {
        true_param <-
          map2(param_mean, param_sd, ~ rnorm(1, .x, .y)) %>%
          unlist() %>%
          as.vector() %>%
          pmin(model_obj$upper_range) %>%
          pmax(model_obj$lower_range)
      }

      sim_pred <- model_obj$predict_choice(error, true_param, ext_params)

      sim_choice <- foreach(pr = sim_pred, .combine = c) %do% {
        sim_seq <- rbinom(1, 1, pr)
      }

      res <- model_obj$mle(error, sim_choice, ext_params = ext_params)
      print(p_ %>% as.character() %>% as.numeric() %>% as.integer())
      for_res <- c(as.integer(as.character(p_)), itr, true_param, res$par)
      names(for_res) <- c("PlayerID", "iteration", model_obj$param_name_list, paste0("res_", model_obj$param_name_list))
      for_res
    }
}

create_param_recovery_figs <- function(filename) {
  data_name <- sub("\\.[^.]*", "", filename)
  recovered_result <- import(here::here("model_based_analysis", filename))

  pdf(sprintf("%s_all.pdf", data_name), width = 10, height = 10, title = data_name)

  (recovered_result %>%
    as_tibble() %>%
    select(-PlayerID, -iteration) %>%
    ggpairs() + theme_tufte()) %>% print()
  dev.off()
}
recovery_paramters <- function(model_obj, input, behaviour, true_param, ext_params) {
  # true_param = runif(model_obj$num_of_params, max = model_obj$upper_range, min = model_obj$lower_range)
  if (missing(ext_params)) {
    sim_pred <- model_obj$predict_choice(input, true_param)
    ext_params <- NA
  } else {
    sim_pred <- model_obj$predict_choice(input, true_param, ext_params)
  }
  sim_choice <- foreach(p = sim_pred, .combine = c) %do% {
    sim_seq <- rbinom(1, 1, p)
  }

  res <- model_obj$mle(input, sim_seq, ext_params = ext_params)
  res$par
}


check_corr <- function(model_obj, input) {
  true_param <- runif(model_obj$num_of_params, max = model_obj$upper_range, min = model_obj$lower_range)
  res_df <- foreach(p = 1:100, .combine = rbind) %do% {
    true_param <- runif(model_obj$num_of_params, max = model_obj$upper_range, min = model_obj$lower_range)
    rec_param <- recovery_paramters(model_obj, input, behaviour, true_param)
    c(true_param, rec_param)
  }
  res_df %>% cor()
}

recovery_model <- function(df, model_obj, model_name, input_col, output_rule, iteration_of_recovery, param_mean = NA, param_sd) {
  # model_name = rlang::expr_text(rlang::enexpr(model_obj))
  print(model_name)

  recovered_result <- recover_paramters(df, model_obj, iteration_of_recovery, input_col, output_rule, param_mean = param_mean, param_sd = param_sd)
  est_result_filename <- sprintf("R_result/%s_%s_%s", model_name, input_col, output_rule)
  export(recovered_result, here::here("model_based_analysis", sprintf("%s_parameter_recovery_iter_%d.rds", est_result_filename, iteration_of_recovery)))
  export(recovered_result, here::here("model_based_analysis", sprintf("%s_parameter_recovery_iter_%d.csv", est_result_filename, iteration_of_recovery)))
}
