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

sigmoid <- function(x, a = 1) {
  return(1 / (1 + exp(-a * x)))
}

step <- function(x) {
  as.numeric(x > 0)
}

fit_all_participants <- function(df_rule_hit, model_obj, input_col, output_rule) {
  # est_result <- foreach(p_ = df_rule_hit$PlayerID %>% unique(), .combine = rbind, .packages = c("foreach", "doParallel", "tidyverse")) %dopar% {
  est_result <- foreach(p_ = df_rule_hit$PlayerID %>% unique(), .combine = rbind, .packages = c("foreach", "doParallel", "tidyverse")) %do% {
    source(here::here("model_based_analysis", "model/model_definition.R"))

    # print(paste0("IDa:", p_))
    each_df <- df_rule_hit %>% filter(PlayerID == p_)
    theta <- each_df %>%
      pull(threshold) %>%
      unique()
    stopifnot(length(theta) == 1)

    error <- each_df %>%
      select({{ input_col }}) %>%
      as.vector() %>%
      unlist()
    is_good <- each_df$DisplayScore
    behaviour <- each_df$EstRule == output_rule

    res <- model_obj$mle(
      input = error,
      behaviour = behaviour,
      num_iter = 10,
      ext_params = is_good,
      theta = theta
    )
    # print(paste0("IDbS:", p_))
    c(as.numeric(as.character(p_)), model_obj$num_of_params, res$par, res$value, calc_AIC(res$value, model_obj$num_of_params))
    # print(class(p_))
  }
  # print(est_result)
  colnames(est_result) <- c("PlayerID", "number_of_params", model_obj$param_name_list, "log_likelihood", "AIC")
  est_result
}

estimate_model <- function(df, model_obj, model_name, input_col, output_rule, suffix = "") {
  # model_name = rlang::expr_text(rlang::enexpr(model_obj))
  print(model_name)

  if (nzchar(suffix)) {
    suffix <- paste0("_", suffix)
  }

  est_result_filename <- sprintf("R_result/%s_%s_%s%s", model_name, input_col, output_rule, suffix)

  est_result <- fit_all_participants(df, model_obj, input_col, output_rule)
  export(est_result, here::here("model_based_analysis", sprintf("%s_estimation.rds", est_result_filename)))
  export(model_obj, here::here("model_based_analysis", sprintf("%s_model.rds", est_result_filename)))

  est_result
}


inv.logit <- function(x) plogis(x)
logit <- function(x) qlogis(x)


log_lik_bernoulli <- function(params, input, behaviour, pred) { # pred(input, params), behaviour should be in [0, 1]
  probs <- pred(input, params)
  each_log_l <- behaviour * clip_vectors(log(probs)) + (1 - behaviour) * clip_vectors(log(1 - probs))
  # print(sum(each_log_l))

  res <- sum(each_log_l)
  if (is.na(res)) {
    res <- -10000
  }
  res
}

maximize_liklihood <- function(input, behaviour, pred, num_param, max_values, min_values, func_liklihood = log_lik_bernoulli, num_iter = 10) {
  # cl_i <- makeCluster(2, outfile = "")
  # registerDoParallel(cl_i)
  # on.exit(stopCluster(cl_i))

  ui_ <- as.matrix(rbind(diag(-1, nrow = num_param, ncol = num_param), diag(1, nrow = num_param, ncol = num_param)))
  ci_ <- as.vector(c(-max_values, min_values))

  res <- foreach::foreach(i = 1:num_iter, .combine = "rbind") %do% {
    # print(paste("itr", i))
    for (i in 1:100) {
      print(paste0("optim itr #", i, " started"))
      init_value <- as.vector(runif(num_param, min = min_values, max = max_values))
      res_ <-
        try(
          constrOptim(
            init_value,
            func_liklihood,
            grad = NULL,
            ui = ui_,
            ci = ci_,
            input = input,
            behaviour = behaviour,
            pred = pred,
            control = list(fnscale = -1)
            # method = "Nelder-Mead"
          ),
          silent = TRUE
        )
      if (class(res_) == "try-error") {
        res_ <- NULL
        res_$value <- -Inf
        res_$par <- init_value / 100
      } else {
        if (all(res_$par >= min_values & res_$par <= max_values)) {
          break
        } else {
          res_$value <- -Inf
        }
      }
    }
    res_
  }
  index <- which.max(res[, "value"])
  res_min <- res[index, ]
  # print(res_min)
  stopifnot(all(res_min$par >= min_values & res_min$par <= max_values))
  print(paste0("precise optime started"))
  res_opt <- constrOptim(
    as.vector(res_min$par),
    func_liklihood,
    grad = NULL,
    ui = ui_,
    ci = ci_,
    input = input,
    behaviour = behaviour,
    pred = pred,
    control = list(fnscale = -1)
    # method = "Nelder-Mead"
  )
  stopifnot(all(res_opt$par > min_values & res_opt$par < max_values))

  res_opt
}

calc_AIC <- function(log_lik, num_params) {
  -2 * log_lik + 2 * num_params
}



AccumulationModelClass <-
  R6Class("AccumulationModelClass",
    public = list(
      model_name = NA,
      num_of_params = NA,
      upper_range = NA,
      lower_range = NA,
      param_name_list = NA,
      initialize = function(num_of_params, upper_range, lower_range, predict_choice, param_name_list) {
        self$num_of_params <- num_of_params
        self$upper_range <- upper_range
        self$lower_range <- lower_range
        self$param_name_list <- param_name_list
        self$predict_choice <- predict_choice
      },
      predict_choice = NA, # pred(input, params)
      calc_loglik =
        function(input, behaviour, params) log_lik_bernoulli(input, behaviour, self$predict_choice, params),
      mle =
        function(input, behaviour, num_iter = 10, ext_params, theta = NA) {
          predict_choice <-
            if (!("threshold" %in% self$param_name_list)) {
              print("no threshold ")
              stopifnot(!is.na(theta))
              function(input, params) {
                params[length(params) + 1] <- theta
                self$predict_choice(input, params, ext_params)
              }
            } else {
              print("threshold exists")
              print(self$param_name_list)
              function(input, params) {
                self$predict_choice(input, params, ext_params)
              }
            }

          maximize_liklihood(
            input,
            behaviour,
            predict_choice,
            self$num_of_params,
            self$upper_range,
            self$lower_range,
            num_iter = num_iter
          )
        }
    )
  )

clip_vectors <- function(value, large = 10^300) {
  value[value == Inf] <- large
  value[value == -Inf] <- -large
  return(value)
}

clip_probs <- function(value, delta = 10^-300) {
  value[value == 0] <- delta
  value[value == 1] <- 1 - delta

  return(value)
}
