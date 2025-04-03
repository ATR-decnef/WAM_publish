source(here::here("model_based_analysis", "model", "param_recovery_utility.R"))
# source(here::here("preprocess/MLE_preprocess.R"))
model_obj_list <- c(
  # alpha_GB_model_obj = alpha_GB_model_obj,
  # alpha_beta_gamma_model_obj = alpha_beta_gamma_model_obj,
  # alpha_beta_GB_gamma_model_obj = alpha_beta_GB_gamma_model_obj,
  # alpha_GB_beta_gamma_model_obj = alpha_GB_beta_gamma_model_obj,
  # alpha_GB_beta_GB_model = alpha_GB_beta_GB_model_obj,
  # alpha_GB_beta_gamma_model_obj = alpha_GB_beta_gamma_model_obj,
  # alpha_GB_beta_GB_gamma_accum_model_obj = alpha_GB_beta_GB_gamma_accum_model_obj,
  # alpha_GB_beta_GB_gamma_model_obj = alpha_GB_beta_GB_gamma_model_obj,
  # alpha_GB_beta_GB_gamma_accum_sign_model_obj = alpha_GB_beta_GB_gamma_accum_sign_model_obj,
  # alpha_GB_beta_GB_gamma_sign_model_obj = alpha_GB_beta_GB_gamma_sign_model_obj
  # alpha_GB_beta_GB_gamma_accum_sign_scaled_model_obj = alpha_GB_beta_GB_gamma_accum_sign_scaled_model_obj,
  # alpha_GB_beta_GB_gamma_sign_scaled_model_obj = alpha_GB_beta_GB_gamma_sign_scaled_model_obj
  binary_sign_model_obj = binary_sign_model_obj
)


# cluster <- makeCluster(min(10, getOption("mc.cores", detectCores())), outfile = "")
# registerDoParallel(cluster)

# for(col in c("state_error",
#              "scaledTransformedError",
#              "TransformedError",
#              "error_binary",
#              "error_binary_2x",
#              "error_binary_3x"))
model_name <- "binary_sign_model_obj"
col <- "Distance"


for (col in c("Distance")) {
  # for(col in c("logit_error", "scaled_logit_error")){
  print(col)
  for (model_name in names(model_obj_list)) {
    model_obj <- model_obj_list[[model_name]]
    print(model_name)

    file <-
      here::here("model_based_analysis", "R_result", paste0(model_name, "_", col, "_random_estimation.rds"))
    print(file)
    df_MLE_result_tmp <- import(file) %>% mutate(PlayerID = as.factor(PlayerID))
    print("file import done")
    df_param_stat_tmp <- df_MLE_result_tmp %>%
      pivot_longer(cols = c(-PlayerID, -number_of_params, -AIC, -log_likelihood), names_to = "param", values_to = "value") %>%
      group_by(param) %>%
      summarise(mean = mean(value), sd = sd(value)) %>%
      mutate(order_key = match(param, model_obj$param_name_list)) %>% # 並び替え用のキーを作成
      arrange(order_key) %>% # キーで並び替え
      select(-order_key) # キー列を削除
    param_mean <- df_param_stat_tmp %>% pull(mean)
    param_sd <- df_param_stat_tmp %>% pull(sd)

    recovery_model(df_rule_hit, model_obj, model_name, col, "random", 100, param_mean, param_sd)
  }
}
# stopCluster(cluster)



file_lists <- list.files("R_result/", pattern = "binary.*recovery_iter_100.rds", full.names = TRUE)

for (filename in file_lists) {
  print(filename)
  create_param_recovery_figs(filename)
}
