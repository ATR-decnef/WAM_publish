source(here::here("model_based_analysis", "model", "model_utility.R"))
# models (functions) ----
alpha_GB_beta_GB_gamma_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # browser()
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 5)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha_G = params[1]
  alpha_B = params[2]
  beta_G = params[3]
  beta_B = params[4]
  gamma = params[5]
  
  z = c(NA)
  bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha_B * error[[i]]
    bias[i] = beta_B
    
  }else{
    z[[i]] = alpha_G * error[[i]]
    bias[i] = beta_G
    
  }
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha_B * error[[i]] + gamma * z[i-1]
      bias[i] = beta_B
      
    }else{
      z[i] = alpha_G * error[[i]] + gamma * z[i-1]
      bias[i] = beta_G
      
    }
  }
  probs =inv.logit(z + bias)
  probs
}

alpha_GB_beta_gamma_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 4)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha_G = params[1]
  alpha_B = params[2]
  beta = params[3]
  gamma = params[4]
  
  z = c(NA)
  bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha_B * error[[i]]
    bias[i] = beta
    
  }else{
    z[[i]] = alpha_G * error[[i]]
    bias[i] = beta
    
  }
  
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha_B * error[[i]] + gamma * z[i-1]
      bias[i] = beta
      
    }else{
      z[i] = alpha_G * error[[i]] + gamma * z[i-1]
      bias[i] = beta
      
    }
  }
  probs =inv.logit(z + bias)
  probs
}

alpha_beta_GB_gamma_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 4)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha = params[1]
  beta_G = params[2]
  beta_B = params[3]
  gamma = params[4]
  
  z = c(NA)
  bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha * error[[i]]
    bias[i] = beta_B
    
  }else{
    z[[i]] = alpha * error[[i]]
    bias[i] = beta_G
    
  }
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha * error[[i]] + gamma * z[i-1]
      bias[i] = beta_B
      
    }else{
      z[i] = alpha * error[[i]] + gamma * z[i-1]
      bias[i] = beta_G
      
    }
  }
  probs =inv.logit(z + bias)
  probs
}

alpha_GB_beta_GB_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 4)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha_G = params[1]
  alpha_B = params[2]
  beta_G = params[3]
  beta_B = params[4]
  gamma = 0
  
  z = c(NA)
  bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha_B * error[[i]]
    bias[i] = beta_B
    
  }else{
    z[[i]] = alpha_G * error[[i]]
    bias[i] = beta_G
    
  }
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha_B * error[[i]] + gamma * z[i-1]
      bias[i] = beta_B
      
    }else{
      z[i] = alpha_G * error[[i]] + gamma * z[i-1]
      bias[i] = beta_G
      
    }
  }
  probs =inv.logit(z + bias)
  probs
}

alpha_beta_gamma_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 3)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha = params[1]
  beta = params[2]
  gamma = params[3]
  
  z = c(NA)
  bias = c(NA)
  i = 1
  
  z[i] = alpha * error[[i]]
  bias[i] = beta
  
  for(i in 2:length(error)){
    z[i] = alpha * error[[i]] + gamma * z[i-1]
    bias[i] = beta
  }
  probs =inv.logit(z + bias)
  probs
}

alpha_GB_gamma_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 3)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha_G = params[1]
  alpha_B = params[2]
  gamma = params[3]
  
  z = c(NA)
  # bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha_B * error[[i]]
    # bias[i] = beta_B
    
  }else{
    z[[i]] = alpha_G * error[[i]]
    # bias[i] = beta_G
    
  }
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha_B * error[[i]] + gamma * z[i-1]
      # bias[i] = beta_B
      
    }else{
      z[i] = alpha_G * error[[i]] + gamma * z[i-1]
      # bias[i] = beta_G
      
    }
  }
  probs =inv.logit(z)
  probs
}

alpha_GB_model = function(error, params, is_good){ # num of params should be 5, and error should be always in [-1, 1], is_good is true when that trial was good
  # stopifnot(all(error >= 0) & all(error <= 1) )
  # stopifnot(length(params) == 2)
  stopifnot(!(any(is_good != 0 & is_good != 1) ))
  stopifnot(length(error) == length(is_good))
  
  alpha_G = params[1]
  alpha_B = params[2]
  gamma = 1
  
  z = c(NA)
  # bias = c(NA)
  i = 1
  if(is_good[i] == 0){
    z[i] = alpha_B * error[[i]]
    # bias[i] = beta_B
    
  }else{
    z[[i]] = alpha_G * error[[i]]
    # bias[i] = beta_G
    
  }
  for(i in 2:length(error)){
    if(is_good[i] == 0){
      z[i] = alpha_B * error[[i]] + gamma * z[i-1]
      # bias[i] = beta_B
      
    }else{
      z[i] = alpha_G * error[[i]] + gamma * z[i-1]
      # bias[i] = beta_G
      
    }
  }
  probs =inv.logit(z)
  probs
}

binary_model_wrapper = function(distance, 
                                params, 
                                is_good, 
                                center_prob_func = function(x, threshold){
                                  1 - step(x - threshold)
                                }, 
                                model = alpha_GB_beta_GB_gamma_model){
  stopifnot(class(center_prob_func) == "function")
  stopifnot(all(distance >= 0 ))
  # browser()

  threshold = params[length(params)]
  
  stopifnot(all(threshold >= 0))
  
  error = abs(center_prob_func(distance, threshold) - is_good)
  
  model(error, params[1:(length(params) - 1)], is_good)
}

# single_alpha_binary

# models (class objects) ----

# alpha_GB_beta_GB_gamma_sign_model_obj = 
#   AccumulationModelClass$new(
#     5, 
#     c(20, 20, 20, 20, 1), 
#     c(-20, -20, -20, -20, 0), 
#     alpha_GB_beta_GB_gamma_model,
#     c("a_G", "a_B", "b_G", "b_B", "gamma"))
# 
binary_sign_model_obj = 
  AccumulationModelClass$new(
    6, 
    c(20, 20, 20, 20, 1, 43), 
    c(-20, -20, -20, -20, 0, 0), 
    binary_model_wrapper,
    c("a_G", "a_B", "b_G", "b_B", "gamma", "threshold"))

binary_sign_no_gamma_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 20, 43), 
    c(-20, -20, -20, -20, 0), 
    function(...) binary_model_wrapper(..., model = alpha_GB_beta_GB_model),
    c("a_G", "a_B", "b_G", "b_B", "threshold"))

binary_sign_true_theta_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 20, 1), 
    c(-20, -20, -20, -20, 0), 
    binary_model_wrapper,
    c("a_G", "a_B", "b_G", "b_B", "gamma"))


binary_sign_common_alpha_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 1, 43), 
    c(-20, -20, -20, 0, 0), 
    function(...) binary_model_wrapper(..., model = alpha_beta_GB_gamma_model),
    c("a", "b_G", "b_B", "gamma", "threshold"))

binary_sign_common_beta_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 1, 43), 
    c(-20, -20, -20, 0, 0), 
    function(...) binary_model_wrapper(..., model = alpha_GB_beta_gamma_model),
    c("a_G", "a_B", "b", "gamma", "threshold"))

binary_sign_common_alpha_common_beta_model_obj = 
  AccumulationModelClass$new(
    4, 
    c(20, 20, 1, 43), 
    c(-20, -20, 0, 0), 
    function(...) binary_model_wrapper(..., model = alpha_beta_gamma_model),
    c("a", "b", "gamma", "threshold"))

binary_sign_true_theta_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 20, 1), 
    c(-20, -20, -20, -20, 0), 
    function(...) {
      parameters = list(...)
      parameters$params[length(parameters$params) + 1] = 
      binary_model_wrapper(...)
      },
    c("a_G", "a_B", "b_G","b_B", "gamma"))

alpha_GB_beta_GB_gamma_model_obj = 
  AccumulationModelClass$new(
    5, 
    c(20, 20, 20, 20, 1), 
    c(0, 0, -20, -20, 0), 
    alpha_GB_beta_GB_gamma_model,
    c("a_G", "a_B", "b_G", "b_B", "gamma"))

alpha_GB_beta_GB_model_obj = 
  AccumulationModelClass$new(
    4, 
    c(20, 20, 20, 20), 
    c(0, 0, -20, -20), 
    alpha_GB_beta_GB_model,
    c("a_G", "a_B", "b_G", "b_B"))

alpha_GB_beta_gamma_model_obj = 
  AccumulationModelClass$new(
    4,
    c(20, 20, 20, 1), 
    c(0, 0, -20, 0), 
    alpha_GB_beta_gamma_model,
    c("a_G", "a_B", "b", "gamma"))

alpha_beta_GB_gamma_model_obj = 
  AccumulationModelClass$new(
    4, 
    c(20, 20, 20, 1), 
    c(0, -10, -10, 0), 
    alpha_beta_GB_gamma_model,
    c("a", "b_G", "b_B", "gamma"))

alpha_beta_gamma_model_obj = 
  AccumulationModelClass$new(
    3, 
    c(20, 20, 1), 
    c(0, -20, 0), 
    alpha_beta_gamma_model,
    c("a", "b", "gamma"))

alpha_beta_gamma_sign_model_obj = 
  AccumulationModelClass$new(
    3, 
    c(20, 20, 1), 
    c(-20, -20, 0), 
    alpha_beta_gamma_model,
    c("a", "b", "gamma"))

alpha_GB_gamma_model_obj = 
  AccumulationModelClass$new(
    3, 
    c(20, 20, 1), 
    c(0, 0, 0), 
    alpha_GB_gamma_model,
    c("a_G", "a_B","gamma"))

alpha_GB_model_obj = 
  AccumulationModelClass$new(
    2, 
    c(20, 1), 
    c(0, 0), 
    alpha_GB_model,
    c("a_G", "a_B"))
