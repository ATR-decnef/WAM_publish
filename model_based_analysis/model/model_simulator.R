library(stringr)

action_generator_factory = function(df, state_col, score_col, action_col, ...){
  df_sample = df %>% select({{state_col}}, {{score_col}}, {{action_col}}) %>% 
    group_by({{state_col}}, {{score_col}}) %>% 
    nest() %>% 
    mutate(samples = map(data, ~ pull(.x, {{action_col}})))
  # print(df_sample)
  
  state_factors = df_sample %>% pull({{state_col}}) %>% unique()
  score_factors = df_sample %>% pull({{score_col}}) %>% unique()
  stopifnot(length(state_factors) == 2)
  stopifnot(length(score_factors) == 2)
  # print(state_factors)
  # print(score_factors)

  # 空のリストを作成
  empirical_dist <- list()
  
  # データを格納
  empirical_dist <- map(state_factors, 
                        function(state){
                          d = map(score_factors, 
                              function(score){
                                data = df_sample %>% 
                                  filter({{state_col}} == state & {{score_col}} == score) %>% 
                                  pull(samples)
                                stopifnot(length(data) == 1)
                                data = data[[1]]
                                stopifnot(typeof(data) == "double")
                                stopifnot(!any(is.na(data)))
                                stopifnot(!any(is.null(data)))
                                stopifnot(!any(is.nan(data)))
                                data
                              }
                              )
                          names(d) = score_factors
                          d
                          }
                        )
  names(empirical_dist) = state_factors
  force(empirical_dist)
  # print(empirical_dist %>% View())
  # sample_skill
  # density_estimation <- density(data, ...)
  # sampled <- sample(density_estimation$x, size = 10, replace = TRUE, prob = density_estimation$y)
  # print(sampled)
  
  
  generator = function(prev_state, prev_score, trial = NA) {
    prev_state = as.character(prev_state)
    prev_score = as.character(prev_score)
    
    sample(empirical_dist[[prev_state]][[prev_score]], replace = TRUE, size = 1)
  }
}

score_generator_factory = function(true_threshold){
  theta = true_threshold
  
  score_generator = function(distance, is_skill){
    # print(paste0("is_skill is ", is_skill))
    if(mode(is_skill) == "numeric"){
      is_skill = is_skill == 1
    } else if(mode(is_skill) == "character"){
      is_skill = sapply(X = is_skill, FUN = function(x) str_detect(x, pattern = "[s|S]kill"))
    }
    stopifnot(typeof(is_skill) == "logical")
    
    if_else(is_skill,
            if_else(distance <= theta, 1, 
                    0),
            sample(c(1, 0), size = 1, replace = TRUE)
    )
  }
}


generate_state_seq <- function(
    trials, blocks, init = sample(c(0, 1), 1), min_switch_perBlock = 0, max_switch_perBlock = Inf, 
    mu = 30, cov = 40, ch = c("random", "skill")) {
  
  shape = mu^2 / cov
  scale = cov / mu
  
  
  v = 0
  itr = 0
  while(!(0.45 < v & v < 0.55)){
    cells <- c()
    rules <- c()
    
    for (i in seq_len(blocks)) {
      length <- 0
      r <- init
      gammas <- rgamma(50, shape = shape, scale = scale)
      end <- FALSE
      c <- 0
      
      for (g in gammas) {
        l <- floor(g) # 整数に変換
        
        if (length + l > trials) {
          l <- trials - length
          end <- TRUE
        }
        
        cells <- c(cells, l)
        c <- c + 1
        
        rules <- c(rules, rep(r, l))
        length <- length + l
        
        # 状態の切り替え
        r <- ifelse(r == ch[1], ch[2], ch[1])
        
        if (end) {
          break
        }
      }
      
      # cat(sprintf("num of cells: %d\n", c))
      # cat(sprintf("mean: %f\n", mean(cells)))
      # cat(sprintf("std: %f\n", sd(cells)))
      
      # ヒストグラムのプロット
      hist(cells, main = sprintf("Block %d", i), xlab = "Cell length", ylab = "Frequency")
    }
    
    v = mean(rules == ch[[1]])
    print(paste0("balance: ", v))
    itr = itr + 1
    stopifnot(itr < 100)
  }
  
  return(rules)
}



accmulater_factory = function(ferror, z_accum, policy){
  force(ferror)
  force(z_accum)
  force(policy)
  
  accumulator = function(action, state, score, param, theta, z_prev){
    error = ferror(action, state, score, theta)
    
    z = z_accum(error, score, param, z_prev)
    choice = policy(z, param)
    list(choice = choice , z = z)
  }
}

binary_accumulator = function(action, state = NA, score, param, z_prev){
  print(param)
  stopifnot(is.numeric(score))
  alpha = if_else(score > 0, param$alpha_pos, param$alpha_neg)
  beta = if_else(score > 0, param$beta_pos, param$beta_neg)
  gamma = param$gamma
  theta = param$theta
  
  is_good = action < theta
  error = abs(is_good - score)
  
  z = alpha * error + gamma * z_prev
  choice = sigmoid(z + beta)
  # print(paste0("score is: ", score, ", is_good is: ", is_good, ", error is: ", error))
  list(z = z, choice = choice)
}


accum_loop = function(z_accumulator, 
                      score_generator, 
                      action_generator, 
                      param, 
                      z_0, 
                      state_0, 
                      score_0, 
                      true_states){
  prev_z = z_0
  prev_score = score_0
  prev_state = state_0
  
  true_states
  
  foreach(trial = 1:length(true_states), .combine = function(acc, res) {
    # accとresの名前に基づいて処理を行う
    acc <- Map(function(name) {
      c(acc[[name]], res[[name]])
    }, names(res))
    
  }, .init = list(z = numeric(), choice = numeric(), score = numeric(), action = numeric(), state = character())) %do%{
    state = true_states[[trial]]
    # print(paste0("state is ", state))
    action = action_generator(prev_state, prev_score)
    # print(paste0("action is ", action))
    score = score_generator(action, is_skill = state)
    # print("call accumulator")
    list_z_choice = z_accumulator(action, state, score, param, z_prev = prev_z)
    
    z = list_z_choice$z
    choice = list_z_choice$choice
    prev_z = z
    prev_score = score
    prev_state = if_else(choice < 0.5, "skill", "random")
    
    list_z_choice$state = state
    list_z_choice$score = score
    list_z_choice$action = action
    list_z_choice$trial = trial
    list_z_choice
  }
}

log_lik_bernoulli = function(probs, behaviour){
  each_log_l = behaviour * clip_vectors(log(probs)) + (1 - behaviour) * clip_vectors(log(1 - probs))
  # print(sum(each_log_l))
  
  res = sum(each_log_l)
  if(is.na(res)){
    res = -10000
  }
  res
}
