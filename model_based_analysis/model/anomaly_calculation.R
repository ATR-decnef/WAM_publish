calc_prob_center = function(x, y, meanx, meany, varx, vary, cov){
  res = mapply(
    function(x,y, meanx, meany, varx, vary, cov) {
      sigma = matrix(c(varx, cov, cov, vary), nrow = 2, ncol = 2)
      # mvtnorm::dmvnorm(c(x, y), mean = c(0, 0), sigma= sigma)
      # (c(x - meanx, y - meany) %*% t(sigma) %*% c(x - meanx, y - meany))[[1]]
      (t(c(x - meanx, y - meany)) %*% solve(sigma) %*% c(x - meanx, y - meany))[[1]]
    },
    x, y, meanx, meany, varx, vary, cov) %>% unlist()
  res
}

add_anomaly_col = function(df_rule_hit, df_score_hit){
  tmp = df_score_hit %>% 
    mutate(PlayerID = as.factor(PlayerID)) %>% 
    group_by(PlayerID) %>% 
    summarise(meanx = mean(LocX), meany = mean(LocY), varx = var(LocX), vary = var(LocY), cov = cov(LocX, LocY)) 
  
  res = df_rule_hit %>% inner_join(tmp, by="PlayerID") %>% 
    mutate(anomaly_score = calc_prob_center(LocX, LocY, meanx, meany, varx, vary, cov)) %>% 
    group_by(PlayerID) %>% 
    mutate(normalized_anomaly_score = anomaly_score / max(anomaly_score),
           anomaly_error = (1 - normalized_anomaly_score) - DisplayScore) %>% ungroup()
  res
}

# df_score_hit %>% 
#   mutate(PlayerID = as.factor(PlayerID)) %>% 
#   group_by(PlayerID) %>% 
#   summarise(meanx = mean(LocX), meany = mean(LocY), varx = var(LocX), vary = var(LocY), cov = cov(LocX, LocY)) %>% inner_join(df_rule_hit, by="PlayerID") %>% 
#   mutate(prob_center_with_norm = prob_center(LocX, LocY, meanx, meany, varx, vary, cov)) %>% 
#   ggplot(aes(x = LocX, y = LocY, color = prob_center_with_norm)) + geom_point()
# 
# 
# df_score_hit %>% 
#   mutate(PlayerID = as.factor(PlayerID)) %>% 
#   group_by(PlayerID) %>% 
#   summarise(meanx = mean(LocX), meany = mean(LocY), varx = var(LocX), vary = var(LocY), cov = cov(LocX, LocY)) %>% inner_join(df_rule_hit, by="PlayerID") %>% 
#   mutate(prob_center_with_norm = prob_center(LocX, LocY, meanx, meany, varx, vary, cov)) %>% group_by(PlayerID) %>% mutate(prob_center_with_norm = prob_center_with_norm / max(prob_center_with_norm)) %>% process_for_switch(prob_center_with_norm, E_val) %>% ggplot(aes(x = `num of trials`, y = E_val, group = switch)) + stat_summary(geom = "line", stat = mean) + coord_cartesian(xlim = c(-7, 7))
# 
# df_score_hit %>% 
#   mutate(PlayerID = as.factor(PlayerID)) %>% 
#   group_by(PlayerID) %>% 
#   summarise(meanx = mean(LocX), meany = mean(LocY), varx = var(LocX), vary = var(LocY), cov = cov(LocX, LocY)) %>% inner_join(df_rule_hit, by="PlayerID") %>% 
#   mutate(prob_center_with_norm = prob_center(LocX, LocY, meanx, meany, varx, vary, cov)) %>% group_by(PlayerID) %>% mutate(prob_center_with_norm = prob_center_with_norm / max(prob_center_with_norm)) %>% 
#   ggplot(aes(x = prob_center_with_norm)) + geom_density() + geom_density(aes(group = PlayerID, color = PlayerID))
# 
# df_score_hit %>% 
#   mutate(PlayerID = as.factor(PlayerID)) %>% 
#   group_by(PlayerID) %>% 
#   summarise(meanx = mean(LocX), meany = mean(LocY), varx = var(LocX), vary = var(LocY), cov = cov(LocX, LocY)) %>% inner_join(df_rule_hit, by="PlayerID") %>% 
#   mutate(prob_center_with_norm = prob_center(LocX, LocY, meanx, meany, varx, vary, cov)) %>% 
#   ggplot(aes(x = LocX/ threshold, y = LocY / threshold, color = prob_center_with_norm)) + geom_point() +facet_wrap(.~PlayerID)
