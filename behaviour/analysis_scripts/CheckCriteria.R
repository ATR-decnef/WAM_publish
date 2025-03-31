library(ggrepel)

CheckCriteria <- function(rule_est_df){
  rule_est_df %>% 
    mutate(PlayerID = as.factor(PlayerID), TrueRule=as.numeric(str_replace_all(TrueRule, pattern = c("skill" = "1", "random" = "0"))), EstRule=as.numeric(str_replace_all(EstRule, pattern = c("skill" = "1", "random" = "0"))), Correct=as.numeric(str_replace_all(Correct, pattern = c("TRUE" = "1", "FALSE" = "0")))) %>% 
    group_by(PlayerID) %>% 
    summarise(acc = mean(Correct), Choice = mean(EstRule)) %>% 
    ggplot(aes( x = `Choice`, y = `acc` ) ) + 
    stat_summary(fun = "mean", geom = "point", aes(group = PlayerID, color = PlayerID), alpha= 0.8) + 
    geom_hline(yintercept = 0.54) + 
    geom_vline(xintercept = 0.2) + 
    geom_vline(xintercept = 0.8) + 
    geom_label_repel(mapping=aes(label=PlayerID), size = 3)

}

listIncludedParticipants <- function(df_rule_hit, acc_criteria = 0.54, conf_criteria = 0.8){
  cr_df = df_rule_hit %>%
    mutate(TrueRule=as.numeric(str_replace_all(TrueRule, pattern = c("skill" = "1", "random" = "0"))), EstRule=as.numeric(str_replace_all(EstRule, pattern = c("skill" = "1", "random" = "0"))), Correct=as.numeric(str_replace_all(Correct, pattern = c("TRUE" = "1", "FALSE" = "0")))) %>%
    group_by(PlayerID) %>%
    summarise(acc = mean(Correct), Choice = mean(EstRule))
  pass_choice_acc_list = (cr_df %>% filter(acc >= acc_criteria))$PlayerID
  pass_choice_ratio_list = (cr_df %>% filter((Choice >= 0.2 & Choice <= 0.8)))$PlayerID
  
  conf_cr_df = df_rule_hit %>% 
               mutate(TrueRule=as.numeric(str_replace_all(TrueRule, pattern = c("skill" = "1", "random" = "0"))), EstRule=as.numeric(str_replace_all(EstRule, pattern = c("skill" = "1", "random" = "0"))), Correct=as.numeric(str_replace_all(Correct, pattern = c("TRUE" = "1", "FALSE" = "0")))) %>%
               group_by(PlayerID) %>% 
               count(EstRuleConfidence) %>% mutate(ratio = n/sum(n)) %>% 
               group_by(PlayerID) %>% nest() %>% 
               mutate( cr = as.vector(map(.x=data, .f=~all(.x$ratio < conf_criteria)))) %>% unnest(c(data, cr))
  
  pass_conf_list = (conf_cr_df %>% filter(cr))$PlayerID
  
  # pass_list = intersect(pass_choice_list, pass_conf_list)
  
  return(list(pass_choice_acc_list, pass_choice_ratio_list,pass_conf_list))
}

excludeDfRuleHit = function(df_rule_hit_origin){
  
  list_of_included_list = listIncludedParticipants(df_rule_hit_origin)
  pass_choice_acc_list = list_of_included_list[[1]]
  pass_choice_ratio_list = list_of_included_list[[2]]
  pass_choice_list = intersect(pass_choice_acc_list, pass_choice_ratio_list)
  pass_conf_list = list_of_included_list[[3]]
  
  included_list = intersect(pass_choice_list, pass_conf_list)
  df_rule_hit_included = df_rule_hit_origin %>% filter(PlayerID %in% included_list)
  
  df_rule_hit_included
}

