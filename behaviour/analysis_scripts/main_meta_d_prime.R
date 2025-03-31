
df_rule_hit_count = df_rule_hit %>% select(PlayerID, TrueRule, EstRule, EstRuleConfidence) %>% group_by(PlayerID, TrueRule, EstRule) %>% count(EstRuleConfidence)

df_rule_hit_metad = df_rule_hit_count %>% 
  mutate(EstRuleConfidence = as.factor(EstRuleConfidence)) %>% 
  pivot_wider( names_from = EstRuleConfidence, values_from = n) %>% 
  replace_na(replace=list(`1` = 0, `2` = 0, `3` = 0, `4` = 0)) %>% 
  group_by(PlayerID, TrueRule) %>% nest() %>% 
  mutate(new = map(data, ~group_by(.x, EstRule) %>% nest() %>% 
                     mutate(Slist = map(data, ~as.list(.x) %>%  `[`(order(names(.))))) %>% 
                     select(-data))) %>% unnest(cols="new")　%>% 
  select(-data) %>% 
  pivot_wider(names_from = EstRule, values_from = Slist) %>% 
  mutate(skill = map(skill, ~`[`(.x, order(names(.x), decreasing = T)))) %>% 
  mutate(concat = map2(.x=skill, .y=random, c)) %>% select(-skill, -random)%>% 
  mutate(concat = lapply(concat, unname) ) %>% 
  mutate(concat = lapply(concat, unlist))%>% 
  pivot_wider(names_from = TrueRule, values_from = concat) %>% group_by(PlayerID) %>% 
  mutate(d = map2(.x=skill, .y=random, .f=~fit_metad_indiv(.x, .y)))

df_rule_hit_metad_extract = df_rule_hit_metad %>% 
  mutate(d1 = d[[1]][[2]]$d1, output=d[[1]][1], Value=list(summary(d[[1]][[1]])), stat = list(data.frame(mean = Value[[1]][["statistics"]][, "Mean"]) %>% rownames_to_column(var = "name")), meta_dprime = stat[[1]]$mean[stat[[1]]$name == "meta_d"]) %>% select(-d)

MLE_result_df %>% mutate(PlayerID = as.factor(PlayerID)) %>% inner_join(df_rule_hit_metad_extract, by="PlayerID") %>% ggplot(aes(x=value, y = meta_dprime, group=PlayerID)) + geom_point() + facet_grid(.~param) + coord_cartesian(xlim=c(-5, 5))
df_rule_hit_metad_extract %>% mutate(main_meta_dprime = meta_dprime)%>% select(PlayerID, main_meta_dprime) %>% inner_join(meta_d_extract_df %>% mutate(rdm_meta_dprime = meta_dprime) %>% select(PlayerID, rdm_meta_dprime), by="PlayerID") %>% ggplot(aes(x = rdm_meta_dprime, y = main_meta_dprime)) + geom_point()
