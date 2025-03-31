library(pROC)
res %>% 
  ungroup() %>% 
  group_by(PlayerID, input) %>% 
  nest() %>% 
  mutate(auc = 
           map(.x = data,  
               .f = ~roc(behaviour ~ pred, data = .x) %>% `$`(auc) %>% as.numeric()
               )
         ) %>% 
  unnest(cols=c(auc)) %>% 
  ggplot(aes(x = input, y = auc, group = input)) + 
  geom_boxplot() + 
  geom_sina()

res %>% 
  ungroup() %>% 
  group_by(PlayerID, input) %>% 
  nest() %>% 
  mutate(cross_entropy = 
           map(.x = data,  
               .f = ~ .x %>% summarise(ent = -mean(behaviour * log(pred) + (1 - behaviour) * log(1 - pred))) %>% pull(ent)
           )
  ) %>% 
  unnest(cols=c(cross_entropy)) %>% 
  ggplot(aes(x = input, y = cross_entropy, group = input)) + 
  geom_boxplot() + 
  geom_sina()
