library(tidyverse)
library(ggplot2)
library(ggforce)
library(estimatr)
library(ggsignif)
load("R_result/meta_d_extract_df.rda")

meta_d_extract_df %>% inner_join(df_MLE_result_binary, by="PlayerID") %>% View()

meta_d_extract_df %>% 
  inner_join(df_MLE_result_binary, by="PlayerID") %>% 
  select(PlayerID, sub, name, performance, meta_dprime, log_likelihood, params, value) %>% 
  pivot_wider(names_from = params, values_from = value)%>% 
  select(-number_of_params) %>% 
  mutate(a_diff = a_G - a_B, b_diff = b_G - b_B) %>% 
  ggcorr(label = TRUE)