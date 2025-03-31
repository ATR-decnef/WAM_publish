library(dplyr)
library(rlang)


# 切り替わり情報を追加
add_switch_count = function(df, state_col, state_list, count_on_switch = 0, count_before_switch = -1){
  stopifnot(length(state_list) == 2)
  
  state_shift_01 = paste0("shift_", state_list[[1]], "_to_", state_list[[2]])
  state_shift_10 = paste0("shift_", state_list[[2]], "_to_", state_list[[1]])
  
  trials_to_01 = paste0("trials_", state_list[[1]], "_to_", state_list[[2]])
  trials_to_10 = paste0("trials_", state_list[[2]], "_to_", state_list[[1]])
  
  group_01 = paste0("group_", state_list[[1]], "_to_", state_list[[2]])
  group_10 = paste0("group_", state_list[[2]], "_to_", state_list[[1]])
  
  df %>%
    mutate(
      # 状態の切り替えを検出
      !!state_shift_01 := (lag({{state_col}}, default = first({{state_col}})) == state_list[[1]] & {{state_col}} == state_list[[2]]),
      !!state_shift_10 := (lag({{state_col}}, default = first({{state_col}})) == state_list[[2]] & {{state_col}} == state_list[[1]]),
      
      # 0->1 の切り替え区間をカウント
      !!group_01 := cumsum(!!sym(state_shift_01)),
      
      # 1->0 の切り替え区間をカウント
      !!group_10 := cumsum(!!sym(state_shift_10))
    ) %>%
    group_by(!!sym(group_01), .add = TRUE) %>%
    mutate(
      # 0->1 の切り替えまでの残りトライアル数
      !!trials_to_01 := if_else(
        {{state_col}} == state_list[[1]],
        -(n() - row_number() - count_before_switch),  # 残りトライアルを負の数で計算
        row_number() - 1 + count_on_switch                # 経過トライアル数
      )
    ) %>%
    ungroup(!!sym(group_01)) %>%
    group_by(!!sym(group_10), .add = TRUE) %>%
    mutate(
      # 1->0 の切り替えまでの残りトライアル数
      !!trials_to_10 := if_else(
        {{state_col}} == state_list[[2]],
        -(n() - row_number()- count_before_switch),  # 残りトライアルを負の数で計算
        row_number() - 1 + count_on_switch                # 経過トライアル数
      )
    ) %>%
    ungroup(!!sym(group_10)) %>% 
    select(-!!sym(group_01), -!!sym(group_10), -!!sym(state_shift_01), -!!sym(state_shift_10))
}
