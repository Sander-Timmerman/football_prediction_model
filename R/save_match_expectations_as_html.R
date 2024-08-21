save_match_expectations_as_html <- function(match_expectations, file_path) {
  gt_table <- match_expectations %>%
    gt() %>%
    fmt_number(columns = c(Thuisgoals, Uitgoals), decimals = 2, sep_mark = ".", dec_mark = ",") %>%
    fmt_percent(columns = c(Thuiswinst, Gelijk, Uitwinst), decimals = 0) %>%
    cols_align(align = "center", columns = colnames(match_expectations)[3 : 8]) %>% 
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(columns = everything())) %>%
    tab_options(table.font.size = "small", column_labels.font.size = "medium", table.align = "left")
  gtsave(gt_table, file_path)
}