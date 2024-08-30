save_results_table_as_html <- function(results_table, points_per_position, file_path) {
  gt_table <- results_table %>%
    gt() %>%
    fmt_number(columns = c(Positie, Punten), decimals = 1, sep_mark = ".", dec_mark = ",") %>%
    fmt_number(columns = Rating, decimals = 2, sep_mark = ".", dec_mark = ",") %>%
    fmt_percent(columns = colnames(results_table)[5 : ncol(results_table)], 
                decimals = 0) %>%
    cols_align(align = "center", columns = colnames(results_table)[-1]) %>% 
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(columns = everything())) %>%
    tab_options(table.font.size = "small", column_labels.font.size = "medium", table.align = "left")
  for(position in seq_along(points_per_position)) {
    gt_table <- gt_table %>%
      tab_spanner(label = gsub("\\.", ",", as.character(points_per_position[position])), columns = colnames(results_table)[position + 4])
  }
  gtsave(gt_table, file_path)
}