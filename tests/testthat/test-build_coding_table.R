test_that("build_coding_table_works", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  build_coding_table(coded_col_df)
})
