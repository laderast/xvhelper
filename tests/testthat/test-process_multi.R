test_that("map_data_frame works", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  coded_col_df |> dplyr::filter(is_sparse_coding != "yes")
  out_frame <- map_data_frame(cohort, coded_col_df, drop_sparse=TRUE)

  expect_true(is.data.frame(out_frame))
})



test_that("decoding columns works",{

  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  out_frame <- decode_column_names(cohort, coded_col_df)
  expect_equal(ncol(out_frame), ncol(cohort))}
  )
