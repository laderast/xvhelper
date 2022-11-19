test_that("decoding columns works",{

  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  out_frame <- decode_column_names(cohort, coded_col_df)
  expect_equal(ncol(out_frame), ncol(cohort))}
  )
