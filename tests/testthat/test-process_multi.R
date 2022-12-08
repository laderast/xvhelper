test_that("decoding column names works",{

  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  cohort2 <- cohort[,1:2]
  out_frame <- decode_column_names(cohort2, coded_col_df)
  expect_equal(ncol(out_frame), ncol(cohort2))

  cohort3 <- data.frame(fake_col="blah", nrow=10)
  out_frame2 <- decode_column_names(cohort3, coded_col_df)
  expect_equal(colnames(cohort3), colnames(out_frame2))}
  )

