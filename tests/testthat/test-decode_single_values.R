
test_that("merge coding data dict",{
  load("coding_dict.rda")
  load("data_dict.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  expect_true(is.data.frame(coded_col_df))
  expect_equal(9, ncol(coded_col_df))
})


test_that("decode single", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  cohort2 <- cohort |> dplyr::select(participant.p100240_i0, participant.p1508_i0)
  out_frame <- decode_single(cohort2, coded_col_df)
  expect_equal(colnames(cohort2), colnames(out_frame))
})


test_that("decode single from DB", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("mydf2.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  cohort2 <- mydf2 |> dplyr::select(participant.p31, participant.p1508_i0)
  out_frame <- decode_single(cohort2, coded_col_df)
  expect_equal(colnames(cohort2), colnames(out_frame))
})
