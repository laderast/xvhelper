test_that("decode multi-values", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  test_frame <- cohort |> dplyr::select(participant.p41202)

  out <- decode_multi(test_frame, coded_col_df)

  expect_true(is.data.frame(out))
  expect_equal(colnames(test_frame), colnames(out))
})

