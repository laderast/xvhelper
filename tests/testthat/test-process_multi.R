test_that("map_data_frame works", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  coded_col_df |> dplyr::filter(is_sparse_coding != "yes")
  out_frame <- map_data_frame(cohort, coded_col_df, drop_sparse=TRUE)

  expect_true(is.data.frame(out_frame))
})

test_that("multi-column mapping works",{

  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  cohort2 <- cohort |> dplyr::select(participant.p21022, participant.p1508_i0, participant.p41202)
  coded_codl_df <- coded_col_df |> dplyr::filter(is_sparse_coding != "yes")
  out_frame <- decode_categories(cohort2, coded_col_df)
  expect_equal(ncol(out_frame), 3)

})
