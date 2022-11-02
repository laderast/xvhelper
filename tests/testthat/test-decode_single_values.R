
test_that("merge coding data dict",{
  load("coding_dict.rda")
  load("data_dict.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  expect_true(is.data.frame(coded_col_df))
  expect_equal(9, ncol(coded_col_df))
})

test_that("decode single values", {
  load("coding_dict.rda")
  load("cohort.rda")
  load("data_dict.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  test_frame <- cohort |> dplyr::select(participant.p31)

  out_frame <- map_categories(column_name = "participant.p31",
                              df_to_convert = test_frame,
                              coded_col_df = coded_col_df)

  expect_true(is.data.frame(out_frame))
  expect_equal(nrow(test_frame), nrow(out_frame))
  expect_null(map_categories("blah", cohort, coded_col_df))

})

test_that("decode multi-values",{
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)

  test_frame <- cohort |> dplyr::select(participant.p41202)

  out_frame <- map_categories_multi_column(df_to_convert = test_frame,
                                    column_name = "participant.p41202",
                                     coded_col_df = coded_col_df
                                     )
  expect_equal(nrow(test_frame), nrow(out_frame))

})
