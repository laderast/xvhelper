test_that("decode multi-values", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  test_frame <- cohort |> dplyr::select(participant.p41202)

  out <- test_frame |>
    decode_multi(coded_col_df)

  expect_true(is.data.frame(out))
  expect_equal(colnames(test_frame), colnames(out))
})

test_that("decode multi-values duckdb", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  test_frame <- cohort |> dplyr::select(participant.p41202)

  out <- test_frame |>
    decode_multi_db(coded_col_df)

  expect_true(is.data.frame(out))
  expect_equal(colnames(test_frame), colnames(out))
})


test_that("decode multi from database", {

  load("data_dict.rda")
  load("coding_dict.rda")
  load("mydf.rda")

  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  out <- mydf |> dplyr::select(participant.p41202) |> decode_multi(coded_col_df)

  expect_true(is.data.frame(out))
})
