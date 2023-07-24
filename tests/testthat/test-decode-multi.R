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

test_that("decode multi-values purrr", {
  load("data_dict.rda")
  load("coding_dict.rda")
  load("cohort.rda")
  coded_col_df <- merge_coding_data_dict(coding_dict, data_dict)
  test_frame <- cohort |> dplyr::select(participant.p41202)

  out <- test_frame |>
    decode_multi_purrr(coded_col_df)

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

test_that("decode multi large df", {
  load("data_dict.rda")
  load("icd10new_code.rda")
  load("icd10_new.rda")

  coded_col_df <- merge_coding_data_dict(coding_df, data_dict)
  out <- small_icd10 |>
    decode_multi_large_df(coded_col_df, df_size = 5)

  expect_true(is.data.frame(out))
})

test_that("detect lists from character vectors", {

  load("icd10_new.rda")

  output <- detect_list(small_icd10)
  expect_equal(TRUE, output$participant.p41202)
})
