test_that("strip_field_ids works", {

  fields <- c("participant.eid", "participant.p31", "participant.p41202")

  out <- strip_field_names(fields)
  detect <- stringr::str_detect(".", out)


  expect_equal(length(out), length(fields))
  expect_false(sum(detect) > 0)
})

test_that("format_field_list", {

  fields <- c("participant.eid", "participant.p31", "participant.p41202")
  out <- format_field_list(fields)
  expect_true(is.character(out))
  expect_true(length(out)==1)
})
