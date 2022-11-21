get_field_list <- function(dataset_id) {
  tmp <- tempdir()
  cmd <- glue::glue("dx extract_data --ddd {dataset_id} -o {tmp}/data_files")
  system(cmd)
  dict <- readr::read_csv(tmp, pattern=".data_dictionary.csv")
  coding <-readr::read_csv(tmp, pattern=".codings.csv")
  return(dict)

}

build_data_dictionary <- function(dataset_id) {

}

#' Given a data dictionary, displays a searachable table in a quarto document or Jupyter Notebook
#'
#' @param dict
#'
#' @return none - a reactable table will be created in the document, with a searchable window.
#' @export
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' search_field_list(cdata)
#'
search_field_list <- function(dict) {
  dict |>
    dplyr::select(title, entity, name, coding_name, units) |>
    reactable::reactable()
}



#' Returns cleaned field titles for a cohort dataset
#'
#' @param out_frame
#' @param coded_col_df
#'
#' @return
#' @export
#'
#' @examples
#'
#' #' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' cohort |>
#'   decode_column_names(cdata)
decode_column_names <- function(cohort, coded_col_df) {

  coded_col_df <- coded_col_df |>
    dplyr::select(ent_field, title) |>
    dplyr::distinct() |>
    dplyr::filter(ent_field %in% colnames(cohort))

  names_to_replace <- tibble::tibble(orig_names =colnames(cohort))
  replacements <- names_to_replace |>
    dplyr::left_join(y=coded_col_df, by=c("orig_names"="ent_field")) |>
    dplyr::pull(title) |>
    janitor::make_clean_names()

  colnames(cohort) <- replacements

  cohort

}




#' Title
#'
#' @param coding_dict
#' @param data_dict
#'
#' @return
#' @export
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#' cdata
merge_coding_data_dict <- function(coding_dict, data_dict) {

  coded_col_df <-
    data_dict |>
      dplyr::mutate(ent_field=glue::glue("{entity}.{name}")) |>
      dplyr::left_join(y=coding_dict, by="coding_name") |>
      dplyr::select(title, ent_field, entity, name, coding_name,
             code, meaning, is_sparse_coding,
             is_multi_select)

  return(coded_col_df)

}

#' Decodes variables with single coded entries
#'
#' @param cohort - A cohort or dataset extracted using `dx extract data`
#' @param coding - Combined coding/data dictionary generated from `merge_coding_data_dict`
#'
#' @return data.frame with single coded columns decoded
#' @export
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' cohort |>
#'   decode_single(cdata)
decode_single <- function(cohort, coding){
  coding_table <- build_coding_table(coding)

  not_multi <- coding_table |>
    dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::filter(is.na(is_multi_sparse)) |>
    dplyr::filter(!is.na(coding_name)) |>
    dplyr::pull(ent_field)

  sparse_columns <- cohort |> dplyr::select(-any_of(not_multi))

  out <- cohort |>
    dplyr::select(any_of(not_multi)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(rown=dplyr::row_number()) |>
    tidyr::pivot_longer(-rown, names_to = "name", values_to = "code") |>
    dplyr::left_join(y=coding, by = c("name"="ent_field", "code"="code")) |>
    dplyr::select(rown, name, meaning) |>
    tidyr::pivot_wider(id_cols = rown, names_from = "name", values_from = "meaning") |>
    dplyr::select(-rown)

  out <- out[,not_multi]

  sparse_cols <- coding_table |>
    dplyr::filter(is_sparse_coding == "yes" & is.na(coding_name)) |> dplyr::pull(ent_field)

  originals <- cohort[, sparse_cols]

  cohort[,not_multi] <- out

  sparsed <- cohort[, sparse_cols]

  cohort[, sparse_cols] <-  purrr::map2(sparsed, originals, ~(dplyr::coalesce(as.character(.x), as.character(.y))))

  cohort
}



#' Decodes Multi Category variables
#'
#' @param cohort
#' @param coding
#'
#' @return Labeled data frame where multi category columns are decoded
#' @export
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' cohort |>
#'   decode_multi(cdata)
#'
decode_multi <- function(cohort, coding){
  coding_table <- build_coding_table(coding)

  multi_columns <- coding_table |>
    dplyr::filter(is_multi_select == "yes") |>
    dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::pull(ent_field)

  multi_cols <- cohort |>
    dplyr::select(any_of(multi_columns))

  multi_cols <- multi_cols |>
    #dplyr::select({{multi_column}}) |>
    dplyr::mutate(rown=dplyr::row_number()) |>
    tidyr::pivot_longer(-rown, names_to="col", values_to="code") |>
    dplyr::mutate(code :=
                    stringr::str_replace_all(code, '\\[|\\]|\\"', "")) |>
    tidyr::separate_rows(code, sep = ",") |>
    dplyr::left_join(y=coding, by=c("code"="code", "col"="ent_field")) |>
    dplyr::select(rown, col, meaning) |>
    dplyr::group_by(rown, col) |>
    dplyr::summarize(code := paste(meaning, collapse=", ")) |>
    tidyr::pivot_wider(id_cols = "rown", names_from = "col", values_from = "code") |>
    dplyr::ungroup() |>
    dplyr::select(-rown)

  multi_cols <- multi_cols[, multi_columns]

  cohort[,multi_columns] <- multi_cols

  #cohort[,multi_columns] <- purrr::map_df(multi_cols, ~(decode_multi_column(.x, coding)))

  cohort
}

build_coding_table <- function(coding){

  coding |>
    dplyr::select(ent_field, coding_name, is_multi_select, is_sparse_coding) |>
    dplyr::distinct() |>
    dplyr::mutate(is_multi_sparse=dplyr::coalesce(is_multi_select, is_sparse_coding))
}

#' Main Function to Decode Fields from Codings
#'
#' @param df
#' @param coding
#'
#' @return
#' @export
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' cohort |>
#'   decode_df(cdata)
#'
decode_df <- function(df, coding){
  df |>
    decode_single(coding) |>
    decode_multi(coding)
}
