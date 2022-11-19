get_field_list <- function(dataset_id) {
  tmp <- tempdir()
  cmd <- glue::glue("dx extract_data --ddd {dataset_id} ")
  system(cmd)
  dict <- read.csv(tmp, pattern=(".data"))
  #coding <-
  return(dict)

}

#' Title
#'
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
search_field_list <- function(dict) {
  dict |>
    dplyr::select(title, entity, name, coding_name, units) |>
    reactable::reactable()
}



#' Title
#'
#' @param column_name
#' @param df_to_convert
#' @param coded_col_df
#'
#' @return
#' @export
#'
#' @examples
map_categories <- function(column_name, df_to_convert, coded_col_df){
  code_df <- coded_col_df |> dplyr::filter(ent_field == column_name)

  if(nrow(code_df)==0){return(NULL)}

  codes <- code_df |> dplyr::pull(code)

  in_fields <- as.character(column_name) %in% codes



  if(!is.character(column_name) & in_fields) {
    df[[column_name]] <- as.character(df[[column_name]])

    #column_name <- as.character(column_name)
  }

  if(!is.character(column_name) & !in_fields) {
    df[[column_name]] <- paste0(df[[column_name]], ".0")
    #column_name <- paste0(column_name, ".0")
  }

  df_to_convert |> dplyr::select(code=any_of(column_name)) |>
    dplyr::mutate(code=as.character(code)) |>
    dplyr::left_join(code_df, by=c("code")) |>
    dplyr::select(code, meaning) |>
    dplyr::mutate(meaning = dplyr::coalesce(meaning,code)) |>
    dplyr::select(meaning) |>
    dplyr::rename({{column_name}}:=meaning)

}

#' Title
#'
#' @param column_name
#' @param df_to_convert
#' @param coded_col_df
#'
#' @return
#' @export
#'
#' @examples
map_categories_multi_column <- function(column_name, df_to_convert, coded_col_df){

  code_df <- coded_col_df |> dplyr::filter(ent_field == column_name)

  if(nrow(code_df)==0){return(NULL)}

  multi_code <- df_to_convert[[column_name]]

  if(is.numeric(multi_code)) {
    out_frame <- data.frame(multi_code) |>
      dplyr::rename({{column_name}} := multi_code)

    return(out_frame)
  }
  #multi_code <-
  multi_code_substr <- stringr::str_replace_all(multi_code, '\\[|\\]|\\"', "")
  multi_code_vector <- purrr::map(multi_code_substr, ~(stringr::str_split(.x, ",")))
  multi_code_frame <- purrr::map(multi_code_vector,
                                 ~(tibble::tibble(code=.x[[1]]) |> dplyr::mutate(col=column_name)))
  multi_code_frame <- purrr::map(multi_code_frame,
                                 ~(.x |> dplyr::rename({{column_name}}:=code, ent_field=col) ))

  multi_code_frame <- purrr::map(1:length(multi_code_frame), ~(multi_code_frame[[.x]] |> dplyr::mutate(id=.x)))

  transposed_frame <- Reduce(dplyr::bind_rows, multi_code_frame)

  decoded_frame <-
    transposed_frame |> dplyr::select(code=any_of(column_name), id=id) |>
      dplyr::mutate(code=as.character(code)) |>
      dplyr::left_join(code_df, by=c("code")) |>
      dplyr::select(code, meaning, id) |>
      dplyr::mutate(meaning = dplyr::coalesce(meaning,code)) |>
      dplyr::group_by(id) |>
      #dplyr::summarise(meaning = list(meaning)) |>
      dplyr::summarise(meaning = paste(meaning, collapse='","')) |>
      dplyr::mutate(meaning = dplyr::case_when(meaning == "NA" ~ NA_character_,
                                               TRUE ~  paste0('"', meaning, '"'))
                    ) |>
      dplyr::select(meaning)

  #multi_column <- map_categories(column_name=column_name,transposed_frame, coded_col_df)

  decoded_frame <- data.frame(decoded_frame) |> dplyr::rename({{column_name}}:=meaning)

  return(decoded_frame)
}

#' Title
#'
#' @param df
#' @param coded_col_df
#'
#' @return
#'
#' @examples
map_data_frame <- function(df, coded_col_df, drop_sparse){

    colstatus <- purrr::map_df(colnames(df),
                               ~(coded_col_df |>
                                   dplyr::select(ent_field,is_sparse_coding, is_multi_select, coding_name) |>
                                   dplyr::filter(ent_field==.x) |>
                                   dplyr::distinct()
                                 )
                               )
    colstatus <- colstatus |>
      dplyr::mutate(single = dplyr::case_when(is.na(is_multi_select) ~ TRUE,
                                      is.na(coding_name) ~ NA,
                                       TRUE ~ FALSE))

    if(drop_sparse){
      colstatus <- colstatus |>
        dplyr::filter(is.na(is_sparse_coding))
    }


    return(colstatus)

}

map_columns <- function(colstatus, df, coded_col_df){

  results <- colstatus |>
    dplyr::rowwise() |>
    dplyr::mutate(data_col =
                    dplyr::case_when(is.na(coding_name) ~ list(df[[ent_field]]),
                                     single == TRUE ~ list(map_categories(column_name = ent_field,df, coded_col_df)),
                                     single == FALSE ~ list(map_categories_multi_column(column_name = ent_field, df, coded_col_df)),

                    ))
  return(results)
}

#' Title
#'
#' @param df
#' @param coded_col_df
#' @param drop_sparse
#'
#' @return
#' @export
#'
#' @examples
decode_categories <- function(df, coded_col_df, drop_sparse=TRUE){

  to_process <- map_data_frame(df, coded_col_df, drop_sparse)
  out_process <- suppressMessages(map_columns(to_process, df, coded_col_df))
#  names(out_process$data_col) <- to_process$ent_field
  out_frame <- Reduce(dplyr::bind_cols, out_process$data_col)
  colnames(out_frame) <- out_process$ent_field
  return(out_frame)
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
