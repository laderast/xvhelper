get_project_id <- function() {
  system("dx env | ")

}

get_field_list <- function(dataset_id) {
  tmp <- tempdir()
  cmd <- glue::glue("dx extract_data --ddd {dataset_id} ")
  system(cmd)
  dict <- read.csv(tmp, pattern=(".data"))
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
    select(title, entity, name, coding_name, units) |>
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
    column_name <- as.character(column_name)
  }

  if(!is.character(column_name) & !in_fields) {
    column_name <- paste0(column_name, ".0")
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
                                 ~(.x |> dplyr::rename({{column_name}}:=code, ent_field=col)))
  multi_column <- purrr::map(multi_code_frame,
                             ~(map_categories(column_name=column_name,.x, coded_col_df)))

  decoded <- unlist(purrr::map(multi_column,
                               ~(paste(.x, collapse='","'))))

  decoded <- data.frame(decoded) |> dplyr::rename({{column_name}}:=decoded)

  return(decoded)
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
                                   dplyr::filter(ent_field==.x) |>
                                   dplyr::select(ent_field,is_sparse_coding, is_multi_select, coding_name) |>
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
  out_process <- map_columns(to_process, df, coded_col_df)
  out_frame <- Reduce(dplyr::bind_cols, out_process$data_col)
  colnames(out_frame) <- out_process$ent_field
  return(out_frame)
}


# Decoding function
#' Title
#'
#' @param code
#' @param curr_col
#'
#' @return
#' @export
#'
#' @examples
decode_fun <- function(code, curr_col) {
  ifelse(
    !is.na(code),
    ifelse(
      is.na(coded_col_df %>% dplyr::filter(ent_field == curr_col) %>% distinct(is_multi_select) %>% pull(is_multi_select)),
      decode_single(code, coded_col_df, curr_col),
      decode_multi_select(code, coded_col_df, curr_col)
    ),
    NA
  )
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

# Function for dropping single sparsely coded values
decode_single_sparse <- function(single_code, coded_col_df, curr_col) {
  single_code <- ifelse(
    typeof(single_code) != "character",
    ifelse(
      as.character(single_code) %in% (coded_col_df %>% filter(ent_field == curr_col) %>% pull(code)),
      as.character(single_code),
      paste(as.character(single_code), ".0", sep = "")
    ),
    single_code
  )
  ifelse(single_code %in% (coded_col_df %>% filter(ent_field == curr_col) %>% pull(code)),
         NA,
         single_code
  )
}

# Function for dropping multi select sparsely coded values
#' Title
#'
#' @param multi_code
#' @param coded_col_df
#' @param curr_col
#'
#' @return
#' @export
#'
#' @examples
decode_multi_sparse <- function(multi_code, coded_col_df, curr_col) {
  multi_code_substr <- str_replace_all(multi_code, '\\[|\\]|\\"', "")
  multi_code_vector <- unlist(strsplit(multi_code_substr, ','))
  decoded <- c()
  for (code in multi_code_vector) {
    meaning <- decode_single_sparse(code, coded_col_df, curr_col)
    decoded <- append(decoded, meaning)
  }
  decode_list <- paste(decoded, collapse='","')
  paste0('["', decode_list, '"]')
}

# Function for dropping sparsely coded values
#' Title
#'
#' @param code
#' @param curr_col
#'
#' @return
#' @export
#'
#' @examples
decode_fun_sparse <- function(code, curr_col) {
  ifelse(
    !is.na(code),
    ifelse(
      !is.na(coded_col_df %>% filter(ent_field == curr_col) %>% distinct(is_sparse_coding) %>% pull(is_sparse_coding)),
      ifelse(
        is.na(coded_col_df %>% filter(ent_field == curr_col) %>% distinct(is_multi_select) %>% pull(is_multi_select)),
        decode_single_sparse(code, coded_col_df, curr_col),
        decode_multi_sparse(code, coded_col_df, curr_col)
      ),
      toString(code)
    ),
    NA
  )
}
