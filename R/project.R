

#' Given a data dictionary, displays a searachable table in a quarto document or Jupyter Notebook
#'
#' This function leverages reactable to give a searchable table of fields. It's most useful
#' in a Jupyter Notebook or RMarkdown/Quarto Document
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
explore_field_list <- function(dict) {
  dict |>
    dplyr::select(title, entity, name, coding_name, units) |>
    reactable::reactable(searchable = TRUE)
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
decode_column_names <- function(cohort, coded_col_df, r_clean_names=TRUE) {

  coded_col_df <- coded_col_df |>
    dplyr::select(ent_field, title) |>
    dplyr::distinct() |>
    dplyr::filter(ent_field %in% colnames(cohort))

  names_to_replace <- tibble::tibble(orig_names =colnames(cohort))
  replacements <- names_to_replace |>
    dplyr::left_join(y=coded_col_df, by=c("orig_names"="ent_field")) |>
    dplyr::mutate(new_title = dplyr::coalesce(new_title = title, orig_names)) |>
    dplyr::pull(new_title)

  if(r_clean_names){
    replacements <- replacements |>
      janitor::make_clean_names()
  }


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
#' headh(cdata)
merge_coding_data_dict <- function(coding_dict, data_dict) {

  coded_col_df <-
    data_dict |>
      dplyr::mutate(ent_field=glue::glue("{entity}.{name}")) |>
      dplyr::left_join(y=coding_dict, by="coding_name", relationship="many-to-many") |>
      dplyr::mutate(code = as.character(code)) |>
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
  coding_table <- build_coding_table(coding)  |>
    dplyr::filter(ent_field %in% colnames(cohort))

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


decode_single_db <- function(cohort, coding){
  coding_table <- build_coding_table(coding)  |>
    dplyr::filter(ent_field %in% colnames(cohort))

  coding <- coding |> dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::select(-name)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir="tablesduckdb", read_only=FALSE)
  duckdb::duckdb_register(con, "coding", coding)
  coding_db <- dplyr::tbl(con, "coding")


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
    tidyr::pivot_longer(-rown, names_to = "name", values_to = "code")

  duckdb::duckdb_register(con, "output", out)

  decoded <- dplyr::tbl(con, "output") |>
    dplyr::left_join(y=coding_db, by = c("name"="ent_field", "code"="code")) |>
    dplyr::select(rown, name, meaning) |> tibble::as_tibble()

  out <- decoded |>
    tidyr::pivot_wider(id_cols = rown, names_from = "name", values_from = "meaning") |>
    dplyr::select(-rown)

  out <- out[,not_multi]

  DBI::dbDisconnect(con, shutdown=TRUE)

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
  coding_table <- build_coding_table(coding) |>
    dplyr::filter(ent_field %in% colnames(cohort))

  col_class <- lapply(cohort, class)
  list_cols <- names(col_class[col_class == "list"])

  #if column is a list-column, paste values together as comma delimited string
  list_columns <- cohort |>
    dplyr::select(dplyr::any_of(list_cols)) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(list_cols), \(x) paste(x, collapse = ",")))

  list_columns <- list_columns[,list_cols]
  cohort[,list_cols] <- list_columns

  multi_columns <- coding_table |>
    dplyr::filter(is_multi_select == "yes") |>
    dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::pull(ent_field)

  multi_cols <- cohort |>
    dplyr::select(dplyr::any_of(multi_columns))

  multi_cols <- multi_cols |>
    #dplyr::select({{multi_column}}) |>
    dplyr::mutate(rown=dplyr::row_number()) |>
    tidyr::pivot_longer(-rown, names_to="col", values_to="code") |>
    dplyr::mutate(code :=
                    stringr::str_replace_all(code, '\\[|\\]|\\"', "")) |>
    #separate commaa delimited string in multicolumn to multiple rows
    tidyr::separate_rows(code, sep = ",") |>
    #join to coding file
    dplyr::left_join(y=coding, by=c("code"="code", "col"="ent_field")) |>
    dplyr::select(rown, col, meaning) |>
    dplyr::group_by(rown, col) |>
    dplyr::summarize(code := paste(meaning, collapse="|")) |>
    tidyr::pivot_wider(id_cols = "rown", names_from = "col", values_from = "code") |>
    dplyr::ungroup() |>
    dplyr::select(-rown)

  multi_cols <- multi_cols[, multi_columns]

  cohort[,multi_columns] <- multi_cols

  cohort
}


#' Decodes Multi Category variables
#'
#' For a smaller data frame, decodes the columns that have a list of values.
#'
#' For a larger data frame, such as from UK Biobank, we recommend you use
#' \seealso{decode_multi_large_df}, which will split your data frame into
#' smaller ones for decoding.
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
decode_multi_purrr <- function(cohort, coding){
  coding_table <- build_coding_table(coding) #|>
    #dplyr::filter(ent_field %in% colnames(cohort))

  col_class <- lapply(cohort, class)

  list_cols <- names(col_class[col_class == "list"])

  if(length(list_cols)==0){
    dl <- detect_list(cohort)
    list_cols <- names(dl[dl == TRUE])

  }

  if(length(list_cols)==0){
    stop("no list columns in data frame")
  }


  #sum(str_detect("\\["))

  if(length(list_cols) !=0) {
  #if column is a list-column, paste values together as comma delimited string
  list_columns <- cohort |>
    dplyr::select(dplyr::any_of(list_cols)) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(list_cols), \(x) paste(x, collapse=",")))

  list_columns <- list_columns[,list_cols]
  cohort[,list_cols] <- list_columns
  }

  multi_columns <- coding_table |>
    dplyr::filter(!is.na(is_multi_select)) |>
    dplyr::filter(is_multi_select == "yes" | is_multi_sparse == "yes") |>
    dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::pull(ent_field)

  multi_cols <- cohort |>
    dplyr::select(dplyr::any_of(multi_columns))

  multi_cols <- multi_cols |>
    #dplyr::select({{multi_column}}) |>
    dplyr::mutate(rown=dplyr::row_number()) |>
    #dplyr::mutate(across(-rown), as_character) |>
    tidyr::pivot_longer(-rown, names_to="col", values_to="code") |>
    dplyr::mutate(code :=
                    stringr::str_replace_all(code, '\\[|\\]|\\"', "")) |>
    #separate commaa delimited string in multicolumn to multiple rows
    tidyr::separate_rows(code, sep = ",")

  #multi_cols <- multi_cols |>
  #  tidyr::nest(col)

  test <- multi_cols |> dplyr::group_by(col) |> tidyr::nest(data=c(rown, code))
  test2 <- coding |> dplyr::select(ent_field, coding_name, code, meaning) |> dplyr::group_by(ent_field, coding_name) |> tidyr::nest(data=c(code, meaning))
  test3 <- dplyr::left_join(test, test2, by=c("col"="ent_field"))
  test4 <- purrr::map2(test3$data.x, test3$data.y, ~(dplyr::left_join(.x,.y, by=c("code"))))
  names(test4) <- multi_columns
  test5 <- purrr::map(test4, ~(.x |>
                                 dplyr::group_by(rown) |>
                                 dplyr::summarize(code := paste(meaning, collapse="|")) |>
                                 dplyr::ungroup() |>
                                 dplyr::select(-rown)
                               ))
  multi_cols <- as.data.frame(test5)
  colnames(multi_cols) <- multi_columns

  cohort[,multi_columns] <- multi_cols

  cohort
}


strip_quotes_and_brackets <- function(df, colname) {
  {{colname}} := stringr::str_replace_all({{colname}}, '\\[|\\]|\\"', "")
}


#' Detects lists specified as strings in a data frame
#'
#' Given data extracted from dx extract_data,
#'
#' @param df
#'
#' @return
#'
#' @examples
detect_list <- function(df){
  detect_list_column <- function(col){
  sum(stringr::str_detect(col[!is.na(col)], "\\[")) > 0
  }
  if(nrow(df) > 100){
    df <- df[1:100,]
  }

  lapply(df, detect_list_column)
}


#' Decodes Multi Category variables
#'
#' For a smaller data frame, decodes the columns that have a list of values.
#'
#' @param cohort
#' @param coding
#'
#' @return Labeled data frame where multi category columns are decoded
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
decode_multi_db <- function(cohort, coding){
  coding_table <- build_coding_table(coding) |>
    dplyr::filter(ent_field %in% colnames(cohort))

  coding <- coding |> dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::select(-name)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir="tablesduckdb", read_only=FALSE)
  duckdb::duckdb_register(con, "coding", coding)
  coding_db <- dplyr::tbl(con, "coding")

  col_class <- lapply(cohort, class)
  list_cols <- names(col_class[col_class == "list"])

  #if column is a list-column, paste values together as comma delimited string
  list_columns <- cohort |>
    dplyr::select(dplyr::any_of(list_cols)) |>
    dplyr::rowwise() |>
    #dplyr::mutate(dplyr::across(dplyr::any_of(list_cols), paste, collapse=","))
    dplyr::mutate(dplyr::across(dplyr::any_of(list_cols), \(x) paste(x, collapse=",")))

  list_columns <- list_columns[,list_cols]
  cohort[,list_cols] <- list_columns

  multi_columns <- coding_table |>
    dplyr::filter(is_multi_select == "yes") |>
    dplyr::filter(ent_field %in% colnames(cohort)) |>
    dplyr::pull(ent_field)

  multi_cols <- cohort |>
    dplyr::select(dplyr::any_of(multi_columns))

  multi_cols <- multi_cols |>
    #dplyr::select({{multi_column}}) |>
    dplyr::mutate(rown=dplyr::row_number()) |>
    tidyr::pivot_longer(-rown, names_to="col", values_to="code") |>
    dplyr::mutate(code :=
                    stringr::str_replace_all(code, '\\[|\\]|\\"', "")) |>
    #separate commaa delimited string in multicolumn to multiple rows
    tidyr::separate_rows(code, sep = ",")

  duckdb::duckdb_register(con, "multicol", multi_cols)
  multi_cols_db <- dplyr::tbl(con, "multicol")

  multi_cols <-
  multi_cols_db |>
    #join to coding file
    dplyr::left_join(y=coding_db, by=c("code"="code", "col"="ent_field")) |>
    dplyr::select(rown, col, meaning) |>
    tibble::as_tibble()

  multi_cols <-
    multi_cols |>
    dplyr::group_by(rown, col) |>
    dplyr::summarize(code := paste(meaning, collapse="|")) |>
    tidyr::pivot_wider(id_cols = "rown", names_from = "col", values_from = "code") |>
    dplyr::ungroup() |>
    dplyr::select(-rown)

  multi_cols <- multi_cols[, multi_columns]

  cohort[,multi_columns] <- multi_cols

  DBI::dbDisconnect(con, shutdown=TRUE)

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
    decode_multi_purrr(coding)
}


#' Main Function to Decode Fields from Codings using Duck DB
#'
#' @param df
#' @param coding
#'
#' @return
#'
#' @examples
#' data(coding_dict)
#' data(cohort)
#' data(data_dict)
#'
#' cdata <- merge_coding_data_dict(coding_dict, data_dict)
#'
#' cohort |>
#'   decode_df_db(cdata)
#'
decode_df_db <- function(df, coding){
  df |>
    decode_single(coding) |>
    decode_multi_purrr(coding)
}


split_df_by_index <- function(df, split_by=1000){
  list_size <- nrow(df) %/% split_by
  df_vec_end <- 1:list_size * split_by

  df_vec_end[list_size] = nrow(df)
  df_vec_start <- df_vec_end - (split_by-1)
  df_ind <- data.frame(ind=1:list_size, start=df_vec_start,
                       end = df_vec_end)
  df_ind

  out_list <- list()

  for(i in 1:nrow(df_ind)){
    row_vals <- df_ind[i,]
    out_list[[i]] <- df[row_vals$start:row_vals$end,]
  }

  out_list
}

process_multi_list <- function(out_list, codings){
  list_len <- length(out_list)

  id <- cli::cli_progress_bar("decoding multi columns", total = list_len)

  map_fun <- function(x, codings, id){
      cli::cli_progress_update(id=id)
      decode_multi_purrr(cohort = x, coding = codings)
  }

  out_list2 <- purrr::map(out_list, map_fun, codings, id)

  cli::cli_progress_done()

  out_list2
}


#' Decodes large columns of a large data frame by splitting it
#'
#' Given an extremely large data frame to decode (such as those of UK Biobank,
#' over 500K rows), split the data into a list of smaller data frames, decode
#' each smaller data frame, and then merge list into a larger data frame.
#'
#' This method is not necessarily faster than merging the entire data frame,
#' but it shows a progress bar.
#'
#' @param df - data.frame to process
#' @param coding - coding data frame
#' @param df_size - size of the smaller data frames in number of rows.
#'
#' @return decoded data frame in terms of columns
#' @export
#'
#' @examples
decode_multi_large_df <- function(df, coding, df_size=2000){
  out_list <- split_df_by_index(df, split_by = df_size)
  out_list2 <- process_multi_list(out_list, coding)
  out_frame <- purrr::reduce(out_list2, rbind)
  out_frame

}


.onLoad <- function(libname, pkgname) {
  reticulate::import("dxpy")
}

