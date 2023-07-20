build_data_dictionary <- function(dataset_id, project_id) {

  system(glue::glue("dx select {project_id}"))

  tmp <- tempdir()
  cmd <- glue::glue("dx extract_dataset {dataset_id} -ddd -o {tmp}/")
  print(cmd)
  system(cmd)

  dict <- readr::read_csv(tmp, pattern=".data_dictionary.csv")
  coding <-readr::read_csv(tmp, pattern=".codings.csv")

  coding_df <- xvhelper::merge_coding_data_dict(coding, dict)

  return(coding_df)

}

retrieve_raw_data <- function(dataset_id, field_list){

}

register_api_key <- function(api_key) {

  template <- "dx login {api_key}"
  system(glue::glue(template))
}

