#' Installs the dx-toolkit into a virtualenv
#'
#' @return no-value
#' @export
#'
#' @examples
install_dxpy <- function() {
  env_name <- env_name()

  if(!reticulate::virtualenv_exists(env_name)){
    reticulate::virtualenv_create(env_name)
  }

  reticulate::virtualenv_install(env_name, packages = c("dxpy", "pandas"))
  cli::cli_alert_success("dxpy environment is now installed and activated")
  cli::cli_alert_success("use reticulate::use_virtualenv({env_name}) to use")

}

env_name <- function(){
  return("dxpy_env")
}

format_field_list <- function(field_list, return_eids=TRUE){

  if(return_eids){
    field_list = c("participant.eid", field_list)
  }

  return(paste(field_list, collapse = ","))
}

#' Given a dataset id and list of fields, extract data
#'
#' @param dataset_id - dataset id in the form project-xxxx:record-yyyy- you can use get_dataset_id() to find this.
#' @param field_list - vector/list of fields. Note that you can choose to return eids below
#' @param return_eids - Whether to return eids
#'
#' @return
#' @export
#'
#' @examples
extract_data <- function(dataset_id, field_list, return_eids=TRUE) {
  env_name <- check_env()

  ds_name <- get_name_from_full_id(dataset_id)
  file_out <- paste0(ds_name, ".data.csv")

  cmd <- "dx"

  field_list_format <- format_field_list(field_list, return_eids)

  args <- c("extract_dataset", glue::glue("{dataset_id}"),
            "--fields", glue::glue("{field_list_format}"),
            "-o", file_out)

  sys::exec_wait(cmd=cmd, args=args)
  cli::cli_alert_success("data is now extracted to {file_out}")

}



check_env <- function(){
  env_name <- env_name()

  if(!reticulate::virtualenv_exists(env_name)){
    cli::cli_abort("install dxpy using install_dxpy() first")
  }
  reticulate::use_virtualenv(env_name)

  return(env_name)
}

#' Finds the current dataset id in the project
#'
#' @return - dataset id in the form project-xxxx:record-yyyy
#' @export
#'
#' @examples
find_dataset_id <- function(){
  env_name <- check_env()

  dxpy <- reticulate::import("dxpy")

  dispensed_dataset <- dxpy$find_one_data_object(
    typename='Dataset',
    name='app*.dataset',
    folder='/',
    name_mode='glob',
    describe=TRUE)

  project <- dispensed_dataset$describe$project
  id <- dispensed_dataset$describe$id

  ds_id <- glue::glue("{project}:{id}")

  return(ds_id)
}

find_dataset_name <- function(){
  env_name <- check_env()

  dxpy <- reticulate::import("dxpy")

  dispensed_database <- dxpy$find_one_data_object(
    classname="database",
    name='app*',
    folder='/',
    name_mode='glob',
    describe=TRUE)

  ds_name <- dispensed_database$describe$name

  return(ds_name)
}

#' Find all datasets in the current project
#'
#' @return data.frame of all datasets in the project
#' @export
#'
#' @examples
find_all_datasets <- function() {
  iter_py <- dxpy$find_data_objects(
    typename = "Dataset",
    name="app*.dataset",
    folder="/",name_mode="glob",
    describe = TRUE)

  extract_fun <- function(x){desc_obj <- x$describe
      out_frame <- data.frame(id=desc_obj$id,
                          name=desc_obj$name,
                          project=desc_obj$project)
      out_frame}

  out_list <- iterate(iter_py, extract_fun)

  out_frame <- purrr::reduce(out_list, rbind)

  out_frame <- out_frame |>
    dplyr::arrange(desc(name))

  return(out_frame)
}

find_all_cohorts <- function(){

  iter_py <- dxpy$find_data_objects(
    typename = "DatabaseQuery",
    folder="/",name_mode="glob",recurse = TRUE,
    describe = TRUE)

  extract_fun <- function(x){desc_obj <- x$describe
  out_frame <- data.frame(id=desc_obj$id,
                          name=desc_obj$name,
                          project=desc_obj$project)
  out_frame}

  out_list <- iterate(iter_py, extract_fun)
  out_frame <- purrr::reduce(out_list, rbind)
  out_frame <- out_frame |>
    dplyr::arrange(desc(name))

  out_frame

}

list_fields <- function(dataset_id=NULL) {
  env_name <- check_env()
  if(is.null(dataset_id)){
    dataset_id = find_dataset_id()
  }

  tmp <- tempfile()

  cmd <- "dx"
  cmdargs <-c("extract_dataset",
              glue::glue("{dataset_id}"),
              "--list-fields")

  cli::cli_alert("Retrieving Fields")

  sys::exec_wait(cmd, args = cmdargs, std_out = tmp)

  out_fields <- readr::read_delim(tmp)
  out_fields
}

get_name_from_full_id <- function(id){
  obj_id <- strsplit(id, ":")[[1]][2]
  ds_name <- dxpy$describe(obj_id)
  ds_name
}

get_dictionaries <- function(dataset_id=NULL){
  env_name <- check_env()
  if(is.null(dataset_id)){
    dataset_id = find_dataset_id()
  }
  ds_name <- get_name_from_full_id(dataset_id)

  tmp <- tempfile()

  cmd <- "dx"
  cmdargs <-c("extract_dataset",
              glue::glue("{dataset_id}"),
              "--dump-dataset-dictionary")

  sys::exec_wait(cmd, args = cmdargs, std_out = tmp)

  cli::cli_alert_success("Data dictionary is downloaded as {ds_name}.dataset.data_dictionary.csv")
  cli::cli_alert_success("Coding dictionary is downloaded as {ds_name}.dataset.codings.csv")
  cli::cli_alert_success("Entity dictionary is downloaded as {ds_name}.entity_dictionary.csv")
}
