#' Installs the dx-toolkit into a virtualenv
#'
#' Not needed for running in JupyterLab/RStudio on UKB RAP or JupyterLab on
#' core platform, as the dx-toolkit is installed on these apps.
#'
#' Useful for using xvhelper on your own machine.
#'
#' @return no-value. Side effect is that dxpy and pandas are installed
#' into reticulate environment.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' install_dxpy()
#' }
install_dxpy <- function(method="auto", conda="auto") {
  #env_name <- env_name()

#  if(!reticulate::virtualenv_exists(env_name)){
#    reticulate::virtualenv_create(env_name)
#  }

  reticulate::py_install(packages=c("dxpy", "pandas"), method=method, auto=auto)
  #reticulate::virtualenv_install(env_name, packages = c("dxpy", "pandas"))
  cli::cli_alert_success("dxpy is now installed")
  #cli::cli_alert_success("use reticulate::use_virtualenv('{env_name}') to use")

}

env_name <- function(){
  return("dxpy_env")
}

format_field_list <- function(field_list){

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
#'
#' \dontrun{
#' fields <- c("participant.eid", "participant.p31", "participant.p41202")
#' ds_id <- get_dataset_id()
#' extract_data(ds_id, field_list=fields)
#' }
extract_data <- function(dataset_id, field_list) {
  env_name <- check_env()

  ds_name <- get_name_from_full_id(dataset_id)
  file_out <- paste0(ds_name, ".data.csv")

  cmd <- "dx"

  field_list_format <- format_field_list(field_list)

  args <- c("extract_dataset", glue::glue("{dataset_id}"),
            "--fields", glue::glue("{field_list_format}"),
            "-o", file_out)

  cli::cli_alert("running dx extract_dataset {dataset_id} --fields {field_list_format} -o {file_out}")
  sys::exec_wait(cmd=cmd, args=args)
  curr_dir <- getwd()
  cli::cli_alert_success("data is now extracted to {curr_dir}/{file_out}")

}

check_env <- function(){
  if(!reticulate::py_module_available("dxpy")){
    cli::cli_abort("install dxpy using install_dxpy() first")
  }
  dxpy <- reticulate::import("dxpy")
  return(dxpy)
}

#' Finds the current dataset id in the project
#'
#' @return - dataset id in the form project-xxxx:record-yyyy
#' @export
#'
#' @examples
#' \dontrun{
#' find_dataset_id()
#' }
find_dataset_id <- function(){
  dxpy <- check_env()

  dispensed_dataset <- dxpy$find_one_data_object(
    typename='Dataset',
    folder='/',
    describe=TRUE)

  project <- dispensed_dataset$describe$project
  id <- dispensed_dataset$describe$id

  ds_id <- glue::glue("{project}:{id}")

  return(ds_id)
}


#' Find all datasets in the current project
#'
#' Finds all datasets in current project.
#'
#' @return data.frame of all datasets in the project
#' @export
#'
#' @examples
#'
#' \dontrun{
#' find_all_datasets()
#' }
find_all_datasets <- function() {
  dxpy <- check_env()

  iter_py <- dxpy$find_data_objects(
    typename = "Dataset",
    folder="/",name_mode="glob",
    recurse = TRUE,
    describe = TRUE)

  extract_fun <- function(x){desc_obj <- x$describe
      out_frame <- data.frame(id=desc_obj$id,
                          name=desc_obj$name,
                          project=desc_obj$project)
      out_frame}

  out_list <- reticulate::iterate(iter_py, extract_fun)

  out_frame <- purrr::reduce(out_list, rbind)

  out_frame |>
    dplyr::mutate(project_record = glue::glue("{project}:{id}"))

  out_frame <- out_frame |>
    dplyr::arrange(desc(name))

  return(out_frame)
}

find_linked_dataset <- function(cohort_id){

  dxpy <- check_env()
  if(stringr::str_detect(cohort_id, ":")){
    cohort_id <- strsplit(cohort_id, ":")[[1]][2]
  }

  out <- dxpy$describe(obj_id)
  links <- out$links
  obj_id <- links[stringr::str_detect(links, "record")]
  proj_id <- out$project

  ds_id <- glue::glue("{proj_id}:{obj_id}")
  return(ds_id)
}

#' Finds all cohorts and their ids in the current project
#'
#' @return `data.frame` listing all cohorts in the project.
#' Cohort ID, name, project_id, project_record ID, and the
#' linked dataset ID is returned
#' @export
#'
#' @examples
#'
#' \dontrun{
#' find_all_cohorts()
#' }
find_all_cohorts <- function(){
  dxpy <- check_env()

  iter_py <- dxpy$find_data_objects(
    typename = "DatabaseQuery",
    folder="/",name_mode="glob", recurse = TRUE,
    describe = TRUE)

  extract_fun <- function(x){desc_obj <- x$describe
  out_frame <- data.frame(id=desc_obj$id,
                          name=desc_obj$name,
                          project=desc_obj$project)
  out_frame}

  out_list <- reticulate::iterate(iter_py, extract_fun)
  out_frame <- purrr::reduce(out_list, rbind)
  out_frame <- out_frame |>
    dplyr::arrange(desc(name)) |>
    dplyr::mutate(project_record = glue::glue("{project}:{id}")) #|>
    #dplyr::mutate(dataset = find_linked_dataset(project_record))

  out_frame

}

#' List fields associated with dataset ID
#'
#' Given a dataset ID generated from get_dataset_id(), runs `dx extract_dataset`
#' with the `--list-fields` option.
#'
#' @param dataset_id - ID of the dataset, in `project-XXXX:record-YYYY` format
#'
#' @return data.frame of all field IDs associated with the dataset
#'
#' @examples
#'
#' \dontrun{
#' ds_id <- get_dataset_id()
#' field_frame <- list_fields(ds_id)
#' }
list_fields <- function(dataset_id=NULL) {
  dxpy <- check_env()

  if(is.null(dataset_id)){
    dataset_id = find_dataset_id()
  }

  tmp <- tempfile()

  cmd <- "dx"
  cmdargs <-c(
            "extract_dataset",
              glue::glue("{dataset_id}"),
            "--list-fields"
            )

  cli::cli_alert("Retrieving Fields")
  cli::cli_alert("running `dx extract_dataset {dataset_id} --list-fields`")

  sys::exec_wait(cmd, args = cmdargs, std_out = tmp)

  out_fields <- readr::read_delim(tmp, col_names = FALSE)
  colnames(out_fields) <- c("field_id", "field_title")
  out_fields
}

get_name_from_full_id <- function(id){
  dxpy <- check_env()

  if(stringr::str_detect(id, ":")){
    id <- strsplit(id, ":")[[1]][2]
  }

  ds_name <- dxpy$describe(id)
  ds_name$name
}

#' Given a dataset id, builds a coding table to decode raw data
#'
#' @param ds_id - dataset id, with the format `project-XXXXX:record-YYYY`
#' @param path - path in current project
#'
#' @return data.frame of codings merged with data dictionary for dataset/
#' cohort
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ds_id <- find_dataset_id()
#' get_dictionaries(ds_id)
#' codings <- get_coding_table(ds_id)
#' head(codings)
#' }
get_coding_table <- function(ds_id, path="."){
  app_name <- get_name_from_full_id(ds_id)

  if(length(app_name)==0){
    cli_abort("Check whether {ds_id} is a dataset")
  }

  file_list <- list.files(path = path,pattern = app_name, recursive = TRUE)

  if(length(file_list) == 0){
    cli::cli_abort("No data/coding/entity dictionaries named {app_name}")
  }

  data_dict_path <- list.files(path=path,
                               pattern=glue::glue("{app_name}.data_dictionary.csv"),
                               recursive = TRUE, full.names = TRUE)
  coding_dict_path <- list.files(path=path,
                                 pattern=glue::glue("{app_name}.codings.csv"),
                                 recursive = TRUE, full.names = TRUE)

  dd <- readr::read_csv(data_dict_path, show_col_types = FALSE)
  cd <- readr::read_csv(coding_dict_path, show_col_types = FALSE)

  cd_dict <- merge_coding_data_dict(cd, dd)

  cd_dict
}

#' Extracts the dictionary files for a dataset id into current project
#'
#' @param dataset_id
#'
#' @return side effect is downloading the `data.dictionary.csv`, `codings.csv`,
#' and `entity.csv` file for the dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' ds_id <- get_dataset_id()
#' get_dictionaries(ds_id)
#' }
get_dictionaries <- function(dataset_id=NULL){
  dxpy <- check_env()
  if(is.null(dataset_id)){
    dataset_id = find_dataset_id()
  }
  ds_name <- get_name_from_full_id(dataset_id)

  tmp <- tempfile()

  cmd <- "dx"
  cmdargs <-c("extract_dataset",
              glue::glue("{dataset_id}"),
              "--dump-dataset-dictionary")

  cli::cli_alert("running dx extract_dataset {dataset_id} --dump-dataset-dictionary")
  sys::exec_wait(cmd, args = cmdargs, std_out = tmp)

  curr_dir <- getwd()

  cli::cli_alert_success("Data dictionary is downloaded as {curr_dir}/{ds_name}.dataset.data_dictionary.csv")
  cli::cli_alert_success("Coding dictionary is downloaded as {curr_dir}/{ds_name}.dataset.codings.csv")
  cli::cli_alert_success("Entity dictionary is downloaded as {curr_dir}/{ds_name}.entity_dictionary.csv")
}
