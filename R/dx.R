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
#' @return Alert to dataset location
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

  out <- try(dxpy$describe(cohort_id))

  #                error=reticulate::py_last_error())

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

#' Finds all jobs and executions in current project
#'
#' @return - data.frame with the following columns:
#'  ``
#' @export
#'
#' @examples
find_all_jobs <- function(){
  dxpy <- check_env()
  proj_id <- dxpy$PROJECT_CONTEXT_ID

  extract_fun <- function(x){
    desc_obj <- x$describe
    #desc_obj

    out_file <- NA_character_

    if(!is.null(desc_obj$output)){
      out_file <- desc_obj$output$csv[[1]]$`$dnanexus_link`
      if(is.null(out_file)){
        out_file <- NA_character_
      }
    }

    out_frame <- data.frame(job_id=glue::glue("{desc_obj$project}:{desc_obj$id}"),
                            name=desc_obj$name, state=desc_obj$state,
                            app=desc_obj$executableName, output_file=out_file)

    out_frame

  }

  results <- dxpy$find_executions(describe=TRUE, project = proj_id)
  out_list <- reticulate::iterate(results, extract_fun)

  out_frame <- purrr::reduce(out_list, rbind)

  return(out_frame)
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

strip_field_names <- function(field_list){
  stringr::str_split_i(field_list, "\\.", 2)
}

strip_id <- function(ds_id){
  strsplit(ds_id, ":")[[1]][[2]]
}

strip_id_for_project <- function(ds_id){
  strsplit(ds_id, ":")[[1]][[1]]
}



#' Runs Table Exporter
#'
#' @param ds_id
#' @param field_list
#' @param ...
#'
#' @return job_id Job ID of launched job
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ds_id <- find_dataset_id()
#' fields <- fields <- c("participant.eid", "participant.p31", "participant.p41202")
#' job_id <- launch_table_exporter(ds_id, fields)
#'
#' # Watch our job
#' check_job(job_id)
#'
#' # Terminate our job if necessary
#' terminate_job(job_id)
#'
#' }
launch_table_exporter <- function(ds_id, field_list, entity="participant",...){
  dxpy = check_env()

  app_iter <- dxpy$find_apps("table-exporter")
  app_id <- reticulate::iter_next(app_iter)$id

  ds_bare_id <- strip_id(ds_id)
  project_id <- strip_id_for_project(ds_id)


  table_exporter <- dxpy$DXApp(app_id)

  if (is.null(ds_id)){
    #opt$dataset <- dataset
    cli::cli_abort("No dataset argument")
  }

  if (is.null(field_list)){
    cli::cli_abort("No list of fields")
  }

  field_list = strip_field_names(field_list)

  param_list <- list(
    dataset_or_cohort_or_dashboard=list(`$dnanexus_link`=list(project=project_id, id=ds_bare_id)),
    entity=entity,
    field_names=field_list,
    header_style = "FIELD-TITLE",
    coding_option = "REPLACE")

  job <- table_exporter$run(app_input = param_list)

  job_id <- job$get_id()

  cli::cli_alert("Job has been submitted as {job_id}")
  cli::cli_alert('Use  check_job("{job_id})") to monitor job')

  return(job_id)


}

#' Watches a job given a job id
#'
#' @param job_id - job id in the format of `job-XXXXXX`.
#'
#' @return file_id - If a CSV file was generated in the job, otherwise NULL. Prints alert
#' to screen of job status and job_id for file generated.
#' @export
#'
#' @examples
#' \dontrun{
#' ds_id <- find_dataset_id()
#' fields <- fields <- c("participant.eid", "participant.p31", "participant.p41202")
#' job_id <- launch_table_exporter(ds_id, fields)
#'
#' check_job(job_id)
#' }
#'
check_job <- function(job_id){
  dxpy = check_env()

  job <- try(dxpy$DXJob(dxid = job_id))
  desc <- job$describe()
  status <- desc$state

  cli::cli_alert_success("Job is currently {status}")

  file_id <- NULL

  if(!is.null(desc$output)){
    file_id <- desc$output$csv[[1]]$`$dnanexus_link`
    cli::cli_alert_success("File generated in project storage: {file_id}")
    cli::cli_alert_success("Use `system('dx download {file_id}')` to download to local storage)")
  }

  return(file_id)
}

#' Terminates a job using job id
#'
#' @param job_id
#'
#' @return Nothing - alert that job has been terminated.
#' @export
#'
#' @examples
terminate_job <- function(job_id) {
  dxpy = check_env()

  job = try(dxpy$DXJob(dxid = job_id))
  job$terminate()

  cli::cli_alert("Job {job_id} has been terminated")
}
